use std::collections::HashMap;
use std::convert::Infallible;
use std::iter;
use std::ops::Deref;
use crate::opentype::post::parse_post;
use crate::{
    Encoder, Font, FontError, Glyph, GlyphId, HMetrics, IResultExt, Info, Name, Pen, R, Shape,
    VMetrics,
};
use crate::parsers::{iterator, parse};
use pdf_encoding::Encoding;
use nom::{
    number::complete::{be_u8, be_i8, be_i16, be_u16},
    bytes::complete::take,
    sequence::tuple
};
use pathfinder_content::outline::{Outline, Contour, ContourIterFlags};
use pathfinder_content::segment::SegmentKind;
use pathfinder_geometry::{vector::Vector2F, transform2d::Transform2F, rect::RectF};
use crate::opentype::{
    parse_tables, parse_head, parse_maxp, parse_loca,
    parse_hhea, parse_hmtx, parse_name, Hmtx, Tables,
    cmap::{CMap, parse_cmap},
    kern::{parse_kern},
    gpos::KernTable,
    os2::parse_os2,
};
use pathfinder_geometry::{transform2d::Matrix2x2F};
use itertools::Itertools;

/// Where one component of a composite glyph (OT §glyf, "Composite glyph description") sits.
#[derive(Copy, Clone, Debug)]
pub enum ComponentOffset {
    /// `ARGS_ARE_XY_VALUES` is set: the two arguments are an x/y offset.
    Vector(Vector2F),
    /// Otherwise the arguments are point numbers (§glyf): the component's point `child` is
    /// moved onto point `parent` of the outline built from the preceding components.
    MatchPoints { parent: u16, child: u16 },
}

/// One component of a composite glyph: glyph id, the 2×2 transform that applies to its
/// coordinates, and where the component is placed.
#[derive(Copy, Clone, Debug)]
pub struct Component {
    pub gid: u32,
    pub matrix: Matrix2x2F,
    pub offset: ComponentOffset,
    /// `SCALED_COMPONENT_OFFSET` (0x0800): the placement offset is part of the component's
    /// own design space and is scaled by `matrix`, as Apple's rasterizer does it. Without the
    /// flag — and without `UNSCALED_COMPONENT_OFFSET` (0x1000) too, which only exists to say
    /// so explicitly — the offset is in the composite's coordinates and applied after the
    /// matrix, the Microsoft default (MS §glyf, "Composite glyph description").
    pub scaled_offset: bool,
}

/// What a `glyf` record says about one glyph, before it is handed to the encoder.
///
/// This is the parse representation and it keeps outlines: a composite placed by point matching
/// needs the points of the components placed before it, which the encoder's own `Shape<E>`
/// cannot give back. `Shape<E>` is what the font finally exposes.
#[derive(Clone)]
pub enum GlyfShape {
    Simple(Outline),
    Compound(Vec<Component>),
    Empty
}

#[derive(Clone)]
pub struct TrueTypeFont<E: Encoder = crate::pathfinder_impl::OutlineEncoder> {
    glyphs: Vec<Glyph<E>>,
    pub cmap: Option<CMap>,
    encoding: Option<Encoding>,
    hmtx: Hmtx,
    num_glyphs: u32,
    units_per_em: u16,
    bbox: RectF,
    kern: KernTable,
    name: Name,
    info: Info,
    vmetrics: Option<VMetrics>,
    pub name_map: HashMap<String, u16>,
}

impl<E: Encoder> TrueTypeFont<E> {
    pub fn parse(data: &[u8], encoder: &mut E) -> Result<Self, FontError> {
        let tables = parse_tables(data)?;
        TrueTypeFont::parse_glyf(tables, encoder)
    }
    pub fn parse_glyf(tables: Tables<impl Deref<Target=[u8]>>, encoder: &mut E) -> Result<Self, FontError> {
        let head = parse_head(expect!(tables.get(b"head"), "no head"))?;
        let maxp = parse_maxp(expect!(tables.get(b"maxp"), "no maxp"))?;
        let loca = parse_loca(expect!(tables.get(b"loca"), "no loca"), &head, &maxp)?;
        let glyf = expect!(tables.get(b"glyf"), "no glyf");
        let hhea = parse_hhea(expect!(tables.get(b"hhea"), "no hhea"))?;
        let hmtx = parse_hmtx(expect!(tables.get(b"hmtx"), "no hmtx"), &hhea, &maxp)?;
        
        let glyphs = parse_glyphs(&loca, glyf, &hmtx, encoder)?;
        
        TrueTypeFont::from_shapes_and_metrics(tables, glyphs, hmtx)
    }
    pub fn from_shapes_and_metrics(tables: Tables<impl Deref<Target=[u8]>>, glyphs: Vec<Glyph<E>>, hmtx: Hmtx) -> Result<TrueTypeFont<E>, FontError> {
        let head = parse_head(expect!(tables.get(b"head"), "no head"))?;
        // OT §maxp: this is the authoritative glyph count; `loca` and `post` have to agree
        // with it, and it must not silently differ from what the glyphs contain.
        let maxp = parse_maxp(expect!(tables.get(b"maxp"), "no maxp"))?;
        if glyphs.len() != maxp.num_glyphs as usize {
            warn!("glyf/loca describe {} glyphs, maxp says {}", glyphs.len(), maxp.num_glyphs);
        }
        let (cmap, encoding) = match tables.get(b"cmap").map(|data| parse_cmap(data)).transpose()? {
            Some((cmap, encoding)) => (Some(cmap), Some(encoding)),
            None => (None, None)
        };
        let name = tables.get(b"name").map(|data| parse_name(data)).transpose()?.unwrap_or_default();
        let os2 = tables.get(b"OS/2").map(|data| parse_os2(data)).transpose()?;
        // OT §OS/2: with `fsSelection` bit 7 (USE_TYPO_METRICS) the `sTypo*` values are
        // the line metrics; without it applications lay out with `hhea`.
        let hhea = tables.get(b"hhea").map(parse_hhea).transpose()?;
        let vmetrics = os2.as_ref().and_then(|os2| os2.typo_metrics())
            .or_else(|| hhea.map(|hhea| hhea.into()));

        let post = t!(tables.get(b"post").map(parse_post).transpose());
        let mut name_map = HashMap::new();
        if let Some(post) = post {
            name_map.extend(post.names.into_iter().enumerate().map(|(i, name)| (name.into(), i as u16)));
        }
        
        Ok(TrueTypeFont {
            glyphs,
            cmap,
            encoding,
            hmtx,
            num_glyphs: maxp.num_glyphs as u32,
            units_per_em: head.units_per_em,
            bbox: head.bbox(),
            kern: tables.get(b"kern").map(|data| parse_kern(data)).transpose()?.unwrap_or_default(),
            name,
            info: Info {
                weight: os2.as_ref().map(|t| t.weight),
                x_height: os2.as_ref().and_then(|t| t.x_height).map(|height| height as f32),
                cap_height: os2.as_ref().and_then(|t| t.cap_height).map(|height| height as f32),
                unicode_ranges: os2.as_ref().map(|t| t.unicode_ranges).unwrap_or_default(),
            },
            vmetrics,
            name_map,
        })
    }
}

impl<E: Encoder + 'static> Font<E> for TrueTypeFont<E> {
    fn num_glyphs(&self) -> u32 {
        // OT §maxp, not `shapes.len()`: both `OpenTypeFont` and `TrueTypeFont` must answer
        // the same number for the same font.
        self.num_glyphs
    }
    fn font_matrix(&self) -> Transform2F {
        let scale = 1.0 / self.units_per_em as f32;
        Transform2F::from_scale(Vector2F::splat(scale.into()))
    }
    fn glyph(&self, id: GlyphId) -> Option<&Glyph<E>> {
        self.glyphs.get(id.0 as usize)
    }
    fn is_empty_glyph(&self, gid: GlyphId) -> bool {
        match self.glyphs.get(gid.0 as usize) {
            None => true,
            Some(Glyph { shape: Shape::Empty, .. }) => true,
            Some(Glyph { shape, .. }) => shape.is_empty(),
        }
    }
    fn gid_for_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        match self.cmap {
            Some(ref cmap) => cmap.get_codepoint(codepoint).map(GlyphId),
            None => None
        }
    }
    fn gid_for_unicode_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        match (self.cmap.as_ref(), self.encoding) {
            (Some(cmap), Some(Encoding::Unicode)) => cmap.get_codepoint(codepoint).map(GlyphId),
            (Some(cmap), _) => self.gid_for_codepoint(codepoint),
            _ => None
        }
    }
    fn encoding(&self) -> Option<Encoding> {
        self.encoding
    }
    fn bbox(&self) -> Option<RectF> {
        Some(self.bbox)
    }
    fn kerning(&self, left: GlyphId, right: GlyphId) -> f32 {
        self.kern.get(left.0 as u16, right.0 as u16).unwrap_or(0) as f32
    }
    fn name(&self) -> &Name {
        &self.name
    }
    fn info(&self) -> &Info {
        &self.info
    }
    fn vmetrics(&self) -> Option<VMetrics> {
        self.vmetrics
    }
}

#[inline]
fn vec_i8(i: &[u8]) -> R<Vector2F> {
    let (i, x) = be_i8(i)?;
    let (i, y) = be_i8(i)?;
    Ok((i, Vector2F::new(x as f32, y as f32)))
}
#[inline]
fn vec_i16(i: &[u8]) -> R<Vector2F> {
    let (i, x) = be_i16(i)?;
    let (i, y) = be_i16(i)?;
    Ok((i, Vector2F::new(x as f32, y as f32)))
}
#[inline]
fn fraction_i16(i: &[u8]) -> R<f32> {
    let (i, s) = be_i16(i)?;
    Ok((i, s as f32 / 16384.0))
}

pub fn parse_shapes(loca: &[u32], data: &[u8]) -> Result<Vec<GlyfShape>, FontError> {
    // OT §loca: the last offset is the length of `glyf`; anything larger points outside it.
    // Only the glyphs past the end are affected and they are read as empty below, so a font
    // that overshoots still renders the rest.
    if let Some(&end) = loca.last() {
        if end as usize > data.len() {
            warn!("loca: last offset {} is past glyf's {} bytes — those glyphs stay empty",
                end, data.len());
        }
    }
    let mut shapes = Vec::with_capacity(loca.len() - 1);
    for (i, (start, end)) in loca.iter().cloned().tuple_windows().enumerate() {
        // OT §glyf: one corrupt glyph record must not cost the whole font. `parse_glyph_shape`
        // is allowed to read past its slice, so a bad record is just an empty glyph here.
        let shape = match data.get(start as usize .. end as usize) {
            Some(slice) => match parse_glyph_shape(slice) {
                Ok(shape) => shape,
                Err(e) => {
                    warn!("glyph {}: {:?} — treated as an empty glyph", i, e);
                    GlyfShape::Empty
                }
            },
            None => {
                warn!("glyph {}: loca range {}..{} outside glyf — treated as an empty glyph", i, start, end);
                GlyfShape::Empty
            }
        };
        shapes.push(shape);
    }
    Ok(shapes)
}
// the following code is borrowed from stb-truetype and modified heavily

fn parse_glyph_shape(data: &[u8]) -> Result<GlyfShape, FontError> {
    if data.len() == 0 {
        return Ok(GlyfShape::Empty);
    }
    let (i, number_of_contours) = be_i16(data)?;
    
    let (i, _) = take(8usize)(i)?;
    //debug!("n_contours: {}", number_of_contours);
    match number_of_contours {
        0 => Ok(GlyfShape::Empty),
        n if n >= 0 => glyph_shape_positive_contours(i, number_of_contours as usize),
        -1 => compound(i).get(),
        n => error!("Contour format {} not supported.", n)
    }
}

pub fn compound(mut input: &[u8]) -> R<GlyfShape> {
    // Compound shapes
    let mut parts = Vec::new();
    loop {
        let (flags, gidx) = parse(&mut input, tuple((be_u16, be_u16)))?;
        // OT §glyf: with ARGS_ARE_XY_VALUES the two arguments are an x/y offset, otherwise they
        // are unsigned point numbers (one into the composite built so far, one into the
        // component) and the component is placed by matching them.
        let offset = if flags & 2 != 0 {
            if flags & 1 != 0 {
                // shorts
                ComponentOffset::Vector(parse(&mut input, vec_i16)?)
            } else {
                ComponentOffset::Vector(parse(&mut input, vec_i8)?)
            }
        } else if flags & 1 != 0 {
            let (parent, child) = parse(&mut input, tuple((be_u16, be_u16)))?;
            ComponentOffset::MatchPoints { parent, child }
        } else {
            let (parent, child) = parse(&mut input, tuple((be_u8, be_u8)))?;
            ComponentOffset::MatchPoints { parent: parent as u16, child: child as u16 }
        };
        let mut transform = Transform2F::default();
        if flags & (1 << 3) != 0 {
            // WE_HAVE_A_SCALE
            let scale = parse(&mut input, fraction_i16)?;
            transform.matrix = Matrix2x2F::from_scale(Vector2F::splat(scale));
        } else if flags & (1 << 6) != 0 {
            // WE_HAVE_AN_X_AND_YSCALE
            let (sx, sy) = parse(&mut input, tuple((fraction_i16, fraction_i16)))?;
            let s = Vector2F::new(sx, sy);
            transform.matrix = Matrix2x2F::from_scale(s);
        } else if flags & (1 << 7) != 0 {
            // WE_HAVE_A_TWO_BY_TWO
            let (a, b, c, d) = parse(&mut input, tuple((fraction_i16, fraction_i16, fraction_i16, fraction_i16)))?;
            transform.matrix = Matrix2x2F::row_major(a, b, c, d);
        }

        // Get indexed glyph.
        parts.push(Component {
            gid: gidx as u32,
            matrix: transform.matrix,
            offset,
            scaled_offset: flags & 0x0800 != 0,
        });
        // More components ?
        if flags & 0x20 == 0 {
            break;
        }
    }
    Ok((input, GlyfShape::Compound(parts)))
}

/// The points of a contour without the closing point that `contour()` appends, i.e. the
/// numbering OT §glyf uses for point matching.
///
/// `contour()` always ends a glyph contour at its start point — a closing line, a closing
/// quadratic, or the implied on-curve point between two off-curve points — so the last point of
/// every contour built there repeats the first, and dropping it restores the glyph's own
/// numbering exactly, including for a glyph whose last point genuinely repeats its first (the
/// fabricated point is still there, after it).
///
/// Two things still shift the numbering: an implied on-curve midpoint inserted between two
/// consecutive off-curve points, and quadratic-to-cubic conversion (the control points are then
/// stored as two cubic control points). Point matching in such a contour therefore refers to the
/// converted outline, not to `glyf` — fixing that needs the original numbering kept beside the
/// outline, which `pathfinder_content::outline::Contour` has no room for.
fn spec_points(contour: &Contour) -> &[Vector2F] {
    let points = contour.points();
    if points.len() > 1 && points[0] == points[points.len() - 1] {
        &points[.. points.len() - 1]
    } else {
        points
    }
}

/// The contours of a glyph as point lists, for point matching (§glyf point numbers).
type PointContours = Vec<Vec<Vector2F>>;

/// Point `index` of those contours, counting continuously over them.
fn point_at(contours: &[Vec<Vector2F>], index: u16) -> Option<Vector2F> {
    let mut n = index as usize;
    for contour in contours {
        if n < contour.len() {
            return Some(contour[n]);
        }
        n -= contour.len();
    }
    None
}

/// A composite glyph may nest; the spec also forbids a glyph that contains itself. Both are
/// bounded here so a malformed font cannot loop or overflow the stack.
const MAX_COMPONENT_DEPTH: usize = 16;

/// The depth cap alone is not enough: one level may hold many components, so a chain of
/// composites that each name two components still asks for `2 ^ 16` expanded outlines. The
/// expansion as a whole is therefore bounded too, counted over every level.
const MAX_COMPONENT_EXPANSIONS: usize = 10_000;

/// Counting expansions is not enough either: each one copies the outline it names, so one big
/// simple glyph named ten thousand times is still hundreds of millions of points. The points
/// copied for one glyph are capped as well (a real glyph has a few thousand).
const MAX_COMPONENT_POINTS: usize = 1_000_000;

/// What is left of `MAX_COMPONENT_EXPANSIONS` for the glyph being expanded.
struct ComponentBudget {
    left: usize,
    points_left: usize,
    reported: bool,
}

impl ComponentBudget {
    fn new() -> ComponentBudget {
        ComponentBudget { left: MAX_COMPONENT_EXPANSIONS, points_left: MAX_COMPONENT_POINTS, reported: false }
    }

    /// Whether one more component may be expanded; reports the truncation once.
    fn take(&mut self) -> bool {
        match self.left.checked_sub(1) {
            Some(left) => {
                self.left = left;
                true
            }
            None => {
                if !self.reported {
                    self.reported = true;
                    warn!("glyf: composite glyphs expand more than {} outlines; dropping the rest",
                          MAX_COMPONENT_EXPANSIONS);
                }
                false
            }
        }
    }
}

impl ComponentBudget {
    /// Whether an outline of `points` points may be copied; reports the truncation once.
    fn take_points(&mut self, points: usize) -> bool {
        match self.points_left.checked_sub(points) {
            Some(left) => {
                self.points_left = left;
                true
            }
            None => {
                if !self.reported {
                    self.reported = true;
                    warn!("glyf: composite glyphs copy more than {} points; dropping the rest",
                          MAX_COMPONENT_POINTS);
                }
                false
            }
        }
    }
}

/// Flattens the composite glyph `idx` into the simple glyphs it draws, each with the transform
/// that places it in `idx`'s own coordinate space.
///
/// A composite may name another composite, and a backend is only promised simple glyphs inside a
/// `Shape::Compound` (see `pdf_render`'s `draw_glyph`), so nesting is resolved here rather than
/// left to the backend. Point matching (§glyf, `ARGS_ARE_XY_VALUES` clear) also needs the points
/// of the components placed before it, which is what the second half of the return value carries.
fn expand_components(shapes: &[GlyfShape], idx: u32, stack: &mut Vec<u32>, budget: &mut ComponentBudget)
    -> (Vec<(u32, Transform2F)>, PointContours)
{
    if stack.contains(&idx) {
        warn!("glyf: composite glyph {} contains itself", idx);
        return (Vec::new(), PointContours::new());
    }
    if stack.len() >= MAX_COMPONENT_DEPTH {
        warn!("glyf: composite glyphs nested deeper than {}", MAX_COMPONENT_DEPTH);
        return (Vec::new(), PointContours::new());
    }
    stack.push(idx);
    let mut out = Vec::new();
    let mut points = PointContours::new();
    match shapes.get(idx as usize) {
        None => warn!("glyf: missing glyph {}", idx),
        Some(GlyfShape::Empty) => {}
        Some(GlyfShape::Simple(path)) => {
            let count: usize = path.contours().iter().map(|contour| contour.points().len()).sum();
            if budget.take_points(count) {
                out.push((idx, Transform2F::default()));
                points.extend(path.contours().iter().map(|contour| spec_points(contour).to_vec()));
            }
        }
        Some(GlyfShape::Compound(parts)) => for part in parts {
            if !budget.take() {
                break;
            }
            // the component in the composite's own space, before its offset is known
            let (children, mut contours) = expand_components(shapes, part.gid, stack, budget);
            let matrix = Transform2F { matrix: part.matrix, vector: Vector2F::zero() };
            for contour in contours.iter_mut() {
                for point in contour.iter_mut() {
                    *point = matrix * *point;
                }
            }
            let offset = match part.offset {
                ComponentOffset::Vector(v) if part.scaled_offset => part.matrix * v,
                ComponentOffset::Vector(v) => v,
                ComponentOffset::MatchPoints { parent, child: child_point } => {
                    // Numbering follows the converted outline (see `spec_points`), which differs
                    // from `glyf` for glyphs with consecutive off-curve points.
                    debug!("glyf: glyph {} places a component by matching points {} and {}",
                           idx, parent, child_point);
                    match (point_at(&points, parent), point_at(&contours, child_point)) {
                        (Some(p), Some(c)) => p - c,
                        _ => {
                            warn!("glyf: glyph {} matches points {} and {} that do not exist",
                                  idx, parent, child_point);
                            Vector2F::zero()
                        }
                    }
                }
            };
            // The 2×2 part applies before the placement offset, except for a component that
            // asks for its offset to be scaled as well (see `Component::scaled_offset`).
            let placement = Transform2F { matrix: part.matrix, vector: offset };
            out.extend(children.into_iter().map(|(gid, transform)| (gid, placement * transform)));
            points.extend(contours.into_iter()
                .map(|contour| contour.into_iter().map(|point| point + offset).collect()));
        }
    };
    stack.pop();
    (out, points)
}

/// The simple glyphs a glyph is made of: one entry for a simple glyph, the flattened components
/// for a composite (see `expand_components`).
fn get_components(shapes: &[GlyfShape], idx: u32) -> Vec<(u32, Transform2F)> {
    let mut budget = ComponentBudget::new();
    expand_components(shapes, idx, &mut Vec::new(), &mut budget).0
}

/// Replays a pathfinder outline through a pen.
///
/// Only a composite placed by point matching is expanded into an outline — everything else stays
/// in the encoder's own representation (see `parse_glyphs`) — so this is how such a glyph gets
/// back into the shape the encoder wants.
fn replay_outline(outline: &Outline, pen: &mut impl Pen) {
    for contour in outline.contours() {
        if contour.is_empty() {
            continue;
        }
        pen.move_to(contour.position_of(0));
        for segment in contour.iter(ContourIterFlags::IGNORE_CLOSE_SEGMENT) {
            match segment.kind {
                SegmentKind::Line => pen.line_to(segment.baseline.to()),
                SegmentKind::Quadratic => pen.quad_to(segment.ctrl.from(), segment.baseline.to()),
                SegmentKind::Cubic =>
                    pen.cubic_to(segment.ctrl.from(), segment.ctrl.to(), segment.baseline.to()),
                SegmentKind::None => {}
            }
        }
        pen.close();
    }
}

fn encode_outline<E: Encoder>(outline: &Outline, encoder: &mut E) -> Result<Shape<E>, FontError> {
    let (_, glyph) = encoder.encode_shape::<(), Infallible>(|pen| {
        replay_outline(outline, pen);
        Ok(())
    }).unwrap();
    Ok(Shape::Simple(glyph))
}

/// One parsed glyph, in the encoder's representation.
pub(crate) fn encode_shape<E: Encoder>(shape: &GlyfShape, shapes: &[GlyfShape], idx: u32, encoder: &mut E)
    -> Result<Shape<E>, FontError>
{
    match *shape {
        GlyfShape::Empty => Ok(Shape::Empty),
        GlyfShape::Simple(ref outline) => encode_outline(outline, encoder),
        GlyfShape::Compound(ref parts) => {
            if parts.is_empty() {
                return Ok(Shape::Empty);
            }
            let mut budget = ComponentBudget::new();
            let (children, _) = expand_components(shapes, idx, &mut Vec::new(), &mut budget);
            if children.is_empty() {
                return Ok(Shape::Empty);
            }
            Ok(Shape::Compound(children.into_iter()
                .map(|(gid, transform)| (GlyphId(gid), transform))
                .collect()))
        }
    }
}

/// Parses every glyph of a `glyf`/`loca` font into the encoder's representation.
///
/// The parse itself has to work on outlines (`GlyfShape`): a composite placed by point matching
/// needs their points. Only then is each glyph handed to the encoder, one `encode_shape` call
/// per glyph, which is where the backend decides what a glyph is made of.
pub fn parse_glyphs<E: Encoder>(loca: &[u32], data: &[u8], hmtx: &Hmtx, encoder: &mut E)
    -> Result<Vec<Glyph<E>>, FontError>
{
    let shapes = parse_shapes(loca, data)?;
    let mut glyphs = Vec::with_capacity(shapes.len());
    for (i, shape) in shapes.iter().enumerate() {
        let shape = encode_shape(shape, &shapes, i as u32, encoder)?;
        // OT §hmtx: ids at or beyond `numGlyphs` have no metrics, and cannot be glyphs either.
        let metrics = hmtx.metrics_for_gid(i as u16).unwrap_or_default();
        glyphs.push(Glyph { shape, metrics });
    }
    Ok(glyphs)
}

#[derive(Copy, Clone, Debug)]
struct FlagData {
    flags: u8,
    p: (i32, i32)
}
fn parse_coord(short: bool, same_or_pos: bool) -> impl Fn(&[u8]) -> R<i16> {
    move |i| match (short, same_or_pos) {
        (true, true) => {
            let (i, dx) = be_u8(i)?;
            Ok((i, dx as i16))
        }
        (true, false) => {
            let (i, dx) = be_u8(i)?;
            Ok((i, - (dx as i16)))
        }
        (false, false) => {
            let (i, dx) = be_i16(i)?;
            Ok((i, dx))
        }
        (false, true) => Ok((i, 0))
    }
}
fn mid(a: Vector2F, b: Vector2F) -> Vector2F {
    (a + b) * 0.5
}
fn glyph_shape_positive_contours(i: &[u8], number_of_contours: usize) -> Result<GlyfShape, FontError> {
    let (i, point_indices) = take(2 * number_of_contours)(i)?;
    let (i, num_instructions) = be_u16(i)?;
    let (mut i, _instructions) = take(num_instructions)(i)?;
    
    // total number of points
    let n = 1 + be_u16(slice!(point_indices, 2 * number_of_contours - 2 ..)).get()? as usize;

    let mut flag_data = Vec::with_capacity(n);

    // first load flags
    while flag_data.len() < n {
        let flags = parse(&mut i, be_u8)?;
        let flag = FlagData { flags, p: (0, 0) };
        
        if flags & 8 != 0 {
            let flagcount = parse(&mut i, be_u8)?;
            let num = (n - flag_data.len()).min(flagcount as usize + 1);
            flag_data.extend(iter::repeat(flag).take(num));
        } else {
            flag_data.push(flag);
        }
    }
    require_eq!(flag_data.len(), n);
    
    // now load x coordinates
    let mut x_coord: i32 = 0;
    for &mut FlagData { flags, ref mut p } in flag_data.iter_mut() {
        x_coord += parse(&mut i, parse_coord(flags & 2 != 0, flags & 16 != 0))? as i32;
        p.0 = x_coord;
    }

    // now load y coordinates
    let mut y_coord: i32 = 0;
    for &mut FlagData { flags, ref mut p } in flag_data.iter_mut() {
        y_coord += parse(&mut i, parse_coord(flags & 4 != 0, flags & 32 != 0))? as i32;
        p.1 = y_coord;
    }

    let mut points = flag_data.iter().map(|&FlagData { flags, p }| 
        (flags & 1 != 0, Vector2F::new(p.0 as f32, p.1 as f32))
    );
    let mut start = 0;
    let mut outline = Outline::new();
    for end in iterator(point_indices, be_u16) {
        let n_points = end + 1 - start;
        start += n_points;
        
        if let Some(contour) = contour((&mut points).take(n_points as usize)) {
            outline.push_contour(contour);
        }
    }
    
    Ok(GlyfShape::Simple(outline))
}

pub fn contour(points: impl Iterator<Item=(bool, Vector2F)>) -> Option<Contour> {
    let mut points = points.peekable();
    
    let (start_on, p) = points.next()?;
    let start_off = !start_on;
    let (s, sc) = if start_off {
        // if we start off with an off-curve point, then when we need to find a
        // point on the curve where we can start, and we
        // need to save some state for
        // when we wraparound.
        let sc = p;

        let (next_on, next_p) = match points.peek() {
            Some(&t) => t,
            None => return None,
        };

        let p = if !next_on {
            // next point is also a curve point, so interpolate an on-point curve
            mid(p, next_p)
        } else {
            // we're using point i+1 as the starting point, so skip it
            let _ = points.next();
            
            // otherwise just use the next point as our start point
            next_p
        };
        (p, Some(sc))
    } else {
        (p, None)
    };
    
    let mut contour = Contour::new();
    contour.push_endpoint(s);
    
    let mut c = None;
    for (on_curve, p) in points {
        if !on_curve {
            // if it's a curve
            if let Some(c) = c {
                // two off-curve control points in a row means interpolate an on-curve
                // midpoint
                contour.push_quadratic(c, mid(c, p));
            }
            c = Some(p);
        } else {
            if let Some(c) = c.take() {
                contour.push_quadratic(c, p);
            } else {
                contour.push_endpoint(p);
            }
        }
    }
    
    if let Some(sc) = sc {
        if let Some(c) = c {
            contour.push_quadratic(c, mid(c, sc));
        }
        contour.push_quadratic(sc, s);
    } else {
        if let Some(c) = c {
            contour.push_quadratic(c, s);
        } else {
            contour.push_endpoint(s);
        }
    }

    contour.close();
    Some(contour)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Instant;

    /// One triangle.
    fn leaf() -> GlyfShape {
        let mut outline = Outline::new();
        let contour = contour(
            vec![
                (true, Vector2F::new(0., 0.)),
                (true, Vector2F::new(100., 0.)),
                (true, Vector2F::new(100., 100.)),
            ].into_iter(),
        ).expect("triangle");
        outline.push_contour(contour);
        GlyfShape::Simple(outline)
    }

    fn component(gid: u32, matrix: Matrix2x2F, offset: Vector2F) -> Component {
        Component { gid, matrix, offset: ComponentOffset::Vector(offset), scaled_offset: false }
    }

    fn identity() -> Matrix2x2F {
        Matrix2x2F::row_major(1., 0., 0., 1.)
    }

    /// Glyph `n` is two components of glyph `n + 1`, the last one is a plain leaf: the
    /// expansion is `2 ^ depth` outlines.
    fn doubling_chain(depth: u32) -> Vec<GlyfShape> {
        let mut shapes = Vec::new();
        for level in 0 .. depth {
            let part = component(level + 1, identity(), Vector2F::zero());
            shapes.push(GlyfShape::Compound(vec![part, part]));
        }
        shapes.push(leaf());
        shapes
    }

    #[test]
    fn many_references_to_a_big_glyph_stop_at_the_point_budget() {
        // A composite naming one 1000-point glyph 5000 times is under the expansion cap but
        // would copy 5 million points.
        let mut points = Vec::new();
        for n in 0 .. 1000 {
            points.push((true, Vector2F::new(n as f32, (n % 7) as f32)));
        }
        let mut outline = Outline::new();
        outline.push_contour(contour(points.into_iter()).expect("big contour"));
        let part = component(1, identity(), Vector2F::zero());
        let shapes = vec![GlyfShape::Compound(vec![part; 5000]), GlyfShape::Simple(outline)];
        let mut budget = ComponentBudget::new();
        let (children, contours) = expand_components(&shapes, 0, &mut Vec::new(), &mut budget);
        let copied: usize = contours.iter().map(|contour| contour.len()).sum();
        assert!(!children.is_empty(), "the budget must leave some of the glyph");
        assert!(copied > 0 && copied <= MAX_COMPONENT_POINTS + 1000, "copied {} points", copied);
    }

    #[test]
    fn a_doubling_composite_chain_expands_within_the_budget() {
        // the depth cap alone does not bound the work — a chain of composites that
        // each name two components asks for 2 ^ 14 outlines. Without the budget every leaf is
        // expanded; with it the expansion stops at MAX_COMPONENT_EXPANSIONS.
        let shapes = doubling_chain(14);
        let start = Instant::now();
        let children = get_components(&shapes, 0);
        assert!(start.elapsed().as_secs() < 2, "expansion took {:?}", start.elapsed());
        assert!(!children.is_empty(), "the budget must leave some of the glyph");
        assert!(
            children.len() <= MAX_COMPONENT_EXPANSIONS,
            "{} components: the component budget did not stop the expansion",
            children.len()
        );
    }

    #[test]
    fn a_composite_that_names_a_composite_is_flattened() {
        // A backend is only promised simple glyphs inside `Shape::Compound`, so a nested
        // composite has to come out of the expansion as its leaves, with the transforms
        // composed: scaled by 2 and moved by (1, 0), then moved by (10, 20).
        let shapes = vec![
            GlyfShape::Compound(vec![component(1, identity(), Vector2F::new(10., 20.))]),
            GlyfShape::Compound(vec![component(
                2,
                Matrix2x2F::from_scale(Vector2F::splat(2.)),
                Vector2F::new(1., 0.),
            )]),
            leaf(),
        ];
        let children = get_components(&shapes, 0);
        assert_eq!(children.len(), 1, "{:?}", children);
        let (gid, transform) = children[0];
        assert_eq!(gid, 2);
        assert_eq!(transform * Vector2F::zero(), Vector2F::new(11., 20.));
        assert_eq!(transform * Vector2F::new(1., 1.), Vector2F::new(13., 22.));
    }

    #[test]
    fn a_composite_that_contains_itself_draws_nothing() {
        let shapes = vec![GlyfShape::Compound(vec![component(0, identity(), Vector2F::zero())])];
        assert!(get_components(&shapes, 0).is_empty());
    }

    #[test]
    fn a_missing_component_is_skipped() {
        let shapes = vec![
            GlyfShape::Compound(vec![
                component(7, identity(), Vector2F::zero()),
                component(1, identity(), Vector2F::new(5., 0.)),
            ]),
            leaf(),
        ];
        let children = get_components(&shapes, 0);
        assert_eq!(children.len(), 1, "{:?}", children);
        assert_eq!(children[0].0, 1);
    }
}
