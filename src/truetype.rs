use std::collections::HashMap;
use std::iter;
use std::ops::Deref;
use crate::opentype::post::parse_post;
use crate::{Font, Glyph, R, IResultExt, GlyphId, Name, Info, FontError};
use crate::parsers::{iterator, parse};
use pdf_encoding::Encoding;
use nom::{
    number::complete::{be_u8, be_i8, be_i16, be_u16},
    bytes::complete::take,
    sequence::tuple
};
use pathfinder_content::outline::{Outline, Contour};
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
}

#[derive(Clone)]
pub enum Shape {
    Simple(Outline),
    Compound(Vec<Component>),
    Empty
}

#[derive(Clone)]
pub struct TrueTypeFont {
    shapes: Vec<Shape>,
    pub cmap: Option<CMap>,
    encoding: Option<Encoding>,
    hmtx: Hmtx,
    num_glyphs: u32,
    units_per_em: u16,
    bbox: RectF,
    kern: KernTable,
    name: Name,
    info: Info,
    pub name_map: HashMap<String, u16>,
}

impl TrueTypeFont {
    pub fn parse(data: &[u8]) -> Result<Self, FontError> {
        let tables = parse_tables(data)?;
        TrueTypeFont::parse_glyf(tables)
    }
    pub fn parse_glyf(tables: Tables<impl Deref<Target=[u8]>>) -> Result<Self, FontError> {
        let head = parse_head(expect!(tables.get(b"head"), "no head"))?;
        let maxp = parse_maxp(expect!(tables.get(b"maxp"), "no maxp"))?;
        let loca = parse_loca(expect!(tables.get(b"loca"), "no loca"), &head, &maxp)?;
        let glyf = expect!(tables.get(b"glyf"), "no glyf");
        let hhea = parse_hhea(expect!(tables.get(b"hhea"), "no hhea"))?;
        let hmtx = parse_hmtx(expect!(tables.get(b"hmtx"), "no hmtx"), &hhea, &maxp)?;
        
        let shapes = parse_shapes(&loca, glyf)?;
        
        TrueTypeFont::from_shapes_and_metrics(tables, shapes, hmtx)
    }
    pub fn from_shapes_and_metrics(tables: Tables<impl Deref<Target=[u8]>>, shapes: Vec<Shape>, hmtx: Hmtx) -> Result<TrueTypeFont, FontError> {
        let head = parse_head(expect!(tables.get(b"head"), "no head"))?;
        // OT §maxp: this is the authoritative glyph count; `loca` and `post` have to agree
        // with it, and it must not silently differ from what the shapes contain.
        let maxp = parse_maxp(expect!(tables.get(b"maxp"), "no maxp"))?;
        if shapes.len() != maxp.num_glyphs as usize {
            warn!("glyf/loca describe {} glyphs, maxp says {}", shapes.len(), maxp.num_glyphs);
        }
        let (cmap, encoding) = match tables.get(b"cmap").map(|data| parse_cmap(data)).transpose()? {
            Some((cmap, encoding)) => (Some(cmap), Some(encoding)),
            None => (None, None)
        };
        let name = tables.get(b"name").map(|data| parse_name(data)).transpose()?.unwrap_or_default();
        let os2 = tables.get(b"OS/2").map(|data| parse_os2(data)).transpose()?;

        let post = t!(tables.get(b"post").map(parse_post).transpose());
        let mut name_map = HashMap::new();
        if let Some(post) = post {
            name_map.extend(post.names.into_iter().enumerate().map(|(i, name)| (name.into(), i as u16)));
        }
        
        Ok(TrueTypeFont {
            shapes,
            cmap,
            encoding,
            hmtx,
            num_glyphs: maxp.num_glyphs as u32,
            units_per_em: head.units_per_em,
            bbox: head.bbox(),
            kern: tables.get(b"kern").map(|data| parse_kern(data)).transpose()?.unwrap_or_default(),
            name,
            info: Info {
                weight: os2.map(|t| t.weight),
            },
            name_map,
        })
    }
    fn get_path(&self, idx: u32) -> Option<Outline> {
        get_outline(&self.shapes, idx)
    }
}
impl Font for TrueTypeFont {
    fn num_glyphs(&self) -> u32 {
        // OT §maxp, not `shapes.len()`: both `OpenTypeFont` and `TrueTypeFont` must answer
        // the same number for the same font.
        self.num_glyphs
    }
    fn font_matrix(&self) -> Transform2F {
        let scale = 1.0 / self.units_per_em as f32;
        Transform2F::from_scale(Vector2F::splat(scale.into()))
    }
    fn glyph(&self, id: GlyphId) -> Option<Glyph> {
        if id.0 > u16::max_value() as u32 {
            return None;
        }
        
        debug!("get gid {:?}", id);
        let path = self.get_path(id.0)?;
        let metrics = self.hmtx.metrics_for_gid(id.0 as u16)?;
        
        Some(Glyph {
            path,
            metrics
        })
    }
    fn is_empty_glyph(&self, gid: GlyphId) -> bool {
        if gid.0 > u16::max_value() as u32 {
            return true;
        }
        match self.shapes.get(gid.0 as usize) {
            None => true,
            Some(Shape::Empty) => true,
            _ => false
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

pub fn parse_shapes(loca: &[u32], data: &[u8]) -> Result<Vec<Shape>, FontError> {
    // OT §loca: the last offset is the length of `glyf`; anything larger points outside it.
    require!(loca.last().map_or(true, |&end| end as usize <= data.len()));
    let mut shapes = Vec::with_capacity(loca.len() - 1);
    for (i, (start, end)) in loca.iter().cloned().tuple_windows().enumerate() {
        // OT §glyf: one corrupt glyph record must not cost the whole font. `parse_glyph_shape`
        // is allowed to read past its slice, so a bad record is just an empty glyph here.
        let shape = match data.get(start as usize .. end as usize) {
            Some(slice) => match parse_glyph_shape(slice) {
                Ok(shape) => shape,
                Err(e) => {
                    warn!("glyph {}: {:?} — treated as an empty glyph", i, e);
                    Shape::Empty
                }
            },
            None => {
                warn!("glyph {}: loca range {}..{} outside glyf — treated as an empty glyph", i, start, end);
                Shape::Empty
            }
        };
        shapes.push(shape);
    }
    Ok(shapes)
}
// the following code is borrowed from stb-truetype and modified heavily

fn parse_glyph_shape(data: &[u8]) -> Result<Shape, FontError> {
    if data.len() == 0 {
        return Ok(Shape::Empty);
    }
    let (i, number_of_contours) = be_i16(data)?;
    
    let (i, _) = take(8usize)(i)?;
    //debug!("n_contours: {}", number_of_contours);
    match number_of_contours {
        0 => Ok(Shape::Empty),
        n if n >= 0 => glyph_shape_positive_contours(i, number_of_contours as usize),
        -1 => compound(i).get(),
        n => error!("Contour format {} not supported.", n)
    }
}

pub fn compound(mut input: &[u8]) -> R<Shape> {
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
        parts.push(Component { gid: gidx as u32, matrix: transform.matrix, offset });
        // More components ?
        if flags & 0x20 == 0 {
            break;
        }
    }
    Ok((input, Shape::Compound(parts)))
}

/// The points of a contour without the explicit closing point that `contour()` appends, i.e.
/// the numbering OT §glyf uses for point matching.
fn spec_points(contour: &Contour) -> &[Vector2F] {
    let points = contour.points();
    if points.len() > 1 && points[0] == points[points.len() - 1] {
        &points[.. points.len() - 1]
    } else {
        points
    }
}

/// Point `index` of an outline, counting continuously over its contours (§glyf point numbers).
fn point_at(outline: &Outline, index: u16) -> Option<Vector2F> {
    let mut n = index as usize;
    for contour in outline.contours() {
        let points = spec_points(contour);
        if n < points.len() {
            return Some(points[n]);
        }
        n -= points.len();
    }
    None
}

/// A composite glyph may nest; the spec also forbids a glyph that contains itself. Both are
/// bounded here so a malformed font cannot loop or overflow the stack.
const MAX_COMPONENT_DEPTH: usize = 16;

pub fn get_outline(shapes: &[Shape], idx: u32) -> Option<Outline> {
    expand_outline(shapes, idx, &mut Vec::new())
}

fn expand_outline(shapes: &[Shape], idx: u32, stack: &mut Vec<u32>) -> Option<Outline> {
    if stack.contains(&idx) {
        warn!("glyf: composite glyph {} contains itself", idx);
        return Some(Outline::new());
    }
    if stack.len() >= MAX_COMPONENT_DEPTH {
        warn!("glyf: composite glyphs nested deeper than {}", MAX_COMPONENT_DEPTH);
        return Some(Outline::new());
    }
    stack.push(idx);
    let result = match shapes.get(idx as usize) {
        None => None,
        Some(&Shape::Empty) => Some(Outline::new()),
        Some(&Shape::Simple(ref path)) => Some(path.clone()),
        Some(&Shape::Compound(ref parts)) => {
            let mut outline = Outline::new();
            for part in parts {
                let mut child = match expand_outline(shapes, part.gid, stack) {
                    Some(child) => child,
                    None => {
                        warn!("glyf: glyph {} references missing glyph {}", idx, part.gid);
                        continue;
                    }
                };
                // The 2×2 part applies before the placement offset.
                child.transform(&Transform2F { matrix: part.matrix, vector: Vector2F::zero() });
                let offset = match part.offset {
                    ComponentOffset::Vector(v) => v,
                    ComponentOffset::MatchPoints { parent, child: child_point } => {
                        match (point_at(&outline, parent), point_at(&child, child_point)) {
                            (Some(p), Some(c)) => p - c,
                            _ => {
                                warn!("glyf: glyph {} matches points {} and {} that do not exist",
                                      idx, parent, child_point);
                                Vector2F::zero()
                            }
                        }
                    }
                };
                child.transform(&Transform2F::from_translation(offset));
                outline.push_outline(child);
            }
            Some(outline)
        }
    };
    stack.pop();
    result
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
fn glyph_shape_positive_contours(i: &[u8], number_of_contours: usize) -> Result<Shape, FontError> {
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
    
    Ok(Shape::Simple(outline))
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
