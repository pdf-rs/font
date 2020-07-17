use std::iter;
use std::ops::Deref;
use crate::{Font, Glyph, R, IResultExt, GlyphId, Name};
use crate::parsers::{iterator, parse};
use encoding::Encoding;
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
};
use pathfinder_geometry::{transform2d::Matrix2x2F};
use itertools::Itertools;

#[derive(Clone)]
pub enum Shape {
    Simple(Outline),
    Compound(Vec<(u32, Transform2F)>),
    Empty
}

#[derive(Clone)]
pub struct TrueTypeFont {
    shapes: Vec<Shape>,
    cmap: Option<CMap>,
    hmtx: Hmtx,
    units_per_em: u16,
    bbox: RectF,
    kern: KernTable,
    name: Name
}

impl TrueTypeFont {
    pub fn parse(data: &[u8]) -> Self {
        let tables = parse_tables(data).get();
        TrueTypeFont::parse_glyf(tables)
    }
    pub fn parse_glyf(tables: Tables<impl Deref<Target=[u8]>>) -> Self {
        let head = parse_head(tables.get(b"head").expect("no head")).get();
        let maxp = parse_maxp(tables.get(b"maxp").expect("no maxp")).get();
        let loca = parse_loca(tables.get(b"loca").expect("no loca"), &head, &maxp).get();
        let hhea = parse_hhea(tables.get(b"hhea").expect("no hhea")).get();
        let hmtx = parse_hmtx(tables.get(b"hmtx").expect("no hmtx"), &hhea, &maxp).get();
        
        let shapes = parse_shapes(&loca, tables.get(b"glyf").unwrap());
        
        TrueTypeFont::from_shapes_and_metrics(tables, shapes, hmtx)
    }
    pub fn from_shapes_and_metrics(tables: Tables<impl Deref<Target=[u8]>>, shapes: Vec<Shape>, hmtx: Hmtx) -> TrueTypeFont {
        let head = parse_head(tables.get(b"head").expect("no head")).get();
        let cmap = tables.get(b"cmap").map(|data| parse_cmap(data).get());
        let name = tables.get(b"name").map(|data| parse_name(data).get()).unwrap_or_default();
        
        TrueTypeFont {
            shapes,
            cmap,
            hmtx,
            units_per_em: head.units_per_em,
            bbox: head.bbox(),
            kern: tables.get(b"kern").map(|data| parse_kern(data).get()).unwrap_or_default(),
            name
        }
    }
    fn get_path(&self, idx: u32) -> Option<Outline> {
        get_outline(&self.shapes, idx)
    }
}
impl Font for TrueTypeFont {
    fn num_glyphs(&self) -> u32 {
        self.shapes.len() as u32
    }
    fn font_matrix(&self) -> Transform2F {
        let scale = 1.0 / self.units_per_em as f32;
        Transform2F::from_scale(Vector2F::splat(scale.into()))
    }
    fn glyph(&self, id: GlyphId) -> Option<Glyph> {
        assert!(id.0 <= u16::max_value() as u32);
        debug!("get gid {:?}", id);
        let path = self.get_path(id.0)?;
        let metrics = self.hmtx.metrics_for_gid(id.0 as u16);
        
        Some(Glyph {
            path,
            metrics
        })
    }
    fn gid_for_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        self.gid_for_unicode_codepoint(codepoint)
    }
    fn gid_for_unicode_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        debug!("glyph for unicode codepoint {0} ({0:#x})", codepoint);
        match self.cmap {
            Some(ref cmap) => cmap.get_codepoint(codepoint).map(GlyphId),
            None => None
        }
    }
    fn encoding(&self) -> Option<Encoding> {
        Some(Encoding::Unicode)
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

pub fn parse_shapes(loca: &[u32], data: &[u8]) -> Vec<Shape> {
    let mut shapes = Vec::with_capacity(loca.len() - 1);
    for (i, (start, end)) in loca.iter().cloned().tuple_windows().enumerate() {
        let slice = &data[start as usize .. end as usize];
        debug!("gid {} : data[{} .. {}]", i, start, end);
        let shape = parse_glyph_shape(slice).get();
        shapes.push(shape);
    }
    shapes
}
// the following code is borrowed from stb-truetype and modified heavily

fn parse_glyph_shape(data: &[u8]) -> R<Shape> {
    if data.len() == 0 {
        debug!("empty glyph");
        return Ok((data, Shape::Empty));
    }
    let (i, number_of_contours) = be_i16(data)?;
    
    let (i, _) = take(8usize)(i)?;
    debug!("n_contours: {}", number_of_contours);
    match number_of_contours {
        0 => Ok((i, Shape::Empty)),
        n if n >= 0 => glyph_shape_positive_contours(i, number_of_contours as usize),
        -1 => compound(i),
        n => panic!("Contour format {} not supported.", n)
    }
}

pub fn compound(mut input: &[u8]) -> R<Shape> {
    // Compound shapes
    let mut parts = Vec::new();
    loop {
        let (flags, gidx) = parse(&mut input, tuple((be_u16, be_u16)))?;
        let mut transform = Transform2F::default();
        if flags & 2 != 0 {
            // XY values
            if flags & 1 != 0 {
                // shorts
                transform.vector = parse(&mut input, vec_i16)?
            } else {
                transform.vector = parse(&mut input, vec_i8)?
            }
        } else {
            panic!("Matching points not supported.");
        };
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
        parts.push((gidx as u32, transform));
        // More components ?
        if flags & 0x20 == 0 {
            break;
        }
    }
    Ok((input, Shape::Compound(parts)))
}

pub fn get_outline(shapes: &[Shape], idx: u32) -> Option<Outline> {
    match shapes.get(idx as usize)? {
        &Shape::Simple(ref path) => Some(path.clone()),
        &Shape::Compound(ref parts) => {
            let mut outline = Outline::new();
            for &(gid, tr) in parts {
                if let Some(Shape::Simple(ref path)) = shapes.get(gid as usize) {
                    let mut path = path.clone();
                    path.transform(&tr);
                    outline.push_outline(path);
                }
            }
            Some(outline)
        }
        &Shape::Empty => Some(Outline::new())
    }
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
fn glyph_shape_positive_contours(i: &[u8], number_of_contours: usize) -> R<Shape> {
    let (i, point_indices) = take(2 * number_of_contours)(i)?;
    let (i, num_instructions) = be_u16(i)?;
    let (mut i, _instructions) = take(num_instructions)(i)?;
    
    // total number of points
    let n = 1 + be_u16(&point_indices[2 * number_of_contours - 2 ..]).get() as usize;

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
    assert_eq!(flag_data.len(), n);
    
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
    
    Ok((i, Shape::Simple(outline)))
}

pub fn contour(points: impl Iterator<Item=(bool, Vector2F)>) -> Option<Contour> {
    let mut points = points.peekable();
    
    let (start_on, p) = points.next().unwrap();
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
