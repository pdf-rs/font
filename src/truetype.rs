use std::iter;
use std::ops::Deref;
use std::collections::HashMap;
use crate::{Font, Glyph, R, IResultExt, GlyphId};
use crate::parsers::{iterator, parse};
use encoding::Encoding;
use nom::{
    number::complete::{be_u8, be_i8, be_i16, be_u16},
    bytes::complete::take,
    sequence::tuple
};
use vector::{Outline, Contour, Transform, Vector, Rect};
use crate::opentype::{parse_tables, parse_head, parse_maxp, parse_loca, parse_cmap, parse_hhea, parse_hmtx, parse_kern, Hmtx, Tables, CMap};
use pathfinder_geometry::{transform2d::Matrix2x2F};
use itertools::Itertools;

#[derive(Clone)]
pub enum Shape<O: Outline> {
    Simple(O),
    Compound(Vec<(u32, Transform)>),
    Empty
}
pub struct TrueTypeFont<O: Outline> {
    shapes: Vec<Shape<O>>,
    cmap: Option<CMap>,
    hmtx: Hmtx,
    units_per_em: u16,
    bbox: Rect,
    kern: HashMap<(u16, u16), i16>,
}

impl<O: Outline> TrueTypeFont<O> {
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
    pub fn from_shapes_and_metrics(tables: Tables<impl Deref<Target=[u8]>>, shapes: Vec<Shape<O>>, hmtx: Hmtx) -> TrueTypeFont<O> {
        let head = parse_head(tables.get(b"head").expect("no head")).get();
        let cmap = tables.get(b"cmap").map(|data| parse_cmap(data).get());
        
        let cvt = |i: i16| i as f32;
        
        TrueTypeFont {
            shapes,
            cmap,
            hmtx,
            units_per_em: head.units_per_em,
            bbox: head.bbox(),
            kern: tables.get(b"kern").map(|data| parse_kern(data).get()).unwrap_or_default()
        }
    }
    fn get_path(&self, idx: u32) -> O {
        let shape = &self.shapes[idx as usize];
    
        match shape {
            Shape::Simple(ref path) => path.clone(),
            Shape::Compound(ref parts) => {
                dbg!(parts);
                let mut path = O::empty();
                for &(gid, tr) in parts {
                    path.add_outline(self.get_path(gid).transform(tr));
                }
                path
            }
            Shape::Empty => O::empty()
        }
    }
}
impl<O: Outline> Font<O> for TrueTypeFont<O> {
    fn num_glyphs(&self) -> u32 {
        self.shapes.len() as u32
    }
    fn font_matrix(&self) -> Transform {
        let scale = 1.0 / self.units_per_em as f32;
        Transform::from_scale(Vector::splat(scale.into()))
    }
    fn glyph(&self, id: GlyphId) -> Option<Glyph<O>> {
        assert!(id.0 <= u16::max_value() as u32);
        debug!("get gid {:?}", id);
        let path = self.get_path(id.0);
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
    fn bbox(&self) -> Option<Rect> {
        Some(self.bbox)
    }
    fn kerning(&self, left: GlyphId, right: GlyphId) -> f32 {
        self.kern.get(&(left.0 as u16, right.0 as u16)).cloned().unwrap_or(0) as f32
    }
}

#[inline]
fn vec_i8(i: &[u8]) -> R<Vector> {
    let (i, x) = be_i8(i)?;
    let (i, y) = be_i8(i)?;
    Ok((i, Vector::new(x as f32, y as f32)))
}
#[inline]
fn vec_i16(i: &[u8]) -> R<Vector> {
    let (i, x) = be_i16(i)?;
    let (i, y) = be_i16(i)?;
    Ok((i, Vector::new(x as f32, y as f32)))
}
#[inline]
fn fraction_i16(i: &[u8]) -> R<f32> {
    let (i, s) = be_i16(i)?;
    Ok((i, s as f32 / 16384.0))
}

pub fn parse_shapes<O: Outline>(loca: &[u32], data: &[u8]) -> Vec<Shape<O>> {
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

fn parse_glyph_shape<O: Outline>(data: &[u8]) -> R<Shape<O>> {
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

pub fn compound<O: Outline>(mut input: &[u8]) -> R<Shape<O>> {
    // Compound shapes
    let mut parts = Vec::new();
    loop {
        let (flags, gidx) = parse(&mut input, tuple((be_u16, be_u16)))?;
        let mut transform = Transform::default();
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
            transform.matrix = Matrix2x2F::from_scale(Vector::splat(scale));
        } else if flags & (1 << 6) != 0 {
            // WE_HAVE_AN_X_AND_YSCALE
            let (sx, sy) = parse(&mut input, tuple((fraction_i16, fraction_i16)))?;
            let s = Vector::new(sx, sy);
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
fn mid(a: Vector, b: Vector) -> Vector {
    (a + b) * Vector::splat(0.5)
}
fn glyph_shape_positive_contours<O: Outline>(i: &[u8], number_of_contours: usize) -> R<Shape<O>> {
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
        (flags & 1 != 0, Vector::new(p.0 as f32, p.1 as f32))
    );
    let mut start = 0;
    let mut outline = O::empty();
    for end in iterator(point_indices, be_u16) {
        let n_points = end + 1 - start;
        start += n_points;
        
        if let Some(contour) = contour((&mut points).take(n_points as usize)) {
            outline.add_contour(contour);
        }
    }
    
    Ok((i, Shape::Simple(outline)))
}

pub fn contour<C: Contour>(points: impl Iterator<Item=(bool, Vector)>) -> Option<C> {
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
    
    let mut contour = C::new();
    contour.move_to(s);
    
    let mut c = None;
    for (on_curve, p) in points {
        if !on_curve {
            // if it's a curve
            if let Some(c) = c {
                // two off-curve control points in a row means interpolate an on-curve
                // midpoint
                contour.quadratic_curve_to(c, mid(c, p));
            }
            c = Some(p);
        } else {
            if let Some(c) = c.take() {
                contour.quadratic_curve_to(c, p);
            } else {
                contour.line_to(p);
            }
        }
    }
    
    if let Some(sc) = sc {
        if let Some(c) = c {
            contour.quadratic_curve_to(c, mid(c, sc));
        }
        contour.quadratic_curve_to(sc, s);
    } else {
        if let Some(c) = c {
            contour.quadratic_curve_to(c, s);
        } else {
            contour.line_to(s);
        }
    }

    contour.close();
    Some(contour)
}
