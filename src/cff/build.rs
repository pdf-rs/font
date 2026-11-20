//! Writing a bare CFF font from glyph outlines (TN5177 §"Generating the file").
//!
//! This is the inverse of the parser in the parent module: it takes an outline per glyph plus
//! its advance width and produces the bytes a `FontFile3` stream with `/Subtype /Type1C`
//! wants, or that `read_cff` reads back. It writes a plain (non-CID, non-subroutinised) font:
//! a `Header`, the `Name`, `Top DICT`, `String` and `Global Subr` INDEXes, a `charset` in
//! format 0, the `CharStrings` INDEX and a `Private` DICT.
//!
//! Glyph 0 is `.notdef` by convention: add it first. Glyph *names* become charset SIDs, so a
//! reader gets `gid_for_name` back; names that are not among the standard strings go into the
//! String INDEX. The encoding is left out on purpose — the default, StandardEncoding, is what
//! a PDF viewer replaces with the `/Encoding` it is given, and a custom encoding here would
//! only be a second, redundant statement about glyph order.

use pathfinder_content::outline::{ContourIterFlags, Outline};
use pathfinder_content::segment::SegmentKind;
use pathfinder_geometry::{rect::RectF, vector::Vector2F};

use super::{STANDARD_STRINGS, SID};
use crate::FontError;

/// One glyph as it goes into the font.
struct GlyphSource {
    name: String,
    advance: f32,
    outline: Outline,
}

/// Builds a bare CFF font from outlines.
pub struct FontBuilder {
    ps_name: String,
    units_per_em: f32,
    glyphs: Vec<GlyphSource>,
}

impl FontBuilder {
    /// A font with no glyphs. `units_per_em` becomes the `FontMatrix` (`1/units_per_em`), so
    /// outlines are given in font units.
    pub fn new(ps_name: impl Into<String>, units_per_em: f32) -> Self {
        FontBuilder {
            ps_name: ps_name.into(),
            units_per_em: if units_per_em > 0.0 { units_per_em } else { 1000.0 },
            glyphs: Vec::new(),
        }
    }

    /// Add a glyph and get its GID back. `advance` is in font units.
    pub fn add_glyph(&mut self, name: impl Into<String>, advance: f32, outline: Outline) -> u32 {
        self.glyphs.push(GlyphSource { name: name.into(), advance, outline });
        (self.glyphs.len() - 1) as u32
    }

    /// Add a glyph that draws nothing, such as a space.
    pub fn add_empty(&mut self, name: impl Into<String>, advance: f32) -> u32 {
        self.add_glyph(name, advance, Outline::new())
    }

    pub fn len(&self) -> usize {
        self.glyphs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.glyphs.is_empty()
    }

    /// Serialise. Fails for a font with no glyphs: a CFF always has glyph 0, `.notdef`
    /// (CFF §"CharStrings INDEX").
    pub fn build(&self) -> Result<Vec<u8>, FontError> {
        if self.glyphs.is_empty() {
            return Err(FontError::Other("a CFF needs at least one glyph (.notdef)".into()));
        }

        // names -> SIDs, collecting the names the standard strings do not carry
        let mut strings: Vec<String> = Vec::new();
        let sids: Vec<SID> = self.glyphs.iter()
            .map(|glyph| sid_for_name(&glyph.name, &mut strings))
            .collect();

        // defaultWidthX = nominalWidthX = 0, so a charstring carries the plain advance
        let default_width = 0.0;
        let nominal_width = 0.0;
        let charstrings: Vec<Vec<u8>> = self.glyphs.iter()
            .map(|glyph| charstring(&glyph.outline, glyph.advance, default_width, nominal_width))
            .collect();

        // CFF §"charset": gid 0 is .notdef and is not listed; format 0 is one SID per glyph.
        let mut charset = Vec::with_capacity(1 + 2 * (self.glyphs.len() - 1));
        charset.push(0u8);
        for &sid in sids.iter().skip(1) {
            charset.extend_from_slice(&(sid as u16).to_be_bytes());
        }

        // A Private DICT is required by the spec; both widths are 0, which is their default,
        // so the two entries cost four bytes and state what the charstrings rely on.
        let mut private = Vec::new();
        dict_int(&mut private, 0);
        private.push(20); // defaultWidthX
        dict_int(&mut private, 0);
        private.push(21); // nominalWidthX

        let bbox = font_bbox(&self.glyphs).unwrap_or_else(RectF::default);

        let header = [1u8, 0, 4, 4];
        let name_index = index(&[self.ps_name.as_bytes()]);
        let string_index = index(&strings.iter().map(|s| s.as_bytes()).collect::<Vec<_>>());
        let global_subrs = index(&[]);
        let charstring_items: Vec<&[u8]> = charstrings.iter().map(|c| c.as_slice()).collect();
        let charstrings_index = index(&charstring_items);

        // Offsets are written as longints (five bytes each), so the Top DICT has the same
        // length with placeholder offsets as with the real ones: measured once, then filled in.
        let placeholder = self.top_dict(bbox, 0, 0, private.len(), 0);
        let top_dict_index_len = index(&[placeholder.as_slice()]).len();

        let charset_offset = header.len() + name_index.len() + top_dict_index_len
            + string_index.len() + global_subrs.len();
        let charstrings_offset = charset_offset + charset.len();
        let private_offset = charstrings_offset + charstrings_index.len();

        let top_dict = self.top_dict(bbox, charset_offset, charstrings_offset,
                                     private.len(), private_offset);
        debug_assert_eq!(top_dict.len(), placeholder.len(),
                         "the Top DICT changed length when the offsets went in");
        let top_dict_index = index(&[top_dict.as_slice()]);

        let mut out = Vec::with_capacity(private_offset + private.len());
        out.extend_from_slice(&header);
        out.extend_from_slice(&name_index);
        out.extend_from_slice(&top_dict_index);
        out.extend_from_slice(&string_index);
        out.extend_from_slice(&global_subrs);
        out.extend_from_slice(&charset);
        out.extend_from_slice(&charstrings_index);
        out.extend_from_slice(&private);
        debug_assert_eq!(out.len(), private_offset + private.len());
        Ok(out)
    }

    /// The Top DICT. Every integer here is a longint, which is what lets `build` measure it
    /// before the offsets it holds exist.
    fn top_dict(&self, bbox: RectF, charset: usize, charstrings: usize,
                private_size: usize, private_offset: usize) -> Vec<u8> {
        let mut dict = Vec::new();
        for v in [bbox.min_x(), bbox.min_y(), bbox.max_x(), bbox.max_y()] {
            dict_int(&mut dict, v.round() as i32);
        }
        dict.push(5); // FontBBox

        let scale = 1.0 / self.units_per_em;
        dict_real(&mut dict, scale);
        dict_int(&mut dict, 0);
        dict_int(&mut dict, 0);
        dict_real(&mut dict, scale);
        dict_int(&mut dict, 0);
        dict_int(&mut dict, 0);
        dict.extend_from_slice(&[12, 7]); // FontMatrix

        dict_int(&mut dict, charset as i32);
        dict.push(15); // charset

        dict_int(&mut dict, charstrings as i32);
        dict.push(17); // CharStrings

        dict_int(&mut dict, private_size as i32);
        dict_int(&mut dict, private_offset as i32);
        dict.push(18); // Private
        dict
    }
}

/// The SID of a glyph name: one of the standard strings, or an entry appended to the String
/// INDEX of this font.
fn sid_for_name(name: &str, strings: &mut Vec<String>) -> SID {
    if let Some(n) = STANDARD_STRINGS.iter().position(|&s| s == name) {
        return n as SID;
    }
    if let Some(n) = strings.iter().position(|s| s == name) {
        return (STANDARD_STRINGS.len() + n) as SID;
    }
    strings.push(name.to_owned());
    (STANDARD_STRINGS.len() + strings.len() - 1) as SID
}

fn font_bbox(glyphs: &[GlyphSource]) -> Option<RectF> {
    let mut bbox: Option<RectF> = None;
    for glyph in glyphs {
        for contour in glyph.outline.contours() {
            if contour.is_empty() {
                continue;
            }
            let b = contour.bounds();
            bbox = Some(match bbox {
                None => b,
                Some(bbox) => bbox.union_rect(b),
            });
        }
    }
    bbox
}

/// CFF §"INDEX data": a count, an offset size, count+1 one-based offsets, then the data.
fn index(items: &[&[u8]]) -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(&(items.len() as u16).to_be_bytes());
    if items.is_empty() {
        return out;
    }
    let total: usize = items.iter().map(|item| item.len()).sum();
    // the last offset is total + 1, and that is the one that picks the size
    let off_size: u8 = if total < 0xff { 1 } else if total < 0xffff { 2 } else { 3 };
    out.push(off_size);
    let mut offset = 1usize;
    for item in items {
        push_offset(&mut out, offset, off_size);
        offset += item.len();
    }
    push_offset(&mut out, offset, off_size);
    for item in items {
        out.extend_from_slice(item);
    }
    out
}

fn push_offset(out: &mut Vec<u8>, offset: usize, size: u8) {
    for n in (0..size).rev() {
        out.push((offset >> (8 * n as usize)) as u8);
    }
}

/// A DICT integer as a longint (`29`, then four bytes): always five bytes, never a shape the
/// reader has to guess at, and what lets the Top DICT be measured before its offsets exist.
fn dict_int(out: &mut Vec<u8>, value: i32) {
    out.push(29);
    out.extend_from_slice(&value.to_be_bytes());
}

/// A DICT real (`30`, then nibbles), enough for a `FontMatrix`.
fn dict_real(out: &mut Vec<u8>, value: f32) {
    out.push(30);
    // "0.001" -> ".001", "1.25" stays, "0" stays a digit
    let text = format!("{}", value);
    let text = text.strip_prefix('0').filter(|t| t.starts_with('.')).unwrap_or(&text);
    let mut nibbles = Vec::new();
    for c in text.bytes() {
        match c {
            b'.' => nibbles.push(0xa),
            b'-' => nibbles.push(0xe),
            b'e' | b'E' => nibbles.push(0xb),
            b'0'..=b'9' => nibbles.push(c - b'0'),
            _ => {}
        }
    }
    nibbles.push(0xf);
    push_nibbles(out, &nibbles);
}

fn push_nibbles(out: &mut Vec<u8>, nibbles: &[u8]) {
    for pair in nibbles.chunks(2) {
        let high = pair[0] << 4;
        let low = if pair.len() > 1 { pair[1] } else { 0xf };
        out.push(high | low);
    }
}

/// A Type 2 charstring operand (TN5177 §"Charstring data").
fn number(out: &mut Vec<u8>, value: f32) {
    let rounded = value.round();
    if (rounded - value).abs() < 0.0005 && rounded >= -32768.0 && rounded <= 32767.0 {
        integer(out, rounded as i32);
        return;
    }
    out.push(255); // 16.16 fixed
    out.extend_from_slice(&((value * 65536.0).round() as i32).to_be_bytes());
}

fn integer(out: &mut Vec<u8>, value: i32) {
    if (-107..=107).contains(&value) {
        out.push((value + 139) as u8);
    } else if (108..=1131).contains(&value) {
        let v = value - 108;
        out.push(247 + (v / 256) as u8);
        out.push((v % 256) as u8);
    } else if (-1131..=-108).contains(&value) {
        let v = -value - 108;
        out.push(251 + (v / 256) as u8);
        out.push((v % 256) as u8);
    } else if (-32768..=32767).contains(&value) {
        out.push(28);
        out.extend_from_slice(&(value as i16).to_be_bytes());
    } else {
        // nothing this large survives as a shortint; 16.16 fixed is the only wider form
        out.push(255);
        out.extend_from_slice(&((value as f32 * 65536.0).round() as i32).to_be_bytes());
    }
}

const OP_HMOVETO: u8 = 22;
const OP_VMOVETO: u8 = 4;
const OP_RMOVETO: u8 = 21;
const OP_RLINETO: u8 = 5;
const OP_HLINETO: u8 = 6;
const OP_VLINETO: u8 = 7;
const OP_RRCURVETO: u8 = 8;
const OP_ENDCHAR: u8 = 14;

/// One glyph as a Type 2 charstring. Coordinates are rounded to font units and carried as
/// integers, so the deltas do not drift along a long contour.
fn charstring(outline: &Outline, advance: f32, default_width: f32, nominal_width: f32) -> Vec<u8> {
    let mut writer = Charstring { out: Vec::new(), cx: 0, cy: 0 };
    // The width is the first operand of the first stack-clearing operator (TN5177 §3), and
    // that is the first moveto here, or the endchar of a glyph that draws nothing.
    if (advance - default_width).abs() > 0.0005 {
        number(&mut writer.out, advance - nominal_width);
    }
    for contour in outline.contours() {
        if contour.is_empty() {
            continue;
        }
        // A closed contour yields its closing line too, so the pen ends where it started and
        // the implicit close of the next contour adds nothing.
        let segments: Vec<_> = contour.iter(ContourIterFlags::empty()).collect();
        let start = segments.first().map(|s| s.baseline.from()).unwrap_or_else(Vector2F::zero);
        writer.move_to(start);
        for segment in &segments {
            match segment.kind {
                SegmentKind::Line => writer.line_to(segment.baseline.to()),
                SegmentKind::Quadratic => {
                    // Type 2 has no quadratics; the exact cubic for a quadratic control point
                    // is p0 + 2/3 (c - p0) and p2 + 2/3 (c - p2).
                    let p0 = segment.baseline.from();
                    let c = segment.ctrl.from();
                    let p2 = segment.baseline.to();
                    let c1 = p0 + (c - p0) * (2.0 / 3.0);
                    let c2 = p2 + (c - p2) * (2.0 / 3.0);
                    writer.curve_to(c1, c2, p2);
                }
                SegmentKind::Cubic => writer.curve_to(segment.ctrl.from(), segment.ctrl.to(),
                                                      segment.baseline.to()),
                SegmentKind::None => {}
            }
        }
    }
    writer.out.push(OP_ENDCHAR);
    writer.out
}

struct Charstring {
    out: Vec<u8>,
    cx: i32,
    cy: i32,
}

impl Charstring {
    fn move_to(&mut self, to: Vector2F) {
        let (x, y) = (to.x().round() as i32, to.y().round() as i32);
        let (dx, dy) = (x - self.cx, y - self.cy);
        if dy == 0 {
            number(&mut self.out, dx as f32);
            self.out.push(OP_HMOVETO);
        } else if dx == 0 {
            number(&mut self.out, dy as f32);
            self.out.push(OP_VMOVETO);
        } else {
            number(&mut self.out, dx as f32);
            number(&mut self.out, dy as f32);
            self.out.push(OP_RMOVETO);
        }
        self.cx = x;
        self.cy = y;
    }

    fn line_to(&mut self, to: Vector2F) {
        let (x, y) = (to.x().round() as i32, to.y().round() as i32);
        let (dx, dy) = (x - self.cx, y - self.cy);
        if dy == 0 {
            number(&mut self.out, dx as f32);
            self.out.push(OP_HLINETO);
        } else if dx == 0 {
            number(&mut self.out, dy as f32);
            self.out.push(OP_VLINETO);
        } else {
            number(&mut self.out, dx as f32);
            number(&mut self.out, dy as f32);
            self.out.push(OP_RLINETO);
        }
        self.cx = x;
        self.cy = y;
    }

    fn curve_to(&mut self, c1: Vector2F, c2: Vector2F, to: Vector2F) {
        let (c1x, c1y) = (c1.x().round() as i32, c1.y().round() as i32);
        let (c2x, c2y) = (c2.x().round() as i32, c2.y().round() as i32);
        let (x, y) = (to.x().round() as i32, to.y().round() as i32);
        number(&mut self.out, (c1x - self.cx) as f32);
        number(&mut self.out, (c1y - self.cy) as f32);
        number(&mut self.out, (c2x - c1x) as f32);
        number(&mut self.out, (c2y - c1y) as f32);
        number(&mut self.out, (x - c2x) as f32);
        number(&mut self.out, (y - c2y) as f32);
        self.out.push(OP_RRCURVETO);
        self.cx = x;
        self.cy = y;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cff::read_cff;
    use crate::{Font as _, GlyphId};
    use pathfinder_content::outline::{Contour, ContourIterFlags};
    use pathfinder_content::segment::{Segment, SegmentKind};
    use pathfinder_geometry::vector::vec2f;

    fn square(x: f32, y: f32, w: f32, h: f32) -> Contour {
        let mut contour = Contour::new();
        contour.push_endpoint(vec2f(x, y));
        contour.push_endpoint(vec2f(x + w, y));
        contour.push_endpoint(vec2f(x + w, y + h));
        contour.push_endpoint(vec2f(x, y + h));
        contour.close();
        contour
    }

    fn sample() -> FontBuilder {
        let mut builder = FontBuilder::new("Example-Regular", 1000.0);
        builder.add_empty(".notdef", 500.0);

        let mut outline = Outline::new();
        outline.push_contour(square(10.0, 0.0, 400.0, 700.0));
        builder.add_glyph("A", 600.0, outline);

        // a name that is not one of the standard strings, so it needs a String INDEX entry
        let mut outline = Outline::new();
        outline.push_contour(square(0.0, 0.0, 300.0, 300.0));
        builder.add_glyph("mysquare", 350.0, outline);
        builder
    }

    fn round_trip(builder: &FontBuilder) -> Vec<(Outline, f32, f32)> {
        let data = builder.build().unwrap();
        let cff = read_cff(&data).unwrap();
        let slot = cff.slot(0).unwrap();
        slot.outlines().unwrap()
            .map(|glyph| glyph.map(|(outline, width, lsb, _seac)| (outline, width, lsb)))
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
    }

    fn segments(outline: &Outline) -> Vec<Segment> {
        outline.contours()[0].iter(ContourIterFlags::empty()).collect()
    }

    fn close_to(a: Vector2F, b: Vector2F) -> bool {
        (a - b).length() < 1.0
    }

    #[test]
    fn outlines_survive_the_round_trip() {
        let glyphs = round_trip(&sample());
        assert_eq!(glyphs.len(), 3);
        assert_eq!(glyphs[0].1, 500.0);
        assert_eq!(glyphs[1].1, 600.0);
        assert_eq!(glyphs[2].1, 350.0);

        let outline = &glyphs[1].0;
        assert_eq!(outline.contours().len(), 1);
        let bounds = outline.contours()[0].bounds();
        assert_eq!((bounds.min_x(), bounds.min_y()), (10.0, 0.0));
        assert_eq!((bounds.max_x(), bounds.max_y()), (410.0, 700.0));
    }

    #[test]
    fn curves_survive_the_round_trip() {
        let mut builder = FontBuilder::new("Curves", 1000.0);
        builder.add_empty(".notdef", 0.0);

        // a quadratic, which has to come out as the equivalent cubic
        let mut contour = Contour::new();
        contour.push_endpoint(vec2f(0.0, 0.0));
        contour.push_quadratic(vec2f(100.0, 200.0), vec2f(200.0, 0.0));
        contour.close();
        let mut outline = Outline::new();
        outline.push_contour(contour);
        builder.add_glyph("quad", 250.0, outline);

        // and a cubic
        let mut contour = Contour::new();
        contour.push_endpoint(vec2f(0.0, 0.0));
        contour.push_cubic(vec2f(0.0, 300.0), vec2f(300.0, 300.0), vec2f(300.0, 0.0));
        contour.close();
        let mut outline = Outline::new();
        outline.push_contour(contour);
        builder.add_glyph("cubic", 350.0, outline);

        let glyphs = round_trip(&builder);

        // the quadratic becomes a cubic with controls at p0 + 2/3 (c - p0) and p2 + 2/3 (c - p2)
        let quad = segments(&glyphs[1].0);
        assert!(quad.len() >= 2, "the curve and the line back to the start");
        assert!(matches!(quad[0].kind, SegmentKind::Cubic));
        assert!(close_to(quad[0].ctrl.from(), vec2f(66.7, 133.3)), "{:?}", quad[0].ctrl);
        assert!(close_to(quad[0].ctrl.to(), vec2f(133.3, 133.3)), "{:?}", quad[0].ctrl);
        assert!(close_to(quad[0].baseline.to(), vec2f(200.0, 0.0)));
        assert!(matches!(quad[1].kind, SegmentKind::Line));
        assert!(close_to(quad[1].baseline.to(), vec2f(0.0, 0.0)));

        // the cubic keeps its control points
        let cubic = segments(&glyphs[2].0);
        assert!(cubic.len() >= 2);
        assert!(matches!(cubic[0].kind, SegmentKind::Cubic));
        assert!(close_to(cubic[0].ctrl.from(), vec2f(0.0, 300.0)));
        assert!(close_to(cubic[0].ctrl.to(), vec2f(300.0, 300.0)));
        assert!(close_to(cubic[0].baseline.to(), vec2f(300.0, 0.0)));
    }

    #[test]
    fn a_readable_font_comes_out() {
        let data = sample().build().unwrap();
        assert_eq!(&data[..4], &[1, 0, 4, 4], "header");

        let font = crate::CffFont::parse(&data, 0).unwrap();
        assert_eq!(font.num_glyphs(), 3);
        assert_eq!(font.gid_for_name(".notdef"), Some(GlyphId(0)));
        assert_eq!(font.gid_for_name("A"), Some(GlyphId(1)));
        assert_eq!(font.gid_for_name("mysquare"), Some(GlyphId(2)), "custom name");
        assert_eq!(font.gid_for_name("nosuchglyph"), None);

        // the FontMatrix is 1/units_per_em
        assert!((font.font_matrix().m11() - 0.001).abs() < 1e-6);

        let bbox = font.bbox().expect("FontBBox");
        assert_eq!((bbox.min_x(), bbox.min_y()), (0.0, 0.0));
        assert_eq!((bbox.max_x(), bbox.max_y()), (410.0, 700.0));
    }

    #[test]
    fn an_empty_font_is_refused() {
        assert!(FontBuilder::new("Empty", 1000.0).build().is_err());
    }
}
