use std::collections::HashMap;
use nom::{
    bytes::complete::{take},
    number::complete::{be_i16, be_u16, be_u32},
    sequence::{tuple},
};
use crate::{R, GlyphId, FontError};
use crate::parsers::{*};
use crate::opentype::{Maxp, parse_extension, parse_lookup_list, coverage_table, parse_class_def};
use itertools::{Itertools};


#[derive(Default, Clone)]
pub struct GPos {
    pub kern: KernTable,
    pub mark_to_base: HashMap<(u16, u16), (i16, i16)>,
    /// OT §GPOS "Single Adjustment" (lookup type 1): the value record of one glyph.
    pub single: HashMap<u16, ValueRecord>,
    /// OT §GPOS "Cursive Attachment" (3): entry and exit anchor of one glyph, in font units.
    pub cursive: HashMap<u16, ((i16, i16), (i16, i16))>,
    /// OT §GPOS "Mark-to-Ligature Attachment" (5): (ligature, mark) -> offset, taken from the
    /// first component of the ligature.
    pub mark_to_ligature: HashMap<(u16, u16), (i16, i16)>,
    /// OT §GPOS "Mark-to-Mark Attachment" (6): (base mark, mark) -> offset.
    pub mark_to_mark: HashMap<(u16, u16), (i16, i16)>,
}
impl GPos {
    pub fn from_kern(kern: KernTable) -> GPos {
        GPos {
            kern,
            ..GPos::default()
        }
    }
    pub fn get_mark_to_base(&self, base: GlyphId, mark: GlyphId) -> Option<(i16, i16)> {
        self.mark_to_base.get(&(glyph_key(base)?, glyph_key(mark)?)).cloned()
    }
    /// OT §GPOS "Single Adjustment": placement and advance adjustment of one glyph.
    pub fn get_single(&self, glyph: GlyphId) -> Option<ValueRecord> {
        self.single.get(&glyph_key(glyph)?).cloned()
    }
    /// OT §GPOS "Cursive Attachment": `(entry, exit)` anchors of one glyph.
    pub fn get_cursive(&self, glyph: GlyphId) -> Option<((i16, i16), (i16, i16))> {
        self.cursive.get(&glyph_key(glyph)?).cloned()
    }
    /// OT §GPOS "Mark-to-Ligature Attachment": where the mark is placed relative to the first
    /// component of the ligature.
    pub fn get_mark_to_ligature(&self, ligature: GlyphId, mark: GlyphId) -> Option<(i16, i16)> {
        self.mark_to_ligature.get(&(glyph_key(ligature)?, glyph_key(mark)?)).cloned()
    }
    /// OT §GPOS "Mark-to-Mark Attachment": where a mark is placed relative to another mark.
    pub fn get_mark_to_mark(&self, base: GlyphId, mark: GlyphId) -> Option<(i16, i16)> {
        self.mark_to_mark.get(&(glyph_key(base)?, glyph_key(mark)?)).cloned()
    }
}

/// The tables are keyed by u16 glyph ids; a larger id cannot be in one.
fn glyph_key(glyph: GlyphId) -> Option<u16> {
    if glyph.0 > u16::MAX as u32 { None } else { Some(glyph.0 as u16) }
}


#[derive(Default, Clone)]
pub struct KernTable {
    pub left_classes: HashMap<u16, u16>, // gid -> class id,
    pub right_classes: HashMap<u16, u16>, // gid -> class id,
    pub class_pairs: HashMap<(u16, u16), i16>,
    pub glyph_pairs: HashMap<(u16, u16), i16>
}
impl KernTable {
    pub fn get(&self, left: u16, right: u16) -> Option<i16> {
        if let Some(&kern) = self.glyph_pairs.get(&(left, right)) {
            return Some(kern);
        }
        let &left_class = self.left_classes.get(&left)?;
        let &right_class = self.right_classes.get(&right)?;
        let &kern = self.class_pairs.get(&(left_class, right_class))?;
        Some(kern)
    }
}

// figure out how to replace with a dedicated sparse 2d map
pub fn parse_gpos<'a>(data: &'a [u8], maxp: &Maxp) -> Result<GPos, FontError> {
    debug!("parse GPOS");
    let (i, major_version) = be_u16(data)?;
    require_eq!(major_version, 1);
    let (i, minor_version) = be_u16(i)?;
    
    let (i, _script_list_off) = be_u16(i)?;
    let (i, _feature_list_off) = be_u16(i)?;
    let (i, lookup_list_off) = be_u16(i)?;
    
    let (i, _feature_variations_offset) = match minor_version {
        0 => (i, 0),
        1 => be_u32(i)?,
        // OT §GPOS: a minor version we do not know may carry fields we cannot skip, but the
        // tables we do read are laid out the same way, so ignore the rest instead of failing.
        v => {
            warn!("unsupported GPOS version 1.{}, ignoring the rest of the header", v);
            (i, 0)
        }
    };
    
    let mut gpos = GPos::default();
    
    parse_lookup_list(slice!(data, lookup_list_off as usize ..), |lookup_idx, data, lookup_type, _lookup_flag| {
        debug!("lookup type {}", lookup_type);
        let _ = lookup_idx;
        parse_positioning(data, lookup_type, &mut gpos, maxp.num_glyphs)
    })?;
    
    Ok(gpos)
}

/// One subtable of a `GPOS` lookup. Single adjustment (1), pair adjustment (2), cursive (3),
/// mark-to-base (4), mark-to-ligature (5) and mark-to-mark (6) are read; the contextual
/// lookups (7, 8) are not, and `Extension` (9) wraps the ones that are, which is what large
/// fonts use.
fn parse_positioning(data: &[u8], lookup_type: u16, gpos: &mut GPos, num_glyphs: u16) -> Result<(), FontError> {
    match lookup_type {
        1 => parse_single_adjustment(data, gpos)?,
        2 => parse_pair_adjustment(data, &mut gpos.kern, num_glyphs)?,
        3 => parse_cursive(data, gpos)?,
        4 => parse_mark_attachment(data, &mut gpos.mark_to_base)?,
        5 => parse_mark_to_ligature_attachment(data, gpos)?,
        6 => parse_mark_attachment(data, &mut gpos.mark_to_mark)?,
        // OT §GPOS "Extension Positioning": a wrapper around another lookup type.
        9 => {
            let (inner_type, inner) = parse_extension(data)?;
            if inner_type == 9 {
                // OT §GPOS "Extension Positioning": extensions may not wrap extensions.
                warn!("GPOS: an Extension lookup may not point at another Extension lookup");
                return Ok(());
            }
            parse_positioning(inner, inner_type, gpos, num_glyphs)?;
        }
        _ => {}
    }
    Ok(())
}


/// OT §GPOS "Value Records": a placement and an advance adjustment per axis; the value format
/// says which of the four are present. `Default` is "no adjustment", which is also what a
/// missing field means.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ValueRecord {
    pub x_placement: i16,
    pub y_placement: i16,
    pub x_advance: i16,
    pub y_advance: i16,
}

fn value_record(flags: u16) -> impl Fn(&[u8]) -> R<ValueRecord> {
    move |mut i: &[u8]| {
        let mut record = ValueRecord::default();
        if flags & 0x0001 != 0 {
            record.x_placement = parse(&mut i, be_i16)?;
        }
        if flags & 0x0002 != 0 {
            record.y_placement = parse(&mut i, be_i16)?;
        }
        if flags & 0x0004 != 0 {
            record.x_advance = parse(&mut i, be_i16)?;
        }
        if flags & 0x0008 != 0 {
            record.y_advance = parse(&mut i, be_i16)?;
        }
        let (i, _) = take((flags & 0x00F0).count_ones() * 2)(i)?;
        Ok((i, record))
    }
}

fn parse_pair_adjustment<'a>(data: &'a [u8], kern: &mut KernTable, _num_glyphs: u16) -> Result<(), FontError> {
    let (i, format) = be_u16(data)?;
    match format {
        1 => {
            let (i, coverage_off) = offset(i)?;
            let (i, value_format_1) = be_u16(i)?;
            let (i, value_format_2) = be_u16(i)?;
            let (i, pair_set_count) = be_u16(i)?;

            let mut coverage = coverage_table(coverage_off.of(data)?)?;
            for offset in iterator_n(i, be_u16, pair_set_count) {
                let first_glyph = match coverage.next() {
                    Some(glyph) => glyph,
                    // fewer coverage entries than pair sets: ignore the rest
                    None => break
                };
                let i = slice!(data, offset as usize ..);
                let (i, pair_value_count) = be_u16(i)?;
                
                let iter = iterator_n(i, tuple((be_u16, value_record(value_format_1), value_record(value_format_2))), pair_value_count);
                for (second_glyph, value_record_1, _value_record_2) in iter {
                    //debug!("{:?}/{}: {:?} {:?}", first_glyph, second_glyph, value_record_1, value_record_2);
                    kern.glyph_pairs.insert((first_glyph, second_glyph), value_record_1.x_advance);
                }
            }
        },
        2 => {
            let (i, _coverage_off) = be_u16(i)?;
            let (i, value_format_1) = be_u16(i)?;
            let (i, value_format_2) = be_u16(i)?;
            let (i, class_def_1_offset) = be_u16(i)?;
            let (i, class_def_2_offset) = be_u16(i)?;
            let (i, class_1_count) = be_u16(i)?;
            let (i, class_2_count) = be_u16(i)?;

            // The class matrix is indexed by the two ClassDefs; without them no glyph ever gets a
            // class and `KernTable::get` can never reach `class_pairs`.
            parse_class_def(Offset(class_def_1_offset).of(data)?, &mut kern.left_classes)?;
            parse_class_def(Offset(class_def_2_offset).of(data)?, &mut kern.right_classes)?;


            // `class_1_count * class_2_count` overflows u16 (300 x 300 classes is a panic in
            // debug builds), and a bogus count would let `iterator_n` run for billions of
            // iterations and `reserve` allocate gigabytes. A matrix cannot hold more values
            // than the subtable has bytes.
            let total = usize::from(class_1_count) * usize::from(class_2_count);
            let total = total.min(data.len());
            kern.class_pairs.reserve(total.min(1 << 16));

            let iter = (0 .. class_1_count).cartesian_product(0 .. class_2_count)
                .zip(iterator_n(i, tuple((value_record(value_format_1), value_record(value_format_2))), total));

            for ((class_1, class_2), (value_record_1, _value_record_2)) in iter {
                if value_record_1.x_advance != 0 {
                    kern.class_pairs.insert((class_1, class_2), value_record_1.x_advance);
                }
            }
        }
        n => error!("unsupported pair adjustment format {}", n)
    }
    Ok(())
}

parser!(int16 : be_i16 -> i16);
parser!(uint16 : be_u16 -> u16);

table!(MarkAnchorOff {
    uint16 mark_class,
    uint16 mark_anchor_off,
});

table!(AnchorTable {
    uint16 format,
    int16 x,
    int16 y,
});

// (mark class, (x, y))
fn parse_mark_array_table(data: &[u8]) -> Result<impl Array<Item=(u16, AnchorTable)> + '_, FontError> {
    let (i, mark_count) = be_u16(data)?;
    let (_, array) = array::<MarkAnchorOff, _>(mark_count)(i)?;
    let array = array.map(move |off: MarkAnchorOff| {
        let anchor = AnchorTable::parse(offset!(data, off.mark_anchor_off))?;
        Ok((off.mark_class, anchor))
    });
    Ok(array)
}

/// OT §GPOS "Mark-to-Base Attachment" (4) and "Mark-to-Mark Attachment" (6), format 1: the two
/// subtables have the same layout, so this fills either map with `base`/`mark` anchor deltas.
fn parse_mark_attachment(data: &[u8], marks: &mut HashMap<(u16, u16), (i16, i16)>) -> Result<(), FontError> {
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);
    let (i, mark_coverage_offset) = be_u16(i)?;
    let (i, base_coverage_offset) = be_u16(i)?;
    let (i, mark_class_count) = be_u16(i)?;
    let (i, mark_array_offset) = be_u16(i)?;
    let (i, base_array_offset) = be_u16(i)?;

    let mark_coverage = coverage_table(slice!(data, mark_coverage_offset as usize..))?;
    let base_coverage: Vec<_> = coverage_table(slice!(data, base_coverage_offset as usize..))?.collect();

    let base_array = parse_base_array(slice!(data, base_array_offset as usize..), mark_class_count)?;

    let mark_array = parse_mark_array_table(slice!(data, mark_array_offset as usize ..))?;
    for (mark_gid, m) in mark_coverage.zip(mark_array.iter()) {
        let (mark_class, mark_anchor) = m?;
        if mark_class >= mark_class_count {
            // A mark whose class the base array does not have: the font is malformed, but the
            // other marks are still usable, so skip this one instead of failing the whole table
            // (which used to drop kerning and mark attachment for the entire font).
            warn!("GPOS: mark glyph {} has class {} but markClassCount is {}",
                  mark_gid, mark_class, mark_class_count);
            continue;
        }
        for (base_nr, &base_gid) in base_coverage.iter().enumerate() {
            let index = base_nr * usize::from(mark_class_count) + usize::from(mark_class);
            let base_anchor = match base_array.get(index) {
                Ok(anchor) => anchor,
                // Fewer anchors than baseCount x markClassCount: skip what is missing.
                Err(_) => continue,
            };
            marks.insert((base_gid, mark_gid), (base_anchor.x - mark_anchor.x, base_anchor.y - mark_anchor.y));
        }
    }

    Ok(())
}

fn parse_base_array(data: &[u8], mark_class_count: u16) -> Result<impl Array<Item=AnchorTable> + '_, FontError> {
    let (i, base_count) = be_u16(data)?;
    let (_, arr) = array::<uint16, _>(usize::from(mark_class_count) * usize::from(base_count))(i)?;

    Ok(arr.map(move |off: u16| AnchorTable::parse(slice!(data, off as usize..))))
}

/// OT §GPOS "Single Adjustment" (lookup type 1), formats 1 and 2: one value record per glyph,
/// either the same for every covered glyph or one each.
fn parse_single_adjustment(data: &[u8], gpos: &mut GPos) -> Result<(), FontError> {
    let (i, format) = be_u16(data)?;
    let (i, coverage_offset) = be_u16(i)?;
    let (i, value_format) = be_u16(i)?;
    let coverage = coverage_table(slice!(data, coverage_offset as usize ..))?;
    match format {
        1 => {
            let (_, value) = value_record(value_format)(i)?;
            for glyph in coverage {
                gpos.single.insert(glyph, value.clone());
            }
        }
        2 => {
            let (i, value_count) = be_u16(i)?;
            for (glyph, value) in coverage.zip(iterator_n(i, value_record(value_format), value_count)) {
                gpos.single.insert(glyph, value);
            }
        }
        n => warn!("GPOS: unsupported single adjustment format {}, ignoring it", n),
    }
    Ok(())
}

/// OT §GPOS "Cursive Attachment" (lookup type 3), format 1: an entry and an exit anchor per
/// glyph. A NULL offset means the glyph has no such anchor, which is stored as `(0, 0)`.
fn parse_cursive(data: &[u8], gpos: &mut GPos) -> Result<(), FontError> {
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);
    let (i, coverage_offset) = be_u16(i)?;
    let (i, entry_exit_count) = be_u16(i)?;
    let coverage = coverage_table(slice!(data, coverage_offset as usize ..))?;
    for (glyph, (entry_offset, exit_offset)) in coverage.zip(iterator_n(i, tuple((be_u16, be_u16)), entry_exit_count)) {
        let entry = match Offset(entry_offset).of_opt(data) {
            Some(anchor) => anchor_coordinates(anchor)?,
            None => (0, 0),
        };
        let exit = match Offset(exit_offset).of_opt(data) {
            Some(anchor) => anchor_coordinates(anchor)?,
            None => (0, 0),
        };
        gpos.cursive.insert(glyph, (entry, exit));
    }
    Ok(())
}

/// OT §GPOS "Anchor Tables": only the `(x, y)` pair is used. Formats 2 and 3 carry a contour
/// point as well, but the coordinates are the same leading fields.
fn anchor_coordinates(data: &[u8]) -> Result<(i16, i16), FontError> {
    let anchor = AnchorTable::parse(data)?;
    Ok((anchor.x, anchor.y))
}

/// OT §GPOS "Mark-to-Ligature Attachment" (lookup type 5), format 1. A ligature can carry an
/// anchor per component; this crate keeps the first component's, which is where a mark after
/// the whole ligature goes.
fn parse_mark_to_ligature_attachment(data: &[u8], gpos: &mut GPos) -> Result<(), FontError> {
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);
    let (i, mark_coverage_offset) = be_u16(i)?;
    let (i, ligature_coverage_offset) = be_u16(i)?;
    let (i, mark_class_count) = be_u16(i)?;
    let (i, mark_array_offset) = be_u16(i)?;
    let (i, ligature_array_offset) = be_u16(i)?;

    let mark_coverage = coverage_table(slice!(data, mark_coverage_offset as usize ..))?;
    let ligature_coverage: Vec<u16> = coverage_table(slice!(data, ligature_coverage_offset as usize ..))?.collect();
    let mark_array = parse_mark_array_table(slice!(data, mark_array_offset as usize ..))?;

    // The marks with their class and anchor, once, instead of once per ligature.
    let mut marks = Vec::new();
    for (mark_gid, entry) in mark_coverage.zip(mark_array.iter()) {
        let (mark_class, mark_anchor) = entry?;
        if mark_class >= mark_class_count {
            warn!("GPOS: mark glyph {} has class {} but markClassCount is {}",
                  mark_gid, mark_class, mark_class_count);
            continue;
        }
        marks.push((mark_gid, mark_class, (mark_anchor.x, mark_anchor.y)));
    }

    let ligature_array = slice!(data, ligature_array_offset as usize ..);
    let (i, ligature_count) = be_u16(ligature_array)?;
    for (&ligature_gid, attach_offset) in ligature_coverage.iter().zip(iterator_n(i, be_u16, ligature_count)) {
        let attach = slice!(ligature_array, attach_offset as usize ..);
        if attach.len() < 2 {
            continue;
        }
        let (i, component_count) = be_u16(attach)?;
        if component_count == 0 {
            warn!("GPOS: ligature {} has componentCount 0, skipping it", ligature_gid);
            continue;
        }
        // The first ComponentRecord: one anchor offset per mark class.
        let anchors = match array::<uint16, _>(usize::from(mark_class_count))(i) {
            Ok((_, anchors)) => anchors,
            Err(_) => continue,
        };
        for &(mark_gid, mark_class, (mark_x, mark_y)) in &marks {
            let anchor = match anchors.get(usize::from(mark_class)) {
                // A NULL offset: no anchor for this class.
                Ok(0) => continue,
                Ok(offset) => match AnchorTable::parse(slice!(attach, offset as usize ..)) {
                    Ok(anchor) => anchor,
                    Err(_) => continue,
                },
                Err(_) => continue,
            };
            gpos.mark_to_ligature.insert(
                (ligature_gid, mark_gid),
                (anchor.x - mark_x, anchor.y - mark_y),
            );
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extension_lookup_reaches_pair_adjustment() {
        // OT §GPOS "Extension Positioning" (lookup type 9) around a PairPos format 1:
        // glyph 20 followed by 21 gets xAdvance -40.
        let mut data = Vec::new();
        data.extend_from_slice(&[0, 1]);       // extension format 1
        data.extend_from_slice(&[0, 2]);       // extensionLookupType = Pair Adjustment
        data.extend_from_slice(&[0, 0, 0, 8]); // extensionOffset
        data.extend_from_slice(&[0, 1]);       // PairPos format 1
        data.extend_from_slice(&[0, 18]);      // coverageOffset (from the subtable start)
        data.extend_from_slice(&[0, 4]);       // valueFormat1 = X_ADVANCE
        data.extend_from_slice(&[0, 0]);       // valueFormat2
        data.extend_from_slice(&[0, 1]);       // pairSetCount
        data.extend_from_slice(&[0, 12]);      // pairSetOffset[0]
        data.extend_from_slice(&[0, 1]);       // pairValueCount
        data.extend_from_slice(&[0, 21]);      // secondGlyph
        data.extend_from_slice(&(-40i16).to_be_bytes()); // xAdvance
        data.extend_from_slice(&[0, 1]);       // Coverage format 1
        data.extend_from_slice(&[0, 1]);       // glyphCount
        data.extend_from_slice(&[0, 20]);      // glyph

        let mut gpos = GPos::default();
        parse_positioning(&data, 9, &mut gpos, 100).expect("extension positioning parses");
        assert_eq!(gpos.kern.get(20, 21), Some(-40));
    }

    fn words_to_bytes(words: &[u16]) -> Vec<u8> {
        words.iter().flat_map(|w| w.to_be_bytes().to_vec()).collect()
    }

    #[test]
    fn pair_adjustment_with_huge_class_matrix_does_not_overflow() {
        // `class1Count * class2Count` in u16 overflowed for 300 x 300 classes. The value
        // formats are 0, so the matrix carries no data at all.
        let words: [u16; 14] = [
            2, 0,                 // PairPos format 2, coverage offset (unused)
            0, 0,                 // valueFormat1, valueFormat2
            16, 22,               // classDef1Offset, classDef2Offset
            300, 300,             // class1Count, class2Count
            1, 0, 0,              // classDef1: format 1, startGlyph 0, glyphCount 0
            1, 0, 0,              // classDef2
        ];
        let data = words_to_bytes(&words);
        let mut kern = KernTable::default();
        parse_pair_adjustment(&data, &mut kern, 1000).expect("pair adjustment parses");
        assert!(kern.class_pairs.is_empty());
    }

    #[test]
    fn mark_in_an_unknown_class_is_skipped_not_fatal() {
        // MarkToBaseFormat1 with markClassCount 1. Mark glyph 100 is in class 0 and attaches
        // to base 200 at (30,40) - (10,10) = (20,30); mark 101 claims class 5, which the base
        // array does not have and used to make the whole GPOS table fail to load.
        let words: [u16; 29] = [
            1, 44, 52, 1, 12, 34, // format, markCov, baseCov, markClassCount, markArray, baseArray
            2, 0, 10, 5, 16,      // MarkArray: 2 marks; class 0 -> +10, class 5 -> +16
            1, 10, 10,            // mark anchor: format 1, (10,10)
            1, 20, 20,            // mark anchor: format 1, (20,20)
            1, 4,                 // BaseArray: baseCount 1, anchor offset +4
            1, 30, 40,            // base anchor: format 1, (30,40)
            1, 2, 100, 101,       // mark coverage
            1, 1, 200,            // base coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 4, &mut gpos, 1000).expect("mark-to-base parses");
        assert_eq!(gpos.get_mark_to_base(GlyphId(200), GlyphId(100)), Some((20, 30)));
        assert_eq!(gpos.get_mark_to_base(GlyphId(200), GlyphId(101)), None);
    }

    #[test]
    fn single_adjustment_format2_reads_one_record_per_glyph() {
        // OT §GPOS "Single Adjustment" (1), format 2: glyph 30 gets xAdvance -10, glyph 31 +25.
        let words: [u16; 10] = [
            2, 12, 4, 2,          // format, coverageOffset, valueFormat = X_ADVANCE, valueCount
            (-10i16) as u16, 25,  // the two value records
            1, 2, 30, 31,         // coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 1, &mut gpos, 100).expect("single adjustment parses");
        assert_eq!(gpos.get_single(GlyphId(30)).map(|v| v.x_advance), Some(-10));
        assert_eq!(gpos.get_single(GlyphId(31)).map(|v| v.x_advance), Some(25));
        // A glyph outside the coverage has no adjustment.
        assert!(gpos.get_single(GlyphId(32)).is_none());
    }

    #[test]
    fn cursive_attachment_reads_entry_and_exit_anchors() {
        // OT §GPOS "Cursive Attachment" (3), format 1: glyph 40 enters at (5, 6) and exits at
        // (7, 8).
        let words: [u16; 14] = [
            1, 10, 1,       // format, coverageOffset, entryExitCount
            16, 22,         // entryAnchorOffset, exitAnchorOffset
            1, 1, 40,       // coverage
            1, 5, 6,        // entry anchor
            1, 7, 8,        // exit anchor
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 3, &mut gpos, 100).expect("cursive attachment parses");
        assert_eq!(gpos.get_cursive(GlyphId(40)), Some(((5, 6), (7, 8))));
    }

    #[test]
    fn mark_to_mark_attachment_fills_its_own_map() {
        // OT §GPOS "Mark-to-Mark Attachment" (6), format 1 — the same layout as mark-to-base:
        // mark 100 attaches to mark 200 at (30,40) - (10,10) = (20,30).
        let words: [u16; 23] = [
            1, 34, 40, 1, 12, 24, // format, mark1Cov, mark2Cov, classCount, mark1Array, mark2Array
            1, 0, 6,              // MarkArray: one mark, class 0, anchor at +6
            1, 10, 10,            // mark anchor (10,10)
            1, 4,                 // Mark2Array: one mark, anchor at +4
            1, 30, 40,            // mark2 anchor (30,40)
            1, 1, 100,            // mark1 coverage
            1, 1, 200,            // mark2 coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 6, &mut gpos, 100).expect("mark-to-mark parses");
        assert_eq!(gpos.get_mark_to_mark(GlyphId(200), GlyphId(100)), Some((20, 30)));
        // Mark-to-mark must not end up in the mark-to-base map.
        assert!(gpos.get_mark_to_base(GlyphId(200), GlyphId(100)).is_none());
    }

    #[test]
    fn mark_to_ligature_attachment_uses_the_first_component() {
        // OT §GPOS "Mark-to-Ligature Attachment" (5), format 1: ligature 200 has one component
        // whose class-0 anchor is (30,40); mark 100 attaches at (30,40) - (10,10) = (20,30).
        let words: [u16; 25] = [
            1, 38, 44, 1, 12, 24, // format, markCov, ligatureCov, classCount, markArray, ligatureArray
            1, 0, 6,              // MarkArray: one mark, class 0, anchor at +6
            1, 10, 10,            // mark anchor (10,10)
            1, 4,                 // LigatureArray: one ligature, attach table at +4
            1, 4,                 // LigatureAttach: one component, class-0 anchor at +4
            1, 30, 40,            // component anchor (30,40)
            1, 1, 100,            // mark coverage
            1, 1, 200,            // ligature coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 5, &mut gpos, 100).expect("mark-to-ligature parses");
        assert_eq!(gpos.get_mark_to_ligature(GlyphId(200), GlyphId(100)), Some((20, 30)));
    }
}
