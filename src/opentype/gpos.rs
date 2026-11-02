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
    pub mark_to_base: HashMap<(u16, u16), (i16, i16)>
}
impl GPos {
    pub fn from_kern(kern: KernTable) -> GPos {
        GPos {
            kern,
            mark_to_base: HashMap::new()
        }
    }
    pub fn get_mark_to_base(&self, base: GlyphId, mark: GlyphId) -> Option<(i16, i16)> {
        self.mark_to_base.get(&(base.0 as u16, mark.0 as u16)).cloned()
    }
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

/// One subtable of a `GPOS` lookup. Lookup types 1 (single), 3 (cursive), 5-8 (mark
/// attachment, contextual and friends) are not implemented; `Extension` wraps the ones that
/// are, which is what large fonts use.
fn parse_positioning(data: &[u8], lookup_type: u16, gpos: &mut GPos, num_glyphs: u16) -> Result<(), FontError> {
    match lookup_type {
        2 => parse_pair_adjustment(data, &mut gpos.kern, num_glyphs)?,
        4 => parse_mark_to_base_attachment(data, gpos)?,
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


#[derive(Default, Debug, Clone)]
struct ValueRecord {
    x_placement: i16,
    y_placement: i16,
    x_advance: i16,
    y_advance: i16
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

fn parse_mark_to_base_attachment<'a>(data: &'a [u8], gpos: &mut GPos) -> Result<(), FontError> {
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
            gpos.mark_to_base.insert((base_gid, mark_gid), (base_anchor.x - mark_anchor.x, base_anchor.y - mark_anchor.y));
        }
    }

    Ok(())
}

fn parse_base_array(data: &[u8], mark_class_count: u16) -> Result<impl Array<Item=AnchorTable> + '_, FontError> {
    let (i, base_count) = be_u16(data)?;
    let (_, arr) = array::<uint16, _>(usize::from(mark_class_count) * usize::from(base_count))(i)?;

    Ok(arr.map(move |off: u16| AnchorTable::parse(slice!(data, off as usize..))))
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
}
