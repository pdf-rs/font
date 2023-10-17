use std::collections::HashMap;
use nom::{
    bytes::complete::{take},
    number::complete::{be_i16, be_u16, be_u32},
    sequence::{tuple},
};
use crate::{R, GlyphId, FontError};
use crate::parsers::{*};
use crate::opentype::{Maxp, parse_lookup_list, coverage_table};
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
        v => panic!("unsupported GPOS version 1.{}", v)
    };
    
    let mut gpos = GPos::default();
    
    parse_lookup_list(slice!(data, lookup_list_off as usize ..), |lookup_idx, data, lookup_type, _lookup_flag| {
        debug!("lookup type {}", lookup_type);
        match lookup_type { 
            2 => parse_pair_adjustment(data, &mut gpos.kern, maxp.num_glyphs)?,
            4 => parse_mark_to_base_attachment(data, &mut gpos)?,
            _ => {}
        }
        Ok(())
    })?;
    
    Ok(gpos)
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
            let (i, _pair_set_offsets) = be_u16(i)?;
            
            let mut coverage = coverage_table(coverage_off.of(data).unwrap())?;
            for offset in iterator_n(i, be_u16, pair_set_count) {
                let first_glyph = coverage.next().unwrap();
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
            let (i, _class_def_1_offset) = be_u16(i)?;
            let (i, _class_def_2_offset) = be_u16(i)?;
            let (i, class_1_count) = be_u16(i)?;
            let (i, class_2_count) = be_u16(i)?;
            
            let iter = (0 .. class_1_count).cartesian_product(0 .. class_2_count)
                .zip(iterator_n(i, tuple((value_record(value_format_1), value_record(value_format_2))), class_1_count * class_2_count));
            
            kern.class_pairs.reserve(class_1_count as usize * class_2_count as usize);

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
        require!(mark_class < mark_class_count);
        for (base_nr, &base_gid) in base_coverage.iter().enumerate() {
            let base_anchor = base_array.get(base_nr * mark_class_count as usize + mark_class as usize)?;
            gpos.mark_to_base.insert((base_gid, mark_gid), (base_anchor.x - mark_anchor.x, base_anchor.y - mark_anchor.y));
        }
    }

    Ok(())
}

fn parse_base_array(data: &[u8], mark_class_count: u16) -> Result<impl Array<Item=AnchorTable> + '_, FontError> {
    let (i, base_count) = be_u16(data)?;
    let (_, arr) = array::<uint16, _>(mark_class_count as usize * base_count as usize)(i)?;

    Ok(arr.map(move |off: u16| AnchorTable::parse(slice!(data, off as usize..))))
}
