use std::collections::HashMap;
use nom::{
    bytes::complete::{take},
    number::complete::{be_i16, be_u16, be_u32},
    sequence::{tuple},
};
use crate::{R};
use crate::parsers::{iterator_n, parse};
use crate::opentype::Maxp;
use itertools::{Itertools, Either};

fn parse_skript_list(data: &[u8]) -> R<()> {
    let (i, script_count) = be_u16(data)?;
    
    for (tag, offset) in iterator_n(i, tuple((take(4usize), be_u16)), script_count) {
        debug!("script {}", String::from_utf8_lossy(tag));
        let script_data = &data[offset as usize ..];
        
        let (i, default_lang_sys_off) = be_u16(script_data)?;
        let (i, sys_lang_count) = be_u16(i)?;
        
        for (tag, offset) in iterator_n(i, tuple((take(4usize), be_u16)), sys_lang_count) {
            let i = &script_data[offset as usize ..];
            let (i, _lookup_order) = be_u16(i)?;
            let (i, required_feature_index) = be_u16(i)?;
            let (i, feature_index_count) = be_u16(i)?;
            for feature_index in iterator_n(i, be_u16, feature_index_count) {
            
            }
        }
    }
    Ok((i, ()))
}

fn parse_lookup_list(data: &[u8], mut inner: impl FnMut(&[u8], LookupType, u16) -> R<()>) -> R<()> {
    let (i, lookup_count) = be_u16(data)?;
    for table_off in iterator_n(i, be_u16, lookup_count) {
        let table_data = &data[table_off as usize ..];
        let (i, lookup_type) = be_u16(table_data)?;
        let (i, lookup_flag) = be_u16(i)?;
        let (i, subtable_count) = be_u16(i)?;
        
        for subtable_off in iterator_n(i, be_u16, subtable_count) {
            if let Some(lookup_type) = LookupType::from_u16(lookup_type) {
                inner(&table_data[subtable_off as usize ..], lookup_type, lookup_flag)?;
            }
        }
    }
    Ok((i, ()))
}

// maps gid -> class id
fn parse_class_def(data: &[u8]) -> R<HashMap<u16, u16>> {
    let (i, format) = be_u16(data)?;
    let mut map = HashMap::new();
    match format {
        1 => {
            let (i, start_glyph_id) = be_u16(i)?;
            let (i, glyph_count) = be_u16(i)?;
            for (gid, class) in (start_glyph_id ..).zip(iterator_n(i, be_u16, glyph_count)) {
                map.insert(gid, class);
            }
        }
        2 => {
            let (i, class_rage_count) = be_u16(i)?;
            for (start_gid, end_gid, class) in iterator_n(i, tuple((be_u16, be_u16, be_u16)), class_rage_count) {
                for gid in start_gid ..= end_gid {
                    map.insert(gid, class);
                }
            }
        }
        f => panic!("invalid class list format {}", f)
    }
    Ok((i, map))
}
fn invert_class_def(map: &HashMap<u16, u16>, num_glyphs: u16) -> HashMap<u16, Vec<u16>> {
    let mut map2 = HashMap::new();
    for gid in 0 .. num_glyphs {
        let class = map.get(&gid).cloned().unwrap_or(0);
        map2.entry(class).or_insert(vec![]).push(gid);
    }
    map2
}

#[derive(Debug)]
enum LookupType {
    SingleAdjustment,
    PairAdjustment,
    CursiveAttachment,
    MarkToBaseAttachment,
    MarkToLigatureAttachment,
    MarkToMarkAttachment,
    ContextPositioning,
    ChainedContextPositioning,
    ExtensionPositioning,
}
impl LookupType {
    fn from_u16(n: u16) -> Option<LookupType> {
        use LookupType::*;
        let t = match n {
            1 => SingleAdjustment,
            2 => PairAdjustment,
            3 => CursiveAttachment,
            4 => MarkToBaseAttachment,
            5 => MarkToLigatureAttachment,
            6 => MarkToMarkAttachment,
            7 => ContextPositioning,
            8 => ChainedContextPositioning,
            9 => ExtensionPositioning,
            _ => return None
        };
        Some(t)
    }
}

#[derive(Default)]
pub struct Gpos {
    pub kern: HashMap<(u16, u16), i16>
}

// figure out how to replace with a dedicated sparse 2d map
pub fn parse_gpos<'a>(data: &'a [u8], maxp: &Maxp) -> R<'a, Gpos> {
    debug!("parse gpos");
    let (i, major_version) = be_u16(data)?;
    assert_eq!(major_version, 1);
    let (i, minor_version) = be_u16(i)?;
    
    let (i, script_list_off) = be_u16(i)?;
    let (i, feature_list_off) = be_u16(i)?;
    let (i, lookup_list_off) = be_u16(i)?;
    
    let (i, feature_variations_offset) = match minor_version {
        0 => (i, 0),
        1 => be_u32(i)?,
        v => panic!("unsupported GPOS version 1.{}", v)
    };
    
    let mut gpos = Gpos::default();
    
    parse_lookup_list(&data[lookup_list_off as usize ..], |data, lookup_type, lookup_flag| {
        match lookup_type { 
            LookupType::PairAdjustment => parse_pair_adjustment(data, &mut gpos.kern, maxp.num_glyphs)?.1,
            _ => {}
        }
        Ok((data, ()))
    });
    
    Ok((i, gpos))
}

fn coverage_table<'a>(i: &'a [u8]) -> R<impl Iterator<Item=u16> + 'a> {
    let (i, format) = be_u16(i)?;
    debug!("coverage table format {}", format);
    match format {
        1 => {
            let (i, glyph_count) = be_u16(i)?;
            Ok((i, Either::Left(iterator_n(i, be_u16, glyph_count))))
        },
        2 => {
            let (i, range_count) = be_u16(i)?;
            Ok((i, Either::Right(
                iterator_n(i, tuple((be_u16, be_u16, be_u16)), range_count)
                    .flat_map(|(start, end, i)| start ..= end)
            )))
        },
        n => panic!("invalid coverage format {}", n)
    }
}

#[derive(Default, Debug)]
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

fn parse_pair_adjustment<'a>(data: &'a [u8], kern: &mut HashMap<(u16, u16), i16>, num_glyphs: u16) -> R<'a, ()> {
    let (i, format) = be_u16(data)?;
    match format {
        1 => {
            let (i, coverage_off) = be_u16(i)?;
            let (i, value_format_1) = be_u16(i)?;
            let (i, value_format_2) = be_u16(i)?;
            let (i, pair_set_count) = be_u16(i)?;
            let (i, pair_set_offsets) = be_u16(i)?;
            
            let mut coverage = coverage_table(&data[coverage_off as usize ..])?.1;
            for offset in iterator_n(i, be_u16, pair_set_count) {
                let first_glyph = coverage.next().unwrap();
                let i = &data[offset as usize ..];
                let (i, pair_value_count) = be_u16(i)?;
                
                let iter = iterator_n(i, tuple((be_u16, value_record(value_format_1), value_record(value_format_2))), pair_value_count);
                for (second_glyph, value_record_1, value_record_2) in iter {
                    //debug!("{:?}/{}: {:?} {:?}", first_glyph, second_glyph, value_record_1, value_record_2);
                    kern.insert((first_glyph, second_glyph), value_record_1.x_advance);
                }
            }
        },
        2 => {
            let (i, coverage_off) = be_u16(i)?;
            let (i, value_format_1) = be_u16(i)?;
            let (i, value_format_2) = be_u16(i)?;
            let (i, class_def_1_offset) = be_u16(i)?;
            let (i, class_def_2_offset) = be_u16(i)?;
            let (i, class_1_count) = be_u16(i)?;
            let (i, class_2_count) = be_u16(i)?;
            
            let class_def_1 = parse_class_def(&data[class_def_1_offset as usize ..])?.1;
            let inverse_1 = invert_class_def(&class_def_1, num_glyphs);
            
            let class_def_2 = parse_class_def(&data[class_def_2_offset as usize ..])?.1;
            let inverse_2 = invert_class_def(&class_def_2, num_glyphs);
            
            let iter = (0 .. class_1_count).cartesian_product(0 .. class_2_count)
                .zip(iterator_n(i, tuple((value_record(value_format_1), value_record(value_format_2))), class_1_count * class_2_count));
            for ((class_1, class_2), (value_record_1, value_record_2)) in iter {
                if value_record_1.x_advance != 0 {
                    for (&gid1, &gid2) in inverse_1[&class_1].iter().cartesian_product(inverse_2[&class_2].iter()) {
                        kern.insert((gid1, gid2), value_record_1.x_advance);
                    }
                }
            }
        }
        n => panic!("unsupported pair adjustment format {}", n)
    }
    Ok((i, ()))
}
