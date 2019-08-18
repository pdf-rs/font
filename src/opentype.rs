#![allow(non_snake_case)]

use std::convert::TryInto;
use std::collections::HashMap;
use std::ops::Deref;
use crate::{CffFont, TrueTypeFont, Font, R, IResultExt};
use crate::parsers::{iterator, parse};
use nom::{
    number::complete::{be_u8, be_i16, be_u16, be_i64, be_i32, be_u32},
    multi::{count, many0},
    combinator::map,
    bytes::complete::take,
    sequence::tuple,
};
use tuple::T4;
use vector::{Outline};

pub fn parse_opentype<O: Outline + 'static>(data: &[u8], idx: u32) -> Box<dyn Font<O>> {
    let mut tables = parse_tables(data).get();
    for (tag, _) in tables.entries() {
        debug!("tag: {:?} ({:?})", tag, std::str::from_utf8(&tag));
    }
    if let Some(data) = tables.take(b"CFF ") {
        Box::new(CffFont::parse(data, idx)) as _
    } else if tables.contains(b"glyf") {
        Box::new(TrueTypeFont::parse_glyf(tables)) as _
    } else {
        panic!("neither CFF nor glyf table found")
    }
}

pub struct Tables<T> {
    // (tag, data)
    pub entries: HashMap<[u8; 4], T>
}
impl<T: Deref<Target=[u8]>> Tables<T> {
    pub fn get(&self, tag: &[u8; 4]) -> Option<&[u8]> {
        self.entries.get(tag).map(|block| &**block)
    }
    pub fn entries(&self) -> impl Iterator<Item=([u8; 4], &[u8])>{
        self.entries.iter().map(|(&tag, data)| (tag, &**data))
    }
    pub fn take(&mut self, tag: &[u8; 4]) -> Option<T> {
        self.entries.remove(tag)
    }
    pub fn contains(&self, tag: &[u8; 4]) -> bool {
        self.entries.contains_key(tag)
    }
}
// (tag, content)
pub fn parse_tables(data: &[u8]) -> R<Tables<&[u8]>> {
    let (i, _magic) = take(4usize)(data)?; 
    let (i, num_tables) = be_u16(i)?;
    let (i, _search_range) = be_u16(i)?;
    let (i, _entry_selector) = be_u16(i)?;
    let (mut i, _range_shift) = be_u16(i)?;
    
    let mut entries = HashMap::with_capacity(num_tables as usize);
    for _ in 0 .. num_tables {
        let (tag, _, off, len) = parse(&mut i, tuple((take(4usize), be_u32, be_u32, be_u32)))?;
        entries.insert(
            tag.try_into().expect("slice too short"),
            data.get(off as usize .. off as usize + len as usize).expect("out of bounds")
        );
    }
    
    Ok((i, Tables { entries }))
}

pub struct Head {
    pub units_per_em: u16,
    pub index_to_loc_format: i16,
    pub x_min: i16,
    pub x_max: i16,
    pub y_min: i16,
    pub y_max: i16,
}
pub fn parse_head(i: &[u8]) -> R<Head> {
    let (i, major) = be_u16(i)?;
    let (i, minor) = be_u16(i)?;
    assert_eq!((major, minor), (1, 0));
    
    let (i, _revision) = be_i32(i)?;
    let (i, _cksum) = be_u32(i)?;
    let (i, magic) = be_i32(i)?;
    assert_eq!(magic, 0x5F0F3CF5);
    
    let (i, _flags) = be_u16(i)?;
    let (i, units_per_em) = be_u16(i)?;

    let (i, _created) = be_i64(i)?;
    let (i, _modified) = be_i64(i)?;
    
    let (i, x_min) = be_i16(i)?;
    let (i, y_min) = be_i16(i)?;
    let (i, x_max) = be_i16(i)?;
    let (i, y_max) = be_i16(i)?;
    
    let (i, _mac_style) = be_u16(i)?;
    
    let (i, _lowest_rec_ppem) = be_u16(i)?;
    
    let (i, _font_direction_hint) = be_u16(i)?;
    let (i, index_to_loc_format) = be_i16(i)?;
    let (i, glyph_data_format) = be_u16(i)?;
    assert_eq!(glyph_data_format, 0);
    
    Ok((i, Head {
        units_per_em,
        index_to_loc_format,
        x_min, x_max, y_min, y_max
    }))
}
pub struct Maxp {
    pub num_glyphs: u16
}
pub fn parse_maxp(i: &[u8]) -> R<Maxp> {
    let (i, _version) = be_i32(i)?;
    let (i, num_glyphs) = be_u16(i)?;
    Ok((i, Maxp { num_glyphs }))
}
pub fn parse_loca<'a>(i: &'a [u8], head: &Head, maxp: &Maxp) -> R<'a, Vec<u32>> {
    match head.index_to_loc_format {
        0 => count(map(be_u16, |n| 2 * n as u32), maxp.num_glyphs as usize + 1)(i),
        1 => count(be_u32, maxp.num_glyphs as usize + 1)(i),
        _ => panic!("invalid index_to_loc_format")
    }
}
pub fn parse_cmap(input: &[u8]) -> R<HashMap<u32, u32>> {
    let (i, _version) = be_u16(input)?;
    let (i, num_tables) = be_u16(i)?;
    
    let tables = iterator(i, tuple((be_u16, be_u16, be_u32))).take(num_tables as usize)
        .filter_map(|entry| match entry {
            (0, _, off) | (3, 10, off) | (3, 1, off) => Some(off),
            _ => None
        })
        .filter_map(|off| input.get(off as usize ..));
    
    let mut cmap = HashMap::new();
    for table in tables {
        let (i, format) = be_u16(table)?;
        debug!("cmap format {}", format);
        match format {
            0 => {
                let (i, len) = be_u16(i)?;
                let (_i, data) = take(len - 4)(i)?; // aleady have 4 header bytes
                
                let (i, _language) = be_u16(data)?;
                for (code, gid) in iterator(i, be_u8).enumerate() {
                    if code != 0 {
                        cmap.insert(code as u32, gid as u32);
                    }
                }
            }
            4 => {
                let (i, len) = be_u16(i)?;
                let (_i, data) = take(len - 4)(i)?; // aleady have 4 header bytes
                
                let (i, _language) = be_u16(data)?;
                let (i, segCountX2) = be_u16(i)?;
                let (i, _searchRange) = be_u16(i)?;
                let (i, _entrySelector) = be_u16(i)?;
                let (i, _rangeShift) = be_u16(i)?;
                let (i, endCode) = take(segCountX2)(i)?;
                let (i, _reservedPad) = be_u16(i)?;
                let (i, startCode) = take(segCountX2)(i)?;
                let (i, idDelta) = take(segCountX2)(i)?;
                let (glyph_data, idRangeOffset) = take(segCountX2)(i)?;
                for (n, T4(start, end, delta, offset)) in T4(
                    iterator(startCode, be_u16),
                    iterator(endCode, be_u16),
                    iterator(idDelta, be_u16),
                    iterator(idRangeOffset, be_u16)
                ).into_iter().enumerate() {
                    debug!("start={}, end={}, delta={}, offset={}", start, end, delta, offset);
                    if start == 0xFFFF && end == 0xFFFF {
                        break;
                    }
                    if offset == 0 {
                        for c in start ..= end {
                            let gid = delta.wrapping_add(c);
                            debug!("codepoint {} -> gid {}", c, gid);
                            cmap.insert(c as u32, gid as u32);
                        }
                    } else {
                        for c in start ..= end {
                            let index = 2 * (n as u16 + (c - start)) + offset - segCountX2;
                            if index as usize > glyph_data.len() - 2 {
                                break;
                            }
                            let (_, gid) = be_u16(&glyph_data[index as usize ..])?;
                            if gid != 0 {
                                let gid = gid.wrapping_add(delta);
                                debug!("codepoint {} -> gid {}", c, gid);
                                cmap.insert(c as u32, gid as u32);
                            }
                        }
                    }
                }
            }
            12 => {
                let (i, _reserved) = be_u16(i)?;
                let (i, len) = be_u32(i)?;
                let (_i, data) = take(len - 8)(i)?; // aleady have 8 header bytes
                
                let (i, _language) = be_u32(data)?;
                let (i, num_groups) = be_u32(i)?;
                for (start_code, end_code, start_gid) in iterator(i, tuple((be_u32, be_u32, be_u32))).take(num_groups as usize) {
                    debug!("start_code={}, end_code={}, start_gid={}", start_code, end_code, start_gid);
                    for (code, gid) in (start_code ..= end_code).zip(start_gid ..) {
                        debug!("codepoint {} -> gid {}", code, gid);
                        cmap.insert(code, gid);
                    }
                }
            }
            n => unimplemented!("cmap format {}", n),
        }
    }
    Ok((&[], cmap))
}

pub struct Hhea {
    line_gap: i16,
    number_of_hmetrics: u16 
}
pub fn parse_hhea(i: &[u8]) -> R<Hhea> {
    let (i, _majorVersion) = be_u16(i)?;
    let (i, _minorVersion) = be_u16(i)?;
    let (i, _ascender) = be_i16(i)?;
    let (i, _descender) = be_i16(i)?;
    let (i, line_gap) = be_i16(i)?;
    let (i, _advanceWidthMax) = be_u16(i)?;
    let (i, _minLeftSideBearing) = be_i16(i)?;
    let (i, _minRightSideBearing) = be_i16(i)?;
    let (i, _xMaxExtent) = be_i16(i)?;
    let (i, _caretSlopeRise) = be_i16(i)?;
    let (i, _caretSlopeRun) = be_i16(i)?;
    let (i, _caretOffset) = be_i16(i)?;
    let (i, _) = be_i16(i)?;
    let (i, _) = be_i16(i)?;
    let (i, _) = be_i16(i)?;
    let (i, _) = be_i16(i)?;
    
    let (i, _metricDataFormat) = be_i16(i)?;
    let (i, number_of_hmetrics) = be_u16(i)?;
    
    Ok((i, Hhea {
        line_gap,
        number_of_hmetrics
    }))
}
pub struct Hmtx {
    metrics: Vec<HMetrics>,
    lsbs: Vec<i16>,
    last_advance: u16
}
#[derive(Copy, Clone)]
pub struct HMetrics {
    pub advance: u16,
    pub lsb: i16
}
impl Hmtx {
    pub fn metrics_for_gid(&self, gid: u16) -> HMetrics {
        self.metrics.get(gid as usize).cloned().unwrap_or_else(|| {
            let lsb = self.lsbs.get(gid as usize - self.metrics.len()).cloned().unwrap_or(0);
            HMetrics { advance: self.last_advance, lsb }
        })
    }
}
pub fn parse_hmtx<'a>(i: &'a [u8], hhea: &Hhea, _maxp: &Maxp) -> R<'a, Hmtx> {
    let num_metrics = hhea.number_of_hmetrics;
    let (i, metrics) = count(
        map(
            tuple((be_u16, be_i16)),
            |(advance, lsb)| HMetrics { advance, lsb }
        ),
        num_metrics as usize
    )(i)?;
    let (i, lsbs) = many0(be_i16)(i)?;
    let last_advance = metrics.last().map(|m| m.advance).unwrap_or(0);
    
    Ok((i, Hmtx {
        metrics,
        lsbs,
        last_advance
    }))
}
pub fn parse_hmtx_woff2_format1<'a>(i: &'a [u8], head: &Head, hhea: &Hhea, maxp: &Maxp) -> R<'a, Hmtx> {
    let num_metrics = hhea.number_of_hmetrics as usize;
    let (i, flags) = be_u8(i)?; 
    let (mut i, advance_stream) = take(num_metrics * 2)(i)?;
    
    let metrics: Vec<_> = if flags & 1 == 0 {
        let lsb_stream = parse(&mut i, take(num_metrics * 2))?;
        iterator(advance_stream, be_u16).zip(iterator(lsb_stream, be_i16))
            .map(|(advance, lsb)| HMetrics { advance, lsb })
            .collect()
    } else {
        iterator(advance_stream, be_u16)
            .map(|advance| HMetrics { advance, lsb: head.x_min })
            .collect()
    };
    
    let lsbs = if flags & 2 == 0 {
        parse(&mut i, count(be_i16, maxp.num_glyphs as usize - num_metrics))?
    } else {
        vec![]
    };
    
    let last_advance = metrics.last().map(|m| m.advance).unwrap_or(0);
    
    Ok((i, Hmtx {
        metrics,
        lsbs,
        last_advance
    }))
}
