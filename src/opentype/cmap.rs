use std::collections::HashMap;
use std::ops::RangeInclusive;
use pdf_encoding::Encoding;
use crate::{GlyphId, R, parsers::*, FontError};
use nom::{
    number::complete::{be_u8, be_u16, be_u32, be_u24},
    bytes::complete::take,
    sequence::tuple,
};
use tuple::T4;

#[derive(Debug, Clone)]
pub struct CMap {
    single_codepoint: HashMap<u32, u32>,
    double_codepoint: HashMap<(u32, u32), u32>
}
impl CMap {
    pub fn get_codepoint(&self, cp: u32) -> Option<u32> {
        self.single_codepoint.get(&cp).cloned()
    }
    pub fn get_pair(&self, base: u32, variant: u32) -> Option<u32> {
        self.double_codepoint.get(&(base, variant)).cloned()
    }
    pub fn items<'a>(&'a self) -> impl Iterator<Item=(u32, GlyphId)> + 'a {
        self.single_codepoint.iter().map(|(&cp, &gid)| (cp, GlyphId(gid)))
    }
    pub fn codepoints(&self, max_ranges: usize) -> Vec<RangeInclusive<u32>> {
        use itertools::Itertools;

        let mut codepoints: Vec<u32> = self.single_codepoint.keys().cloned().collect();
        codepoints.sort();
        let num_gaps = codepoints.iter().cloned().tuple_windows().filter(|&(a, b)| a + 1 != b).count();
        let mut ranges = Vec::with_capacity(num_gaps + 1);
        let mut codepoints = codepoints.into_iter();
        if let Some(mut start) = codepoints.next() {
            let mut current = start;
            for cp in codepoints {
                if cp == current + 1 {
                    current = cp;
                    continue;
                }
                ranges.push(start ..= current);
                start = cp;
                current = cp;
            }
        }

        let mut tmp = Vec::with_capacity(ranges.len());
        while ranges.len() > max_ranges {
            tmp.clear();
            let min_gap = ranges.iter().tuple_windows().map(|(a, b)| *b.start() - *a.end()).min().unwrap();
            let mut ranges_iter = ranges.iter().cloned();
            if let Some(mut prev) = ranges_iter.next() {
                for next in ranges_iter {
                    if next.start() - prev.end() > min_gap * 2 {
                        tmp.push(prev);
                        prev = next;
                    } else {
                        prev = *prev.start() ..= *next.end();
                    }
                }
            } else {
                break;
            }
            std::mem::swap(&mut ranges, &mut tmp);
        }

        ranges
    }
}

pub fn parse_cmap(input: &[u8]) -> Result<(CMap, Encoding), FontError> {
    let (i, _version) = be_u16(input)?;
    let (i, num_tables) = be_u16(i)?;
    
    let mut cmap = HashMap::new();
    let mut cmap2 = HashMap::new();
    let mut cmap3 = Vec::new();
    let mut cmap_encoding = None;

    let have_unicode = iterator(i, tuple((be_u16, be_u16, be_u32))).take(num_tables as usize)
        .any(|t| matches!(t, (0, _, _) | (3, 0, _) | (3, 10, _) | (3, 1, _)));

    for (platform, encoding, offset) in iterator(i, tuple((be_u16, be_u16, be_u32))).take(num_tables as usize) {
        let encoding = match (platform, encoding) {
            (0, _) | (3, 0) | (3, 10) | (3, 1) => Encoding::Unicode,
            (1, 0) => {
                if have_unicode {
                    continue;
                } else {
                    Encoding::MacRomanEncoding
                }
            }
            _ => {
                warn!("unsupported cmap platform={}, encoding={}", platform, encoding);
                continue;
            }
        };

        let table = match input.get(offset as usize ..) {
            Some(data) => data,
            None => continue
        };
        
        match (cmap_encoding, encoding) {
            (None, e) => cmap_encoding = Some(e),
            (Some(c), e) => {
                if c != e {
                    warn!("more then one non-unicode encoding {:?} and {:?}", c, e);
                    continue;
                }
            }
        }

        let (i, format) = be_u16(table)?;
        debug!("cmap format {}", format);
        match format {
            0 => {
                let (i, len) = be_u16(i)?;
                let (_i, data) = t!(take(len - 4)(i)); // aleady have 4 header bytes
                
                let (i, _language) = be_u16(data)?;
                for (code, gid) in iterator(i, be_u8).enumerate() {
                    if gid != 0 {
                        cmap.insert(code as u32, gid as u32);
                    }
                }
            }
            4 => {
                let (i, len) = be_u16(i)?;
                //let (_i, data) = t!(take(len - 4)(i)); // aleady have 4 header bytes
                let data = if i.len() < len as usize - 4 {
                    warn!("not enough data. trying anyway");
                    i
                } else {
                    &i[.. len as usize - 4]
                };
                
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
                    trace!("start={}, end={}, delta={}, offset={}", start, end, delta, offset);
                    if start == 0xFFFF && end == 0xFFFF {
                        break;
                    }
                    if offset == 0 {
                        for c in start ..= end {
                            let gid = delta.wrapping_add(c);
                            trace!("codepoint {} -> gid {}", c, gid);
                            if gid != 0 {
                                cmap.insert(c as u32, gid as u32);
                            }
                        }
                    } else {
                        for c in start ..= end {
                            let index = 2 * (n as u16 + (c - start)) + offset - segCountX2;
                            if index as usize > glyph_data.len() - 2 {
                                break;
                            }
                            let (_, gid) = be_u16(slice!(glyph_data, index as usize ..))?;
                            let gid = gid.wrapping_add(delta);
                            if gid != 0 {
                                trace!("codepoint {} -> gid {}", c, gid);
                                cmap.insert(c as u32, gid as u32);
                            }
                        }
                    }
                }
            }
            6 => {
                let (i, _len) = be_u16(i)?;
                let (i, _language) = be_u16(i)?;
                let (i, first_code) = be_u16(i)?;
                let (i, entry_count) = be_u16(i)?;
                cmap.reserve(entry_count as usize);
                for (i, gid) in iterator_n(i, be_u16, entry_count).enumerate() {
                    if gid != 0 {
                        let c = first_code as u32 + i as u32;
                        trace!("codepoint {}({}+{}) -> gid {}", c, first_code, i, gid);
                        cmap.insert(c, gid as u32);
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
                    trace!("start_code={}, end_code={}, start_gid={}", start_code, end_code, start_gid);
                    for (code, gid) in (start_code ..= end_code).zip(start_gid ..) {
                        trace!("codepoint {} -> gid {}", code, gid);
                        if gid != 0 {
                            cmap.insert(code, gid);
                        }
                    }
                }
            }
            14 => {
                let (i, length) = be_u32(i)?;
                let i = slice!(i, .. length as usize - 6);
                
                let (i, num_var_selector_records) = be_u32(i)?;
                for (var_selector, default_uvs_offset, non_default_uvs_offset) in iterator(i, tuple((be_u24, be_u32, be_u32))).take(num_var_selector_records as usize) {
                    if default_uvs_offset != 0 {
                        let i = slice!(table, default_uvs_offset as usize ..);
                        let (i, num_unicode_value_ranges) = be_u32(i)?;
                        for (start_unicode_value, additional_count) in iterator(i, tuple((be_u24, be_u8))).take(num_unicode_value_ranges as usize) {
                            for cp in start_unicode_value ..= start_unicode_value + additional_count as u32 {
                                cmap3.push((cp, var_selector));
                            }
                        }
                    }
                    if non_default_uvs_offset != 0 {
                        let i = slice!(table, non_default_uvs_offset as usize ..);
                        let (i, num_uvs_mappings) = be_u32(i)?;
                        for (unicode_value, glyph_id) in iterator(i, tuple((be_u24, be_u16))).take(num_uvs_mappings as usize) {
                            if glyph_id != 0 {
                                cmap2.insert((unicode_value, var_selector), glyph_id as u32);
                            }
                        }
                    }
                }
            }
            n => error!("cmap format {}", n),
        }
    }

    for (cp, var_selector) in cmap3 {
        if let Some(&gid) = cmap.get(&cp) {
            cmap2.insert((cp, var_selector), gid);
        }
    }

    Ok((CMap {
        single_codepoint: cmap,
        double_codepoint: cmap2
    }, cmap_encoding.unwrap_or(Encoding::Unicode)))
}