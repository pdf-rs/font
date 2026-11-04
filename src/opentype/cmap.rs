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
        let mut codepoints: Vec<u32> = self.single_codepoint.keys().cloned().collect();
        codepoints.sort_unstable();
        codepoints.dedup();

        // Walk the sorted codepoints into maximal runs.
        let mut ranges: Vec<RangeInclusive<u32>> = Vec::new();
        for cp in codepoints {
            match ranges.last_mut() {
                Some(last) if last.end().checked_add(1) == Some(cp) => *last = *last.start() ..= cp,
                _ => ranges.push(cp ..= cp),
            }
        }

        // Reduce to `max_ranges` runs. Closing the boundary between two runs costs
        // exactly the gap in front of it, so the widest gaps stay open and every other boundary
        // is closed. That is one sort of the gaps, instead of a full rescan and a `splice` for
        // every merge — the old loop was O(n²) on a font whose codepoints are scattered.
        let max_ranges = max_ranges.max(1);
        if ranges.len() > max_ranges {
            let mut gaps: Vec<(u32, usize)> = (1 .. ranges.len())
                .map(|ix| (ranges[ix].start() - ranges[ix - 1].end() - 1, ix))
                .collect();
            // Widest first; equal gaps are broken by position so the result is stable.
            gaps.sort_unstable_by(|a, b| b.0.cmp(&a.0).then(a.1.cmp(&b.1)));
            gaps.truncate(max_ranges - 1);
            gaps.sort_unstable_by_key(|&(_, ix)| ix);

            let mut merged: Vec<RangeInclusive<u32>> = Vec::with_capacity(max_ranges);
            let mut start = *ranges[0].start();
            for &(_, ix) in &gaps {
                merged.push(start ..= *ranges[ix - 1].end());
                start = *ranges[ix].start();
            }
            merged.push(start ..= *ranges[ranges.len() - 1].end());
            return merged;
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
        
        let (i, format) = be_u16(table)?;
        debug!("cmap format {}", format);
        if !matches!(format, 0 | 4 | 6 | 12 | 14) {
            // OT §cmap: a font may carry several subtables of different formats and the reader
            // picks the best one it understands.  An unsupported format must not hide the
            // others, so skip this encoding record and keep looking.
            warn!("unsupported cmap format {}, skipping this subtable", format);
            continue;
        }

        match (cmap_encoding, encoding) {
            (None, e) => cmap_encoding = Some(e),
            (Some(c), e) => {
                if c != e {
                    warn!("more then one non-unicode encoding {:?} and {:?}", c, e);
                    continue;
                }
            }
        }

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
                // `len` counts the whole subtable; 4 header bytes are already consumed.
                // `saturating_sub`: a malformed `len < 4` is not a length, keep what we have.
                let body_len = (len as usize).saturating_sub(4);
                let data = if i.len() < body_len {
                    warn!("not enough data. trying anyway");
                    i
                } else {
                    &i[.. body_len]
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
                let seg_count_x2 = segCountX2 as usize;
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
                        // OT §cmap format 4: idRangeOffset[n] is an address relative to
                        // `&idRangeOffset[n]`.  `glyph_data` is what follows the whole
                        // idRangeOffset array (nom's `take` returns the rest first), so the
                        // start of that array sits `segCountX2` bytes before `glyph_data[0]`.
                        // All of this in `usize`: the operands are u16, the sum is not.
                        for c in start ..= end {
                            let index = 2 * n + offset as usize + 2 * (c - start) as usize;
                            // A malformed offset can point before the array (underflow) or past
                            // the subtable. Drop the rest of the segment instead of panicking
                            // on the subtraction or reading a neighbouring byte.
                            let index = match index.checked_sub(seg_count_x2) {
                                Some(index) => index,
                                None => break,
                            };
                            if index + 2 > glyph_data.len() {
                                break;
                            }
                            let (_, gid) = be_u16(slice!(glyph_data, index ..))?;
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
            n => warn!("cmap format {} is not implemented", n),
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