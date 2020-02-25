#![allow(non_snake_case)]

use std::convert::TryInto;
use std::collections::HashMap;
use std::ops::Deref;
use crate::{Font, R, IResultExt, VMetrics, Glyph, HMetrics, GlyphId};
use crate::truetype::{Shape, parse_shapes, get_outline};
use crate::cff::{read_cff};
use crate::gpos::{parse_gpos, KernTable};
use crate::gsub::{Gsub, parse_gsub};
use encoding::Encoding;
use crate::parsers::{iterator, iterator_n, parse};
use nom::{
    number::complete::{be_u8, be_i16, be_u16, be_i64, be_i32, be_u32, be_u24},
    multi::{count, many0},
    combinator::map,
    bytes::complete::take,
    sequence::tuple,
};
use tuple::T4;
use vector::{Outline, Transform, Rect, Vector};
use itertools::{Either};

#[derive(Clone)]
pub struct OpenTypeFont<O: Outline> {
    outlines: Vec<O>,
    kern: KernTable,
    cmap: Option<CMap>,
    hmtx: Option<Hmtx>,
    bbox: Option<Rect>,
    gsub: Option<Gsub>,
    font_matrix: Transform
}
impl<O: Outline> OpenTypeFont<O> {
    pub fn parse(data: &[u8]) -> Self {
        let tables = parse_tables(data).get();
        for (tag, _) in tables.entries() {
            debug!("tag: {:?} ({:?})", tag, std::str::from_utf8(&tag));
        }
        
        OpenTypeFont::from_tables(tables)
    }
    pub fn from_hmtx_glyf_and_tables(hmtx: Option<Hmtx>, glyf: Option<Vec<Shape<O>>>, tables: Tables<impl Deref<Target=[u8]>>) -> Self {
        let (outlines, font_matrix, bbox) = match glyf {
            Some(shapes) => {
                let head = parse_head(tables.get(b"head").expect("no head")).get();
                let bbox = head.bbox();
                let outlines = (0 .. shapes.len()).map(|idx| get_outline(&shapes, idx as u32)).collect();
                let font_matrix = Transform::from_scale(Vector::splat(1.0 / head.units_per_em as f32));
                (outlines, font_matrix, Some(bbox))
            },
            None => {
                let cff = tables.get(b"CFF ").expect("neither glyf nor CFF tables … might be SVG");
                let slot = read_cff(cff).get().slot(0);
                let bbox = slot.bbox();
                let outlines = slot.outlines().map(|(outline, _, _)| outline).collect();
                let font_matrix = slot.font_matrix();
                (outlines, font_matrix, bbox)
            }
        };
        
        let kern = if let Some(data) = tables.get(b"GPOS") {
            let maxp = parse_maxp(tables.get(b"maxp").expect("no maxp")).get();
            let gpos = parse_gpos(data, &maxp).get();
            gpos.kern
        } else if let Some(data) = tables.get(b"kern") {
            parse_kern(data).get()
        } else {
            Default::default()
        };
        
        let gsub = tables.get(b"GSUB").map(|data| parse_gsub(data).get());
        
        info!("{} glyph pair kern entries and {} class pair kern entries", kern.glyph_pairs.len(), kern.class_pairs.len());

        let cmap = tables.get(b"cmap").map(|data| parse_cmap(data).get());
        
        OpenTypeFont {
            outlines,
            kern,
            cmap,
            hmtx,
            bbox,
            gsub,
            font_matrix
        }
    }
    pub fn from_tables<T>(tables: Tables<T>) -> Self where T: Deref<Target=[u8]> {
        let head = parse_head(tables.get(b"head").expect("no head")).get();
        let maxp = parse_maxp(tables.get(b"maxp").expect("no maxp")).get();
        let hhea = parse_hhea(tables.get(b"hhea").expect("no hhea")).get();
        
        let glyf = tables.get(b"glyf").map(|data| {
            let loca = parse_loca(tables.get(b"loca").expect("no loca"), &head, &maxp).get();
            parse_shapes(&loca, data)
        });
        
        let hmtx = tables.get(b"hmtx").map(|data| parse_hmtx(data, &hhea, &maxp).get());
        
        OpenTypeFont::from_hmtx_glyf_and_tables(hmtx, glyf, tables)
    }
}
impl<O: Outline> Font<O> for OpenTypeFont<O> {
    fn num_glyphs(&self) -> u32 {
        self.outlines.len() as u32
    }
    fn font_matrix(&self) -> Transform {
        self.font_matrix
    }
    fn glyph(&self, gid: GlyphId) -> Option<Glyph<O>> {
        self.outlines.get(gid.0 as usize).map(|outline| {
            Glyph {
                path: outline.clone(),
                metrics: self.hmtx.as_ref().map(|m| m.metrics_for_gid(gid.0 as u16)).unwrap_or_default()
            }
        })
    }
    fn gid_for_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        self.gid_for_unicode_codepoint(codepoint)
    }
    fn gid_for_unicode_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        match self.cmap {
            Some(ref cmap) => cmap.get_codepoint(codepoint).map(GlyphId),
            None => None
        }
    }
    fn gid_for_name(&self, _name: &str) -> Option<GlyphId> {
        None
    }
    fn encoding(&self) -> Option<Encoding> {
        None
    }
    fn bbox(&self) -> Option<Rect> {
        self.bbox
    }
    fn vmetrics(&self) -> Option<VMetrics> {
        None
    }
    fn kerning(&self, left: GlyphId, right: GlyphId) -> f32 {
        self.kern.get(left.0 as u16, right.0 as u16).unwrap_or(0) as f32
    }
    fn get_gsub(&self) -> Option<&Gsub> {
        self.gsub.as_ref()
    }
    fn get_cmap(&self) -> Option<&CMap> {
        self.cmap.as_ref()
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
        debug!("tag: {:?} ({:?})", tag, std::str::from_utf8(&tag));
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
impl Head {
    pub fn bbox(&self) -> Rect {
        let bb_min = Vector::new(self.x_min as f32, self.y_min as f32);
        let bb_max = Vector::new(self.x_max as f32, self.y_max as f32);
        Rect::from_points(bb_min, bb_max)
    }
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
#[derive(Debug, Clone)]
pub struct CMap {
    single_codepoint: HashMap<u32, u32>,
    double_codepoint: HashMap<(u32, u32), u32>
}
impl CMap {
    pub fn get_codepoint(&self, cp: u32) -> Option<u32> {
        self.single_codepoint.get(&cp).cloned()
    }
    pub fn items<'a>(&'a self) -> impl Iterator<Item=(u32, GlyphId)> + 'a {
        self.single_codepoint.iter().map(|(&cp, &gid)| (cp, GlyphId(gid)))
    }
}

pub fn parse_cmap(input: &[u8]) -> R<CMap> {
    let (i, _version) = be_u16(input)?;
    let (i, num_tables) = be_u16(i)?;
    dbg!(num_tables);
    
    let tables = iterator(i, tuple((be_u16, be_u16, be_u32))).take(num_tables as usize)
        .filter_map(|entry| match dbg!(entry) {
            (0, _, off) | (1, 0, off) | (3, 10, off) | (3, 1, off) => Some(off),
            (platform, encoding, _) => {
                warn!("unsupported cmap platform={}, encoding={}", platform, encoding);
                None
            }
        })
        .filter_map(|off| input.get(off as usize ..));
    
    let mut cmap = HashMap::new();
    let mut cmap2 = HashMap::new();
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
                    trace!("start={}, end={}, delta={}, offset={}", start, end, delta, offset);
                    if start == 0xFFFF && end == 0xFFFF {
                        break;
                    }
                    if offset == 0 {
                        for c in start ..= end {
                            let gid = delta.wrapping_add(c);
                            trace!("codepoint {} -> gid {}", c, gid);
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
                cmap.extend(iterator_n(i, be_u16, entry_count).enumerate()
                    .map(|(i, gid)| (first_code as u32 + i as u32, gid as u32))
                );
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
                        cmap.insert(code, gid);
                    }
                }
            }
            14 => {
                let (i, length) = be_u32(i)?;
                let i = &i[.. length as usize - 6];
                
                let (i, num_var_selector_records) = be_u32(i)?;
                for (var_selector, default_uvs_offset, non_default_uvs_offset) in iterator(i, tuple((be_u24, be_u32, be_u32))).take(num_var_selector_records as usize) {
                    if default_uvs_offset != 0 {
                        let i = &table[default_uvs_offset as usize ..];
                        let (i, num_unicode_value_ranges) = be_u32(i)?;
                        for (start_unicode_value, additional_count) in iterator(i, tuple((be_u24, be_u8))).take(num_unicode_value_ranges as usize) {
                            for cp in start_unicode_value ..= start_unicode_value + additional_count as u32 {
                                // lets hope cmap is filled already…
                                cmap2.insert((cp, var_selector), cmap[&cp]);
                            }
                        }
                    }
                    if non_default_uvs_offset != 0 {
                        let i = &table[non_default_uvs_offset as usize ..];
                        let (i, num_uvs_mappings) = be_u32(i)?;
                        for (unicode_value, glyph_id) in iterator(i, tuple((be_u24, be_u16))).take(num_uvs_mappings as usize) {
                            // lets hope cmap is filled already…
                            cmap2.insert((unicode_value, var_selector), glyph_id as u32);
                        }
                    }
                }
            }
            n => unimplemented!("cmap format {}", n),
        }
    }
    Ok((&[], CMap {
        single_codepoint: dbg!(cmap),
        double_codepoint: dbg!(cmap2)
    }))
}

pub struct Hhea {
    pub line_gap: i16,
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
#[derive(Clone)]
pub struct Hmtx {
    metrics: Vec<(u16, i16)>, // (advance, lsb)
    lsbs: Vec<i16>,
    last_advance: u16
}
impl Hmtx {
    pub fn metrics_for_gid(&self, gid: u16) -> HMetrics {
        let (advance, lsb) = self.metrics.get(gid as usize).cloned().unwrap_or_else(|| {
            (self.last_advance, self.lsbs.get(gid as usize - self.metrics.len()).cloned().unwrap_or(0))
        });
        HMetrics {
            advance: Vector::new(advance as f32, 0.0),
            lsb: Vector::new(lsb as f32, 0.0)
        }
    }
}
pub fn parse_hmtx<'a>(i: &'a [u8], hhea: &Hhea, _maxp: &Maxp) -> R<'a, Hmtx> {
    let num_metrics = hhea.number_of_hmetrics;
    let (i, metrics) = count(
        tuple((be_u16, be_i16)),
        num_metrics as usize
    )(i)?;
    let (i, lsbs) = many0(be_i16)(i)?;
    let last_advance = metrics.last().map(|&(advance, _)| advance).unwrap_or(0);
    
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
        iterator(advance_stream, be_u16).zip(iterator(lsb_stream, be_i16)).collect()
    } else {
        iterator(advance_stream, be_u16)
            .map(|advance| (advance, head.x_min ))
            .collect()
    };
    
    let lsbs = if flags & 2 == 0 {
        parse(&mut i, count(be_i16, maxp.num_glyphs as usize - num_metrics))?
    } else {
        vec![]
    };
    
    let last_advance = metrics.last().map(|&(advance, _)| advance).unwrap_or(0);
    
    Ok((i, Hmtx {
        metrics,
        lsbs,
        last_advance
    }))
}

fn parse_kern_format0<'a>(i: &'a [u8], table: &mut KernTable) -> R<'a, ()> {
    let (i, n_pairs) = be_u16(i)?;
    let (i, _search_range) = be_u16(i)?;
    let (i, _entry_selector) = be_u16(i)?;
    let (i, _range_shift) = be_u16(i)?;
    
    table.glyph_pairs.reserve(n_pairs as usize);
    for (left, right, kern) in iterator(i, tuple((be_u16, be_u16, be_i16))).take(n_pairs as usize) {
        table.glyph_pairs.insert((left, right), kern);
    }
    Ok((i, ()))
}
    
fn parse_kern_format2<'a>(data: &'a [u8], table: &mut KernTable) -> R<'a, ()> {
    let (i, _row_width) = be_u16(data)?;
    let (i, left_class_table_off) = be_u16(i)?;
    let (i, right_class_table_off) = be_u16(i)?;
    let (i, _array_off) = be_u16(i)?;
    
    let num_left = be_u16(&data[left_class_table_off as usize + 2 ..])?.1 as usize;
    let num_right = be_u16(&data[right_class_table_off as usize + 2 ..])?.1 as usize;
    table.glyph_pairs.reserve(num_left * num_right);

    let class_table = |off| {
        let (i, first_glyph) = be_u16(&data[off as usize ..])?;
        let (i, n_glyphs) = be_u16(i)?;
        let (_, offsets) = take(n_glyphs as usize * 2)(i)?;
        Ok((first_glyph ..).zip(iterator(offsets, map(be_u16, |n| n as usize))))
    };
    for (left_gid, left_off) in class_table(left_class_table_off)? {
        for (right_gid, right_off) in class_table(right_class_table_off)? {
            let off = left_off + right_off;
            let (_, kern) = be_i16(&data[off .. off + 2])?;
            table.glyph_pairs.insert((left_gid, right_gid), kern);
        }
    }
    Ok((i, ()))
}

pub fn parse_kern(input: &[u8]) -> R<KernTable> {
    let (_i, version) = be_u16(input)?;
    debug!("kern table version {}", version);
    match version {
        0 => parse_kern_ms(input),
        1 => parse_kern_apple(input),
        _ => panic!("stahp!")
    }
}

pub fn parse_kern_apple(i: &[u8]) -> R<KernTable> {
    let (_i, version) = be_u32(i)?;
    assert_eq!(version, 0x00010000);
    
    unimplemented!()
}
pub fn parse_kern_ms(i: &[u8]) -> R<KernTable> {
    let (i, version) = be_u16(i)?;
    assert_eq!(version, 0);
    
    let mut table = KernTable::default();
    let (mut i, n_tables) = be_u16(i)?;
    for _ in 0 .. n_tables {
        let (_version, length, format, coverage) = parse(&mut i, tuple((be_u16, be_u16, be_u8, be_u8)))?;
        debug!("format={}, coverage={:02x}", format, coverage);
        let data = parse(&mut i, take(length as usize - 6))?;
        match (format, coverage) {
            (0, 0x01) => parse_kern_format0(data, &mut table)?.1,
            (2, 0x01) => parse_kern_format2(data, &mut table)?.1,
            (f, _) => panic!("invalid kern subtable format {}", f)
        }
    }
    Ok((i, table))
}

pub fn parse_skript_list(data: &[u8]) -> R<()> {
    let (i, script_count) = be_u16(data)?;
    
    for (tag, offset) in iterator_n(i, tuple((take(4usize), be_u16)), script_count) {
        debug!("script {}", String::from_utf8_lossy(tag));
        let script_data = &data[offset as usize ..];
        
        let (i, _default_lang_sys_off) = be_u16(script_data)?;
        let (i, sys_lang_count) = be_u16(i)?;
        
        for (_tag, offset) in iterator_n(i, tuple((take(4usize), be_u16)), sys_lang_count) {
            let i = &script_data[offset as usize ..];
            let (i, _lookup_order) = be_u16(i)?;
            let (i, _required_feature_index) = be_u16(i)?;
            let (i, feature_index_count) = be_u16(i)?;
            for _feature_index in iterator_n(i, be_u16, feature_index_count) {
            
            }
        }
    }
    Ok((i, ()))
}

pub fn parse_lookup_list(data: &[u8], mut inner: impl FnMut(&[u8], u16, u16) -> R<()>) -> R<()> {
    let (i, lookup_count) = be_u16(data)?;
    for table_off in iterator_n(i, be_u16, lookup_count) {
        let table_data = &data[table_off as usize ..];
        let (i, lookup_type) = be_u16(table_data)?;
        let (i, lookup_flag) = be_u16(i)?;
        let (i, subtable_count) = be_u16(i)?;
        
        for subtable_off in iterator_n(i, be_u16, subtable_count) {
            inner(&table_data[subtable_off as usize ..], lookup_type, lookup_flag)?;
        }
    }
    Ok((i, ()))
}

// maps gid -> class id
pub fn parse_class_def<'a>(data: &'a [u8], map: &mut HashMap<u16, u16>) -> R<'a, ()> {
    let (i, format) = be_u16(data)?;
    match format {
        1 => {
            let (i, start_glyph_id) = be_u16(i)?;
            let (i, glyph_count) = be_u16(i)?;
            map.reserve(glyph_count as usize);
            for (gid, class) in (start_glyph_id ..).zip(iterator_n(i, be_u16, glyph_count)) {
                map.insert(gid, class);
            }
        }
        2 => {
            let (i, class_rage_count) = be_u16(i)?;
            map.reserve(class_rage_count as usize);
            for (start_gid, end_gid, class) in iterator_n(i, tuple((be_u16, be_u16, be_u16)), class_rage_count) {
                for gid in start_gid ..= end_gid {
                    map.insert(gid, class);
                }
            }
        }
        f => panic!("invalid class list format {}", f)
    }
    Ok((i, ()))
}

pub fn coverage_table<'a>(i: &'a [u8]) -> R<impl Iterator<Item=u16> + 'a> {
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
                    .flat_map(|(start, end, _i)| start ..= end)
            )))
        },
        n => panic!("invalid coverage format {}", n)
    }
}
