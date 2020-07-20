#![allow(non_snake_case)]

use std::convert::TryInto;
use std::collections::HashMap;
use std::ops::{Deref, RangeInclusive};
use crate::{Font, R, IResultExt, VMetrics, HMetrics, Glyph, GlyphId, Name, FontInfo, FontType};
use crate::truetype::{Shape, parse_shapes, get_outline};
use crate::cff::{read_cff};
use encoding::Encoding;
use crate::parsers::{iterator, iterator_n, parse};
use nom::{
    number::complete::{be_u8, be_i16, be_u16, be_i64, be_i32, be_u32, be_u24},
    multi::{count, many0},
    combinator::map,
    bytes::complete::take,
    sequence::tuple,
};
use pathfinder_content::outline::{Outline, Contour};
use pathfinder_geometry::{vector::Vector2F, transform2d::Transform2F, rect::RectF};
use itertools::{Either};

#[cfg(feature="svg")]
use crate::svg::{SvgTable, parse_svg, SvgGlyph};

pub mod cmap;
pub mod gpos;
pub mod gsub;
pub mod kern;
pub mod math;
pub mod gdef;

use math::{parse_math, MathHeader};
use gpos::{parse_gpos, KernTable, GPos};
use gsub::{GSub, parse_gsub};
use cmap::{CMap, parse_cmap};
use gdef::{GDef, parse_gdef};
use kern::parse_kern;

#[derive(Clone)]
pub struct OpenTypeFont {
    outlines: Vec<Outline>,
    pub gpos: Option<GPos>,
    pub cmap: Option<CMap>,
    hmtx: Option<Hmtx>,
    bbox: Option<RectF>,
    pub gsub: Option<GSub>,
    pub math: Option<MathHeader>,
    pub gdef: Option<GDef>,
    vmetrics: Option<VMetrics>,

    #[cfg(feature="svg")]
    svg:  Option<SvgTable>,

    font_matrix: Transform2F,
    num_glyphs: u32,
    name: Name
}
impl OpenTypeFont {
    pub fn parse(data: &[u8]) -> Self {
        let tables = parse_tables(data).get();
        for (tag, _) in tables.entries() {
            debug!("tag: {:?} ({:?})", tag, std::str::from_utf8(&tag));
        }
        
        OpenTypeFont::from_tables(tables)
    }
    pub fn info(data: &[u8]) -> FontInfo {
        let tables = parse_tables(data).get();
        let name = tables.get(b"name").map(|data| parse_name(data).get()).unwrap_or_default();
        let cmap = tables.get(b"cmap").map(|data| parse_cmap(data).get());

        FontInfo {
            name,
            typ: FontType::OpenType,
            codepoints: cmap.map(|cmap| cmap.codepoints(10)).unwrap_or_default(),
        }
    }
    pub fn from_hmtx_glyf_and_tables(hmtx: Option<Hmtx>, glyf: Option<Vec<Shape>>, tables: Tables<impl Deref<Target=[u8]>>) -> Self {
        let outlines: Vec<_>;
        let font_matrix;
        let bbox;
        if let Some(cff) = tables.get(b"CFF ") {
            let slot = read_cff(cff).get().slot(0);
            bbox = slot.bbox();
            outlines = slot.outlines().map(|(outline, _, _)| outline).collect();
            font_matrix = slot.font_matrix();
        } else {
            let head = parse_head(tables.get(b"head").expect("no head")).get();
            bbox = Some(head.bbox());
            font_matrix = Transform2F::from_scale(Vector2F::splat(1.0 / head.units_per_em as f32));
            outlines = glyf.map(|shapes| (0 .. shapes.len()).filter_map(|idx| get_outline(&shapes, idx as u32)).collect()).unwrap_or_default();
        }

        #[cfg(feature="svg")]
        let svg = tables.get(b"SVG ").map(|data| parse_svg(data).unwrap().1);
        
        let maxp = tables.get(b"maxp").map(|data| parse_maxp(data).get());
        let num_glyphs = maxp.as_ref().map(|maxp| maxp.num_glyphs as u32).unwrap_or(outlines.len() as u32);

        let gpos = if let Some(data) = tables.get(b"GPOS") {
            let maxp = maxp.as_ref().expect("no maxp");
            Some(parse_gpos(data, &maxp).get())
        } else if let Some(data) = tables.get(b"kern") {
            Some(GPos::from_kern(parse_kern(data).get()))
        } else {
            None
        };
        
        let gsub = tables.get(b"GSUB").map(|data| parse_gsub(data).get());
        
        let cmap = tables.get(b"cmap").map(|data| parse_cmap(data).get());
        let math = tables.get(b"MATH").map(|data| parse_math(data).get());
        let vmetrics = tables.get(b"hhea").map(|data| parse_hhea(data).get().into());
        let name = tables.get(b"name").map(|data| parse_name(data).get()).unwrap_or_default();
        let gdef = tables.get(b"gdef").map(|data| parse_gdef(data).get());

        OpenTypeFont {
            outlines,
            gpos,
            cmap,
            hmtx,
            bbox,
            gsub,
            math,
            gdef,
            vmetrics,

            #[cfg(feature="svg")]
            svg,

            font_matrix,
            num_glyphs,
            name
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
    pub fn glyph_metrics(&self, gid: u16) -> Option<HMetrics> {
        self.hmtx.as_ref().map(|hmtx| hmtx.metrics_for_gid(gid))
    }
}
impl Font for OpenTypeFont {
    fn num_glyphs(&self) -> u32 {
        self.num_glyphs
    }
    fn font_matrix(&self) -> Transform2F {
        self.font_matrix
    }
    fn glyph(&self, gid: GlyphId) -> Option<Glyph> {
        self.outlines.get(gid.0 as usize).map(|outline| {
            Glyph {
                path: outline.clone(),
                metrics: self.hmtx.as_ref().map(|m| m.metrics_for_gid(gid.0 as u16)).unwrap_or_default()
            }
        })
    }

    #[cfg(feature="svg")]
    fn svg_glyph(&self, gid: GlyphId) -> Option<&SvgGlyph> {
        self.svg.as_ref().and_then(|svg| svg.glyphs.get(&(gid.0 as u16)))
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
    fn bbox(&self) -> Option<RectF> {
        self.bbox
    }
    fn vmetrics(&self) -> Option<VMetrics> {
        self.vmetrics
    }
    fn kerning(&self, left: GlyphId, right: GlyphId) -> f32 {
        self.gpos.as_ref().and_then(|gpos| gpos.kern.get(left.0 as u16, right.0 as u16).map(|k| k as f32)).unwrap_or(0.0)
    }
    fn name(&self) -> &Name {
        &self.name
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
    pub fn bbox(&self) -> RectF {
        let bb_min = Vector2F::new(self.x_min as f32, self.y_min as f32);
        let bb_max = Vector2F::new(self.x_max as f32, self.y_max as f32);
        RectF::from_points(bb_min, bb_max)
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

pub struct Hhea {
    line_gap: i16,
    ascender: i16,
    descender: i16,
    number_of_hmetrics: u16 
}
pub fn parse_hhea(i: &[u8]) -> R<Hhea> {
    let (i, _majorVersion) = be_u16(i)?;
    let (i, _minorVersion) = be_u16(i)?;
    let (i, ascender) = be_i16(i)?;
    let (i, descender) = be_i16(i)?;
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
        ascender,
        descender,
        number_of_hmetrics
    }))
}
impl Into<VMetrics> for Hhea {
    fn into(self) -> VMetrics {
        VMetrics {
            line_gap: self.line_gap as f32,
            ascent: self.ascender as f32,
            descent: self.descender as f32,
        }
    }
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
            advance: advance as f32,
            lsb: lsb as f32,
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

pub fn parse_vhea(i: &[u8]) -> R<VMetrics> {
    let (i, _version) = be_u16(i)?;
    let (i, ascent) = be_i16(i)?;
    let (i, descent) = be_i16(i)?;
    let (i, line_gap) = be_i16(i)?;
    Ok((i, VMetrics {
        ascent: ascent as f32,
        descent: descent as f32,
        line_gap: line_gap as f32,
    }))
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

pub fn parse_lookup_list(data: &[u8], mut inner: impl FnMut(usize, &[u8], u16, u16) -> R<()>) -> R<()> {
    let (i, lookup_count) = be_u16(data)?;
    for (lookup_idx, table_off) in iterator_n(i, be_u16, lookup_count).enumerate() {
        let table_data = &data[table_off as usize ..];
        let (i, lookup_type) = be_u16(table_data)?;
        let (i, lookup_flag) = be_u16(i)?;
        let (i, subtable_count) = be_u16(i)?;
        
        for subtable_off in iterator_n(i, be_u16, subtable_count) {
            inner(lookup_idx, &table_data[subtable_off as usize ..], lookup_type, lookup_flag)?;
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

pub fn parse_name(data: &[u8]) -> R<Name> {
    let mut name = Name::default();

    let (i, format) = be_u16(data)?;
    match format {
        0 | 1 => {
            let (i, count) = be_u16(i)?;
            let (i, string_offset) = be_u16(i)?;
            let string_data = &data[string_offset as usize ..];
            for name_record in iterator_n(i, tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16)), count) {
                let (platform_id, encoding_id, language_id, name_id, length, offset) = name_record;
                //debug!("platform_id={}, encoding_id={}, language_id={}, name_id={}", platform_id, encoding_id, language_id, name_id);

                let encoded = &string_data[offset as usize .. offset as usize + length as usize];
                //debug!("string: {:?}", encoded);

                let field = match name_id {
                    1 => &mut name.family,
                    2 => &mut name.subfamily,
                    4 => &mut name.full_name,
                    6 => &mut name.postscript_name,
                    _ => continue,
                };

                match (platform_id, encoding_id) {
                    (0, _) => *field = String::from_utf8(encoded.into()).ok(),
                    (3, 1) => *field = utf16_be(encoded).ok(),
                    _ => {}
                }
            }
        }
        _ => {}
    }
    Ok((i, name))
}

fn utf16_be(data: &[u8]) -> Result<String, std::string::FromUtf16Error> {
    let wide: Vec<u16> = data.chunks_exact(2).map(|c| {
        match c {
            &[a, b] => u16::from_be_bytes([a, b]),
            _ => unreachable!()
        }
    }).collect();
    String::from_utf16(&wide)
}
