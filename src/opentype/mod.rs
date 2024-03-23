#![allow(non_snake_case)]

use std::convert::TryInto;
use std::collections::HashMap;
use std::ops::Deref;
use crate::{Font, R, IResultExt, VMetrics, HMetrics, Glyph, GlyphId, Name, FontInfo, FontType, Info, FontError, ParseResult};
use crate::truetype::{Shape, parse_shapes, get_outline};
#[cfg(feature="cff")]
use crate::cff::read_cff;
use pdf_encoding::Encoding;
use crate::parsers::{*};
use nom::{
    number::complete::{be_u8, be_i16, be_u16, be_i64, be_i32, be_u32},
    multi::{count, many0},
    combinator::map,
    bytes::complete::take,
    sequence::tuple,
};
use pathfinder_content::outline::Outline;
use pathfinder_geometry::{vector::Vector2F, transform2d::Transform2F, rect::RectF};
use itertools::Either;

#[cfg(feature="svg")]
use crate::svg::{SvgTable, parse_svg, SvgGlyph};

pub mod cmap;
pub mod gpos;
pub mod gsub;
pub mod kern;

#[cfg(feature="math")]
pub mod math;

pub mod gdef;
pub mod base;
pub mod os2;
pub mod post;

#[cfg(feature="math")]
use math::{parse_math, MathHeader};

use gpos::{parse_gpos, GPos};
use gsub::{GSub, parse_gsub};
use cmap::{CMap, parse_cmap};
use gdef::{GDef, parse_gdef};
use kern::parse_kern;
use base::parse_base;

#[derive(Clone)]
pub struct OpenTypeFont {
    outlines: Vec<Outline>,
    pub gpos: Option<GPos>,
    pub cmap: Option<CMap>,
    encoding: Option<Encoding>,
    hmtx: Option<Hmtx>,
    bbox: Option<RectF>,
    pub gsub: Option<GSub>,
    #[cfg(feature="math")]
    pub math: Option<MathHeader>,
    pub gdef: Option<GDef>,
    vmetrics: Option<VMetrics>,

    pub name_map: HashMap<String, u16>,
    #[cfg(feature="svg")]
    svg:  Option<SvgTable>,

    font_matrix: Transform2F,
    num_glyphs: u32,
    name: Name,
    info: Info,
}
impl OpenTypeFont {
    pub fn parse(data: &[u8]) -> Result<Self, FontError> {
        let tables = t!(parse_tables(data));
        for (tag, _) in tables.entries() {
            debug!("tag: {:?} ({:?})", tag, std::str::from_utf8(&tag));
        }
        
        OpenTypeFont::from_tables(tables)
    }
    pub fn info(data: &[u8]) -> Result<FontInfo, FontError> {
        let tables = parse_tables(data)?;
        let name = tables.get(b"name").map(|data| parse_name(data)).transpose()?.unwrap_or_default();
        let cmap = tables.get(b"cmap").map(|data| parse_cmap(data)).transpose()?;

        Ok(FontInfo {
            name,
            typ: FontType::OpenType,
            codepoints: cmap.map(|cmap| cmap.0.codepoints(10)).unwrap_or_default(),
        })
    }
    pub fn from_hmtx_glyf_and_tables(hmtx: Option<Hmtx>, glyf: Option<Vec<Shape>>, tables: Tables<impl Deref<Target=[u8]>>) -> Result<Self, FontError> {
        let outlines: Vec<_>;
        let font_matrix;
        let bbox;
        
        if let Some(cff) = tables.get(b"CFF ") {
            #[cfg(feature="cff")]{
            let slot = t!(read_cff(cff)?.slot(0));
            bbox = slot.bbox();
            outlines = t!(slot.outlines()?.map(|r| r.map(|(outline, _, _)| outline)).collect::<Result<_, _>>());
            font_matrix = slot.font_matrix();
            }
            #[cfg(not(feature="cff"))]
            return Err(FontError::TypeError("CFF is not enabled"));
        } else {
            let head = t!(parse_head(expect!(tables.get(b"head"), "no head")));
            bbox = Some(head.bbox());
            font_matrix = Transform2F::from_scale(Vector2F::splat(1.0 / head.units_per_em as f32));
            outlines = glyf.map(|shapes| (0 .. shapes.len()).filter_map(|idx| get_outline(&shapes, idx as u32)).collect()).unwrap_or_default();
        }

        #[cfg(feature="svg")]
        let svg = t!(tables.get(b"SVG ").map(|data| parse_svg(data)).transpose());
        
        let maxp = t!(tables.get(b"maxp").map(parse_maxp).transpose());
        let num_glyphs = maxp.as_ref().map(|maxp| maxp.num_glyphs as u32).unwrap_or(outlines.len() as u32);

        let gpos = if let Some(data) = tables.get(b"GPOS") {
            let maxp = expect!(maxp.as_ref(), "no maxp");
            Some(t!(parse_gpos(data, &maxp)))
        } else if let Some(data) = tables.get(b"kern") {
            Some(GPos::from_kern(t!(parse_kern(data))))
        } else {
            None
        };
        
        let gsub = t!(tables.get(b"GSUB").map(parse_gsub).transpose());
        
        let (cmap, encoding) = match t!(tables.get(b"cmap").map(|data| parse_cmap(data)).transpose()) {
            Some((cmap, encoding)) => (Some(cmap), Some(encoding)),
            None => (None, None)
        };
        #[cfg(feature="math")]
        let math = t!(tables.get(b"MATH").map(parse_math).transpose());
        
        let vmetrics = t!(tables.get(b"hhea").map(parse_hhea).transpose()).map(|v| v.into());
        let name = t!(tables.get(b"name").map(parse_name).transpose()).unwrap_or_default();
        let gdef = t!(tables.get(b"gdef").map(parse_gdef).transpose());
        let base = t!(tables.get(b"BASE").map(parse_base).transpose());
        let post = t!(tables.get(b"post").map(parse_post).transpose());
        let mut name_map = HashMap::new();
        if let Some(post) = post {
            name_map.extend(post.names.into_iter().enumerate().map(|(i, name)| (name.into(), i as u16)));
        }
        

        let weight = t!(tables.get(b"OS/2").map(|data| os2::parse_os2(data)).transpose()).map(|os2| os2.weight);

        Ok(OpenTypeFont {
            outlines,
            gpos,
            cmap,
            hmtx,
            bbox,
            gsub,
            
            #[cfg(feature="math")]
            math,
            
            gdef,
            vmetrics,
            encoding,
            name_map,

            #[cfg(feature="svg")]
            svg,

            font_matrix,
            num_glyphs,
            name,
            info: Info {
                weight,
            },
        })
    }
    pub fn from_tables<T>(tables: Tables<T>) -> Result<Self, FontError> where T: Deref<Target=[u8]> {
        let head = t!(parse_head(expect!(tables.get(b"head"), "no head")));
        let maxp = t!(parse_maxp(expect!(tables.get(b"maxp"), "no maxp")));
        let hhea = t!(parse_hhea(expect!(tables.get(b"hhea"), "no hhea")));
        
        let glyf = t!(tables.get(b"glyf").map(|data| {
            let loca = parse_loca(expect!(tables.get(b"loca"), "no loca"), &head, &maxp)?;
            parse_shapes(&loca, data)
        }).transpose());
        
        let hmtx = t!(tables.get(b"hmtx").map(|data| parse_hmtx(data, &hhea, &maxp).get()).transpose());
        
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
    fn is_empty_glyph(&self, gid: GlyphId) -> bool {
        self.outlines.get(gid.0 as usize).map(|o| o.len() == 0).unwrap_or(true)
    }

    #[cfg(feature="svg")]
    fn svg_glyph(&self, gid: GlyphId) -> Option<&SvgGlyph> {
        self.svg.as_ref().and_then(|svg| svg.glyphs.get(&(gid.0 as u16)))
    }
    fn gid_for_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        match self.cmap {
            Some(ref cmap) => cmap.get_codepoint(codepoint).map(GlyphId),
            None => None
        }
    }
    fn gid_for_unicode_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        match (self.cmap.as_ref(), self.encoding) {
            (Some(cmap), Some(Encoding::Unicode)) => cmap.get_codepoint(codepoint).map(GlyphId),
            (Some(cmap), _) => self.gid_for_codepoint(codepoint),
            _ => None
        }
    }
    fn gid_for_name(&self, name: &str) -> Option<GlyphId> {
        self.name_map.get(name).map(|&n| GlyphId(n as u32))
    }
    fn encoding(&self) -> Option<Encoding> {
        self.encoding
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
    fn info(&self) -> &Info {
        &self.info
    }
}

pub struct Tables<T> {
    // (tag, data)
    pub entries: HashMap<[u8; 4], T>
}
impl<T: Deref<Target=[u8]>> Tables<T> {
    pub fn get(&self, tag: &[u8; 4]) -> Option<&[u8]> {
        self.entries.get(tag).map(|block| &**block).filter(|data| data.len() > 0)
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
pub fn parse_tables(data: &[u8]) -> Result<Tables<&[u8]>, FontError> {
    let (i, _magic) = take(4usize)(data)?; 
    let (i, num_tables) = be_u16(i)?;
    let (i, _search_range) = be_u16(i)?;
    let (i, _entry_selector) = be_u16(i)?;
    let (mut i, _range_shift) = be_u16(i)?;
    
    debug!("{} tables", num_tables);
    let mut entries = HashMap::with_capacity(num_tables as usize);
    for _ in 0 .. num_tables {
        let (tag, _, off, len) = parse(&mut i, tuple((take(4usize), be_u32, be_u32, be_u32)))?;
        let (off, len) = (off as usize, len as usize);
        entries.insert(
            tag.try_into().expect("slice too short"),
            slice!(data, off .. off+len) //off + len)
        );
        debug!("tag: {:?} ({:?})", tag, std::str::from_utf8(&tag));
    }
    
    Ok(Tables { entries })
}

pub struct Head {
    pub units_per_em: u16,
    pub index_to_loc_format: i16,
    pub x_min: i16,
    pub x_max: i16,
    pub y_min: i16,
    pub y_max: i16,
    pub mac_style: u16,
}
impl Head {
    pub fn bbox(&self) -> RectF {
        let bb_min = Vector2F::new(self.x_min as f32, self.y_min as f32);
        let bb_max = Vector2F::new(self.x_max as f32, self.y_max as f32);
        RectF::from_points(bb_min, bb_max)
    }
}
pub fn parse_head(i: &[u8]) -> Result<Head, FontError> {
    let (i, major) = be_u16(i)?;
    let (i, minor) = be_u16(i)?;
    require_eq!((major, minor), (1, 0));
    
    let (i, _revision) = be_i32(i)?;
    let (i, _cksum) = be_u32(i)?;
    let (i, magic) = be_i32(i)?;
    require_eq!(magic, 0x5F0F3CF5);
    
    let (i, _flags) = be_u16(i)?;
    let (i, units_per_em) = be_u16(i)?;

    let (i, _created) = be_i64(i)?;
    let (i, _modified) = be_i64(i)?;
    
    let (i, x_min) = be_i16(i)?;
    let (i, y_min) = be_i16(i)?;
    let (i, x_max) = be_i16(i)?;
    let (i, y_max) = be_i16(i)?;
    
    let (i, mac_style) = be_u16(i)?;
    
    let (i, _lowest_rec_ppem) = be_u16(i)?;
    
    let (i, _font_direction_hint) = be_u16(i)?;
    let (i, index_to_loc_format) = be_i16(i)?;
    let (i, glyph_data_format) = be_u16(i)?;
    require_eq!(glyph_data_format, 0);
    
    Ok(Head {
        units_per_em,
        index_to_loc_format,
        x_min, x_max, y_min, y_max,
        mac_style,
    })
}
pub struct Maxp {
    pub num_glyphs: u16
}
pub fn parse_maxp(i: &[u8]) -> Result<Maxp, FontError> {
    let (i, _version) = be_i32(i)?;
    let (i, num_glyphs) = be_u16(i)?;
    Ok(Maxp { num_glyphs })
}
pub fn parse_loca<'a>(i: &'a [u8], head: &Head, maxp: &Maxp) -> Result<Vec<u32>, FontError> {
    match head.index_to_loc_format {
        0 => count(map(be_u16, |n| 2 * n as u32), maxp.num_glyphs as usize + 1)(i).get(),
        1 => count(be_u32, maxp.num_glyphs as usize + 1)(i).get(),
        _ => error!("invalid index_to_loc_format")
    }
}

pub struct Hhea {
    line_gap: i16,
    ascender: i16,
    descender: i16,
    number_of_hmetrics: u16 
}
pub fn parse_hhea(i: &[u8]) -> Result<Hhea, FontError> {
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
    
    Ok(Hhea {
        line_gap,
        ascender,
        descender,
        number_of_hmetrics
    })
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

pub fn parse_skript_list(data: &[u8]) -> Result<(), FontError> {
    let (i, script_count) = be_u16(data)?;
    
    for (tag, offset) in iterator_n(i, tuple((take(4usize), be_u16)), script_count) {
        debug!("script {}", String::from_utf8_lossy(tag));
        let script_data = slice!(data, offset as usize ..);
        
        let (i, _default_lang_sys_off) = be_u16(script_data)?;
        let (i, sys_lang_count) = be_u16(i)?;
        
        for (_tag, offset) in iterator_n(i, tuple((take(4usize), be_u16)), sys_lang_count) {
            let i = slice!(script_data, offset as usize ..);
            let (i, _lookup_order) = be_u16(i)?;
            let (i, _required_feature_index) = be_u16(i)?;
            let (i, feature_index_count) = be_u16(i)?;
            for _feature_index in iterator_n(i, be_u16, feature_index_count) {
            
            }
        }
    }
    Ok(())
}

pub fn parse_lookup_list(data: &[u8], mut inner: impl FnMut(usize, &[u8], u16, u16) -> Result<(), FontError>) -> Result<(), FontError> {
    let (i, lookup_count) = be_u16(data)?;
    for (lookup_idx, table_off) in iterator_n(i, be_u16, lookup_count).enumerate() {
        let table_data = slice!(data, table_off as usize ..);
        let (i, lookup_type) = be_u16(table_data)?;
        let (i, lookup_flag) = be_u16(i)?;
        let (i, subtable_count) = be_u16(i)?;
        
        for subtable_off in iterator_n(i, be_u16, subtable_count) {
            inner(lookup_idx, slice!(table_data, subtable_off as usize ..), lookup_type, lookup_flag)?;
        }
    }
    Ok(())
}

// maps gid -> class id
pub fn parse_class_def<'a>(data: &'a [u8], map: &mut HashMap<u16, u16>) -> Result<(), FontError> {
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
        f => error!("invalid class list format {}", f)
    }
    Ok(())
}

pub fn coverage_table<'a>(i: &'a [u8]) -> Result<impl Iterator<Item=u16> + 'a, FontError> {
    let (i, format) = be_u16(i)?;
    debug!("coverage table format {}", format);
    match format {
        1 => {
            let (i, glyph_count) = be_u16(i)?;
            Ok(Either::Left(iterator_n(i, be_u16, glyph_count)))
        },
        2 => {
            let (i, range_count) = be_u16(i)?;
            Ok(Either::Right(
                iterator_n(i, tuple((be_u16, be_u16, be_u16)), range_count)
                    .flat_map(|(start, end, _i)| start ..= end)
            ))
        },
        n => error!("invalid coverage format {}", n)
    }
}

pub fn parse_name(data: &[u8]) -> Result<Name, FontError> {
    let mut name = Name::default();

    let (i, format) = be_u16(data)?;
    match format {
        0 | 1 => {
            let (i, count) = be_u16(i)?;
            let (i, string_offset) = be_u16(i)?;
            let string_data = slice!(data, string_offset as usize ..);
            for name_record in iterator_n(i, tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16)), count) {
                let (platform_id, encoding_id, language_id, name_id, length, offset) = name_record;
                //debug!("platform_id={}, encoding_id={}, language_id={}, name_id={}", platform_id, encoding_id, language_id, name_id);

                let encoded = slice!(string_data, offset as usize .. offset as usize + length as usize);
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
                    (3, 0) | (3, 1) => *field = utf16_be(encoded).ok(),
                    _ => {}
                }
            }
        }
        _ => {}
    }
    Ok(name)
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

use std::fmt;

use self::post::parse_post;
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Tag(pub [u8; 4]);
impl fmt::Debug for Tag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match std::str::from_utf8(&self.0) {
            Ok(s) => f.write_str(s),
            Err(_) => write!(f, "{:?}", self.0)
        }
    }
}
impl NomParser for Tag {
    type Output = Self;
    fn parse2(i: &[u8]) -> ParseResult<Self::Output> {
        let (i, t) = tag(i)?;
        Ok((i, t))
    }
}
impl FixedSize for Tag {
    const SIZE: usize = 4;
}
/// Comparison against `&str`. The argument can be up to 4 bytes long.
/// 
/// The comparison behaves as if the argument was padded to a length of 4 bytes with spaces.
impl PartialEq<str> for Tag {
    fn eq(&self, other: &str) -> bool {
        let n = other.len();
        if n < 4 {
            self.0[.. n] == *other.as_bytes() && self.0[n ..].iter().all(|&b| b == b' ')
        } else {
            self.0 == *other.as_bytes()
        }
    }
}
fn tag(i: &[u8]) -> R<Tag> {
    let (i, s) = take(4usize)(i)?;
    Ok((i, Tag(s.try_into().unwrap())))
}
