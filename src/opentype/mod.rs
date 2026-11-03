#![allow(non_snake_case)]

use std::convert::TryInto;
use std::collections::HashMap;
use std::ops::Deref;
use crate::{Font, R, IResultExt, VMetrics, HMetrics, Glyph, GlyphId, GlyphPosition, Name, FontInfo, FontType, Info, FontError, ParseResult};
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
    error::ParseError,
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
use gdef::{GDef, parse_gdef, LookupFlags};
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
            outlines = t!(slot.composed_outlines().map(|v| v.into_iter().map(|(outline, _, _)| outline).collect::<Vec<_>>()));
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

        // OT §GSUB/§GPOS/§GDEF are shaping data: a font that fails to parse them still draws
        // every glyph with the right advance, it only loses kerning/ligatures. Treating a
        // broken optional table as fatal made fonts every other engine renders unloadable
        // (audit B3), so each one is dropped with a warning instead.
        let gpos = match (tables.get(b"GPOS"), maxp.as_ref()) {
            (Some(data), Some(maxp)) => optional_layout("GPOS", || parse_gpos(data, &maxp)),
            // `parse_gpos` needs `num_glyphs`; without `maxp` the table cannot be read, but
            // that is still no reason to reject a font that draws fine (audit B3).
            (Some(_), None) => {
                warn!("GPOS without maxp — ignored");
                None
            }
            (None, _) => tables.get(b"kern")
                .and_then(|data| optional_layout("kern", || parse_kern(data).map(GPos::from_kern))),
        };
        
        let gsub = tables.get(b"GSUB").and_then(|data| optional_layout("GSUB", || parse_gsub(data)));
        
        let (cmap, encoding) = match t!(tables.get(b"cmap").map(|data| parse_cmap(data)).transpose()) {
            Some((cmap, encoding)) => (Some(cmap), Some(encoding)),
            None => (None, None)
        };
        #[cfg(feature="math")]
        let math = t!(tables.get(b"MATH").map(parse_math).transpose());
        
        let hhea = t!(tables.get(b"hhea").map(parse_hhea).transpose());
        let os2 = t!(tables.get(b"OS/2").map(|data| os2::parse_os2(data)).transpose());
        // OT §OS/2: with `fsSelection` bit 7 (USE_TYPO_METRICS) the `sTypo*` values are
        // the line metrics; without it applications lay out with `hhea`.
        let vmetrics = os2.as_ref().and_then(|os2| os2.typo_metrics())
            .or_else(|| hhea.map(|hhea| hhea.into()));
        let name = t!(tables.get(b"name").map(parse_name).transpose()).unwrap_or_default();
        let gdef = tables.get(b"GDEF").and_then(|data| optional_layout("GDEF", || parse_gdef(data)));
        let base = t!(tables.get(b"BASE").map(parse_base).transpose());
        let post = t!(tables.get(b"post").map(parse_post).transpose());
        let mut name_map = HashMap::new();
        if let Some(post) = post {
            name_map.extend(post.names.into_iter().enumerate().map(|(i, name)| (name.into(), i as u16)));
        }
        

        let weight = os2.as_ref().map(|os2| os2.weight);

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
                x_height: os2.as_ref().and_then(|os2| os2.x_height).map(|height| height as f32),
                cap_height: os2.as_ref().and_then(|os2| os2.cap_height).map(|height| height as f32),
                unicode_ranges: os2.as_ref().map(|os2| os2.unicode_ranges).unwrap_or_default(),
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
        
        let hmtx = t!(tables.get(b"hmtx").map(|data| parse_hmtx(data, &hhea, &maxp)).transpose());
        
        OpenTypeFont::from_hmtx_glyf_and_tables(hmtx, glyf, tables)
    }
    pub fn glyph_metrics(&self, gid: u16) -> Option<HMetrics> {
        self.hmtx.as_ref().and_then(|hmtx| hmtx.metrics_for_gid(gid))
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
                metrics: self.hmtx.as_ref().and_then(|m| m.metrics_for_gid(gid.0 as u16)).unwrap_or_default()
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
        self.gpos.as_ref().and_then(|gpos| gpos.kerning(left, right)).map(|k| k as f32).unwrap_or(0.0)
    }
    fn substitute(&self, glyphs: &[GlyphId], features: &[&[u8; 4]]) -> Vec<GlyphId> {
        match self.gsub.as_ref() {
            // OT §GSUB: a feature is enabled by its tag in the default language system. The
            // `GDEF` table tells each lookup which glyph classes it skips.
            Some(gsub) => gsub.substitute_with(glyphs, &|tag| features.iter().any(|&feature| tag.0 == *feature), self.gdef.as_ref()),
            None => glyphs.to_vec(),
        }
    }
    fn position(&self, glyphs: &[GlyphId], features: &[&[u8; 4]]) -> Vec<GlyphPosition> {
        match self.gpos.as_ref() {
            // Cursive and mark attachment place a glyph relative to another one, so they need
            // the advance of the base glyph to know where that one ends.
            Some(gpos) => gpos.position_with_advances(
                glyphs,
                &|tag| features.iter().any(|&feature| tag.0 == *feature),
                self.gdef.as_ref(),
                &|glyph| {
                    self.hmtx.as_ref().map_or(0, |hmtx| {
                        if glyph.0 > u16::max_value() as u32 {
                            0
                        } else {
                            hmtx.advance(glyph.0 as u16)
                        }
                    })
                },
            ),
            None => vec![GlyphPosition::default(); glyphs.len()],
        }
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
    // OT §head: `unitsPerEm` lies in 16..=16384. Zero would turn every coordinate into
    // `inf` through `font_matrix()`, so that one is fatal; the rest of the range is only a
    // convention several real fonts break, and clamping would move every coordinate.
    require!(units_per_em != 0);
    if units_per_em < 16 || units_per_em > 16384 {
        warn!("head: unitsPerEm {} outside 16..=16384 — taken as it is", units_per_em);
    }
    // OT §head: 0 = short `loca` offsets (u16/2), 1 = long (u32).
    require!(index_to_loc_format == 0 || index_to_loc_format == 1);
    
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
    let (i, version) = be_i32(i)?;
    // OT §maxp: 0x00005000 (version 0.5) or 0x00010000 (1.0); nothing else exists. The
    // fields read below are at the same offset in both, so an unknown version is only worth
    // a warning.
    if version != 0x0000_5000 && version != 0x0001_0000 {
        warn!("maxp: unknown version {:#010x} — read as 1.0", version);
    }
    let (i, num_glyphs) = be_u16(i)?;
    // A font without glyphs cannot be indexed: `loca` has `numGlyphs + 1` entries and several
    // computations below count down from `numGlyphs`, so zero would underflow.
    require!(num_glyphs >= 1);
    Ok(Maxp { num_glyphs })
}
pub fn parse_loca<'a>(i: &'a [u8], head: &Head, maxp: &Maxp) -> Result<Vec<u32>, FontError> {
    let loca = match head.index_to_loc_format {
        0 => count(map(be_u16, |n| 2 * n as u32), maxp.num_glyphs as usize + 1)(i).get(),
        1 => count(be_u32, maxp.num_glyphs as usize + 1)(i).get(),
        _ => error!("invalid index_to_loc_format")
    }?;
    // OT §loca: the offsets are monotonically non-decreasing. A glyph whose offsets run
    // backwards is read as an empty glyph by `parse_shapes`, so this is only a warning.
    if !loca.windows(2).all(|w| w[0] <= w[1]) {
        warn!("loca: offsets are not non-decreasing — glyphs with backwards ranges stay empty");
    }
    Ok(loca)
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
    last_advance: u16,
    num_glyphs: u16
}
impl Hmtx {
    pub fn metrics_for_gid(&self, gid: u16) -> Option<HMetrics> {
        // OT §hmtx: there is no metric for a glyph id at or beyond `numGlyphs`.
        if gid >= self.num_glyphs {
            return None;
        }
        let i = gid as usize;
        let (advance, lsb) = match self.metrics.get(i) {
            Some(&metric) => metric,
            None => (self.last_advance, *self.lsbs.get(i - self.metrics.len())?)
        };
        Some(HMetrics {
            advance: advance as f32,
            lsb: lsb as f32,
        })
    }
    /// The horizontal advance of `gid` in font units, 0 for a glyph id at or beyond
    /// `numGlyphs` (OT §hmtx: there is no metric for it).
    pub fn advance(&self, gid: u16) -> i16 {
        if gid >= self.num_glyphs {
            return 0;
        }
        match self.metrics.get(gid as usize) {
            Some(&(advance, _)) => advance as i16,
            None => self.last_advance as i16,
        }
    }
}
pub fn parse_hmtx(i: &[u8], hhea: &Hhea, maxp: &Maxp) -> Result<Hmtx, FontError> {
    // OT §hmtx: `numberOfHMetrics` is the number of glyphs with a full long metric and lies
    // in `1 ..= numGlyphs`. Truncated and padded tables are common, and neither value can make
    // the lookups below fail, so both are clamped to the range the table can describe.
    let num_metrics = match hhea.number_of_hmetrics {
        n if n == 0 || n > maxp.num_glyphs => {
            warn!("hmtx: numberOfHMetrics {} outside 1..={} — clamped", n, maxp.num_glyphs);
            if n == 0 { 1 } else { maxp.num_glyphs }
        }
        n => n,
    };
    let n_metrics = num_metrics as usize;
    let (i, metrics) = count(
        tuple((be_u16, be_i16)),
        n_metrics
    )(i)?;
    // `numGlyphs - numberOfHMetrics` left side bearings follow. A table that stops early leaves
    // the trailing glyphs without one, which `metrics_for_gid` reports as no metric.
    let n_lsbs = (maxp.num_glyphs - num_metrics) as usize;
    let available = i.len() / 2;
    if n_lsbs > available {
        warn!("hmtx: {} trailing side bearings expected, {} present — the rest are missing",
            n_lsbs, available);
    }
    let (i, lsbs) = count(be_i16, n_lsbs.min(available))(i)?;
    let last_advance = metrics.last().map(|&(advance, _)| advance).unwrap_or(0);
    
    Ok(Hmtx {
        metrics,
        lsbs,
        last_advance,
        num_glyphs: maxp.num_glyphs
    })
}
pub fn parse_hmtx_woff2_format1<'a>(i: &'a [u8], head: &Head, hhea: &Hhea, maxp: &Maxp) -> R<'a, Hmtx> {
    let num_metrics = hhea.number_of_hmetrics as usize;
    // WOFF2 §5.4: the transformed `hmtx` omits the trailing bearings whose count is
    // `numGlyphs - numberOfHMetrics`; a larger `numberOfHMetrics` must not wrap around.
    let n_lsbs = match (maxp.num_glyphs as usize).checked_sub(num_metrics) {
        Some(n) if num_metrics != 0 => n,
        _ => return Err(nom::Err::Failure(
            nom::error::VerboseError::from_error_kind(i, nom::error::ErrorKind::Complete)))
    };
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
        parse(&mut i, count(be_i16, n_lsbs))?
    } else {
        vec![]
    };
    
    let last_advance = metrics.last().map(|&(advance, _)| advance).unwrap_or(0);
    
    Ok((i, Hmtx {
        metrics,
        lsbs,
        last_advance,
        num_glyphs: maxp.num_glyphs
    }))
}

pub fn parse_vhea(i: &[u8]) -> R<VMetrics> {
    // OT §vhea: `majorVersion` and `minorVersion` are two separate `u16`s.  Reading them as one
    // field shifted every value after it by two bytes, so the parsed "ascender" was the minor
    // version (0 or 1).  v1.0 and v1.1 are both 36 bytes and differ only in the reserved words.
    let (i, _major_version) = be_u16(i)?;
    let (i, _minor_version) = be_u16(i)?;
    let (i, ascent) = be_i16(i)?;
    let (i, descent) = be_i16(i)?;
    let (i, line_gap) = be_i16(i)?;
    let (i, _advance_height_max) = be_u16(i)?;
    let (i, _min_top_side_bearing) = be_i16(i)?;
    let (i, _min_bottom_side_bearing) = be_i16(i)?;
    let (i, _y_max_extent) = be_i16(i)?;
    let (i, _caret_slope_rise) = be_i16(i)?;
    let (i, _caret_slope_run) = be_i16(i)?;
    let (i, _caret_offset) = be_i16(i)?;
    let (i, _reserved) = count(be_i16, 4)(i)?;
    let (i, _metric_data_format) = be_i16(i)?;
    let (i, _num_of_long_ver_metrics) = be_u16(i)?;
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

/// OT §GSUB/GPOS "Extension Lookup" subtable: format 1, the lookup type it wraps and a
/// 32-bit offset to the wrapped subtable (from the start of the extension subtable). Every
/// large font uses these, so a reader that skips them loses its ligatures and kerning.
pub fn parse_extension(data: &[u8]) -> Result<(u16, &[u8]), FontError> {
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);
    let (i, lookup_type) = be_u16(i)?;
    let (i, extension_offset) = be_u32(i)?;
    Ok((lookup_type, slice!(data, extension_offset as usize ..)))
}

/// Parse an optional layout table, dropping it with a warning when it is malformed.
///
/// `GSUB`, `GPOS`, `GDEF` and the legacy `kern` table are shaping data: a font whose layout
/// tables do not parse is still perfectly drawable, it only loses kerning and ligatures.
/// Failing all of `OpenTypeFont::parse` there rejected fonts that FreeType, HarfBuzz and
/// every browser render (audit B3).
fn optional_layout<T>(name: &str, f: impl FnOnce() -> Result<T, FontError>) -> Option<T> {
    match f() {
        Ok(v) => Some(v),
        Err(err) => {
            warn!("{}: {:?} — ignoring the table, the font itself is still usable", name, err);
            None
        }
    }
}

pub fn parse_lookup_list(data: &[u8], mut inner: impl FnMut(usize, &[u8], u16, LookupFlags) -> Result<(), FontError>) -> Result<(), FontError> {
    let (i, lookup_count) = be_u16(data)?;
    for (lookup_idx, table_off) in iterator_n(i, be_u16, lookup_count).enumerate() {
        // One unreadable lookup or subtable must not cost the font all of its others (audit
        // B3): the callbacks only append to the lookup vector after a successful parse, so
        // skipping a failing subtable leaves the rest of the table intact.
        if let Err(err) = parse_one_lookup(data, table_off as usize, lookup_idx, &mut inner) {
            warn!("shaping lookup {}: {:?} — skipped, keeping the other lookups", lookup_idx, err);
        }
    }
    Ok(())
}

fn parse_one_lookup(data: &[u8], table_off: usize, lookup_idx: usize,
                    inner: &mut impl FnMut(usize, &[u8], u16, LookupFlags) -> Result<(), FontError>) -> Result<(), FontError> {
    let table_data = match data.get(table_off ..) {
        Some(d) => d,
        None => return Err(FontError::OutOfBounds(file!(), line!())),
    };
    let (i, lookup_type) = be_u16(table_data)?;
    let (i, lookup_flag) = be_u16(i)?;
    let (i, subtable_count) = be_u16(i)?;
    // `UseMarkFilteringSet` (0x0010) makes the `Lookup` carry one more field after the subtable
    // count: the index of the GDEF mark set the lookup works on (OT §"Lookup Flags").
    let (i, mark_filtering_set) = if lookup_flag & 0x0010 != 0 {
        let field: R<u16> = be_u16(i);
        match field {
            Ok((rest, set)) => (rest, Some(set)),
            Err(_) => {
                warn!("lookup {} (type {}): UseMarkFilteringSet set but the field is missing",
                      lookup_idx, lookup_type);
                (i, None)
            }
        }
    } else {
        (i, None)
    };
    let lookup_flags = LookupFlags { flag: lookup_flag, mark_filtering_set };

    for subtable_off in iterator_n(i, be_u16, subtable_count) {
        let subtable = match table_data.get(subtable_off as usize ..) {
            // `slice!` would fail the whole lookup for one bad offset; keep the others.
            Some(s) => s,
            None => {
                warn!("lookup {} (type {}): subtable offset {} is past the end — skipped",
                      lookup_idx, lookup_type, subtable_off);
                continue;
            }
        };
        if let Err(err) = inner(lookup_idx, subtable, lookup_type, lookup_flags) {
            warn!("lookup {} (type {}): {:?} — subtable skipped", lookup_idx, lookup_type, err);
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

/// How good a `name` record is for a given name ID; higher wins (OT §name).
///
/// The same name can appear in many records — several languages and encodings. Windows
/// Unicode BMP in English is the one every application agrees on; a bare Windows record or
/// the Unicode platform come next, and Macintosh Roman (what Type 1-era outlines mostly
/// carry) is used only when nothing better exists.
fn name_record_score(platform_id: u16, encoding_id: u16, language_id: u16) -> Option<u8> {
    match (platform_id, encoding_id) {
        (3, 1) if language_id == 0x409 => Some(5),
        (3, 10) => Some(4),
        (3, 0) | (3, 1) => Some(3),
        (0, _) => Some(2),
        (1, 0) => Some(1),
        _ => None,
    }
}

fn decode_name_string(platform_id: u16, encoded: &[u8]) -> Option<String> {
    match platform_id {
        // Unicode platform 0: UTF-16BE in every version 0/1 record in practice.
        0 => utf16_be(encoded).ok().or_else(|| String::from_utf8(encoded.into()).ok()),
        3 => utf16_be(encoded).ok(),
        1 => Some(mac_roman(encoded)),
        _ => None,
    }
}

/// Macintosh Roman, upper half (`0x80..=0xFF`); the lower half is ASCII.
fn mac_roman(data: &[u8]) -> String {
    let mut out = String::with_capacity(data.len());
    for &b in data {
        if b < 0x80 {
            out.push(b as char);
        } else {
            out.push(MAC_ROMAN[b as usize - 0x80]);
        }
    }
    out
}

const MAC_ROMAN: [char; 128] = [
    'Ä', 'Å', 'Ç', 'É', 'Ñ', 'Ö', 'Ü', 'á', 'à', 'â', 'ä', 'ã', 'å', 'ç', 'é', 'è',
    'ê', 'ë', 'í', 'ì', 'î', 'ï', 'ñ', 'ó', 'ò', 'ô', 'ö', 'õ', 'ú', 'ù', 'û', 'ü',
    '†', '°', '¢', '£', '§', '•', '¶', 'ß', '®', '©', '™', '´', '¨', '≠', 'Æ', 'Ø',
    '∞', '±', '≤', '≥', '¥', 'µ', '∂', '∑', '∏', 'π', '∫', 'ª', 'º', 'Ω', 'æ', 'ø',
    '¿', '¡', '¬', '√', 'ƒ', '≈', '∆', '«', '»', '…', '\u{a0}', 'À', 'Ã', 'Õ', 'Œ', 'œ',
    '–', '—', '“', '”', '‘', '’', '÷', '◊', 'ÿ', 'Ÿ', '⁄', '€', '‹', '›', 'ﬁ', 'ﬂ',
    '‡', '·', '‚', '„', '‰', 'Â', 'Ê', 'Á', 'Ë', 'È', 'Í', 'Î', 'Ï', 'Ì', 'Ó', 'Ô',
    '\u{f8ff}', 'Ò', 'Ú', 'Û', 'Ù', 'ı', 'ˆ', '˜', '¯', '˘', '˙', '˚', '¸', '˝', '˛', 'ˇ',
];

pub fn parse_name(data: &[u8]) -> Result<Name, FontError> {
    let mut name = Name::default();
    // Best score seen per name ID 1..=6 (1 family, 2 subfamily, 4 full name, 6 PostScript).
    let mut best = [0u8; 7];

    let (i, format) = be_u16(data)?;
    match format {
        0 | 1 => {
            let (i, count) = be_u16(i)?;
            let (i, string_offset) = be_u16(i)?;
            let string_data = slice!(data, string_offset as usize ..);
            for name_record in iterator_n(i, tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16)), count) {
                let (platform_id, encoding_id, language_id, name_id, length, offset) = name_record;

                let field = match name_id {
                    1 => &mut name.family,
                    2 => &mut name.subfamily,
                    4 => &mut name.full_name,
                    6 => &mut name.postscript_name,
                    _ => continue,
                };
                // OT §name: the records for one ID are ordered arbitrarily, so the *last*
                // one is not the best one — pick by (platform, encoding, language).
                let score = match name_record_score(platform_id, encoding_id, language_id) {
                    Some(score) => score,
                    None => continue,
                };
                if score <= best[name_id as usize] {
                    continue;
                }

                let encoded = slice!(string_data, offset as usize .. offset as usize + length as usize);
                if let Some(decoded) = decode_name_string(platform_id, encoded) {
                    best[name_id as usize] = score;
                    *field = Some(decoded);
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
