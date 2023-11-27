#![allow(non_snake_case)]

use std::collections::HashMap;
use std::iter::once;
use std::borrow::Cow;
use std::rc::Rc;
use crate::{
    Font, Glyph, Value, Context, State, type1, type2, IResultExt, R,
    VMetrics, HMetrics, GlyphId, Name, Info, FontError, ParseResult,
};
use crate::parsers::count as count2;
use nom::{
    number::complete::{be_u8, be_u16, be_i16, be_u24, be_u32, be_i32},
    bytes::complete::{take},
    multi::{count, many0},
    combinator::map,
    sequence::tuple,
    error::{make_error, ErrorKind},
    Err::*,
};
use pdf_encoding::{Encoding, glyphname_to_unicode};
use pathfinder_content::outline::{Outline};
use pathfinder_geometry::{vector::Vector2F, transform2d::Transform2F, rect::RectF};
use tuple::TupleElements;

#[derive(Clone)]
pub struct CffFont {
    glyphs: Vec<Glyph>,
    font_matrix: Transform2F,
    pub codepoint_map: [u16; 256],  // codepoint -> glyph index
    name_map: HashMap<String, u16>,
    pub sid_map: HashMap<SID, u16>,
    pub unicode_map: HashMap<&'static str, u16>,
    encoding: Option<Encoding>,
    bbox: Option<RectF>,
    vmetrics: Option<VMetrics>,
    name: Name,
    info: Info,
    pub cid: bool,
}

impl CffFont {
    pub fn parse(data: &[u8], idx: u32) -> Result<Self, FontError> {
        let cff = t!(read_cff(data));
        let slot = t!(cff.slot(idx));
        let font = t!(slot.parse_font());
        Ok(font)
    }
}
impl Font for CffFont {
    fn num_glyphs(&self) -> u32 {
        self.glyphs.len() as u32
    }
    fn font_matrix(&self) -> Transform2F {
        self.font_matrix
    }
    fn glyph(&self, id: GlyphId) -> Option<Glyph> {
        self.glyphs.get(id.0 as usize).cloned()
    }

    fn is_empty_glyph(&self, gid: GlyphId) -> bool {
        self.glyphs.get(gid.0 as usize).map(|g| g.path.len() == 0).unwrap_or(true)
    }
    fn gid_for_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        match self.codepoint_map.get(codepoint as usize) {
            None | Some(&0) => None,
            Some(&n) => Some(GlyphId(n as u32))
        }
    }
    fn gid_for_unicode_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        let c = std::char::from_u32(codepoint)?;
        let mut buf = [0; 4];
        let s = c.encode_utf8(&mut buf);
        self.unicode_map.get(&*s).map(|&id| GlyphId(id as u32))
    }

    fn gid_for_name(&self, name: &str) -> Option<GlyphId> {
        match self.name_map.get(name) {
            None => None,
            Some(&gid) => Some(GlyphId(gid as u32))
        }
    }
    fn encoding(&self) -> Option<Encoding> {
        self.encoding
    }
    fn get_notdef_gid(&self) -> GlyphId {
        GlyphId(0)
    }
    fn bbox(&self) -> Option<RectF> {
        self.bbox
    }
    fn vmetrics(&self) -> Option<VMetrics> {
        self.vmetrics
    }
    fn name(&self) -> &Name {
        &self.name
    }
    fn info(&self) -> &Info {
        &self.info
    }
}

pub fn read_cff(data: &[u8]) -> Result<Cff, FontError> {
    let i = data;
    let (i, major) = be_u8(i)?;
    require_eq!(major, 1);
    let (i, minor) = be_u8(i)?;
    
    let (i, hdrSize) = be_u8(i)?;

    debug!("CFF Table: v. {}.{} {} bytes", major, minor, hdrSize);
    let (i, _offSize) = be_u8(i)?;
    require!(hdrSize >= 4);
    let (i, _) = take(hdrSize - 4)(i)?;
    
    let (i, _name_index) = index(i)?;
    let (i, dict_index) = index(i)?;
    let (i, string_index) = index(i)?;
    let (i, subroutines) = index(i)?;
    
    Ok(Cff {
        data,
        dict_index,
        string_index,
        subroutines
    })
}
fn bias(num: usize) -> i32 {
    if num < 1240 {
        107
    } else if num < 33900 {
        1131
    } else {
        32768
    }
}
type Dict = HashMap<Operator, Vec<Value>>;
type Index<'a> = Vec<&'a [u8]>;

pub struct Cff<'a> {
    data: &'a [u8],
    dict_index: Index<'a>,
    string_index:Index<'a>,
    subroutines: Index<'a>,
}
pub struct CffSlot<'a> {
    cff: Cff<'a>,
    top_dict: Dict,
    private_dict: Vec<Rc<Dict>>,
    char_strings: Index<'a>,
    subrs: Vec<Rc<Index<'a>>>,
    num_glyphs: usize,
    font_matrix: Transform2F,
}

fn parse_font_matrix(arr: &[Value]) -> Option<Transform2F> {
    TupleElements::from_iter(arr.iter().map(|&v| v.to_float()))
    .map(|(a, b, c, d, e, f)| {
        Transform2F::row_major(a, b, e, c, d, f)
    })
}
    
impl<'a> Cff<'a> {
    pub fn slot(self, idx: u32) -> Result<CffSlot<'a>, FontError> {
        let data = self.dict_index.get(idx as usize).ok_or(FontError::NoSuchSlot)?;
        let top_dict = dict(data)?;
        info!("top dict: {:?}", top_dict);
        
        let offset = get!(top_dict, &Operator::CharStrings, 0).to_usize()?;
        let (_, char_strings) = index(slice!(self.data, offset ..))?;
        
        // num glyphs includes glyph 0 (.notdef)
        let num_glyphs = char_strings.len() as usize;

        let mut font_matrix = top_dict.get(&Operator::FontMatrix).and_then(|a| parse_font_matrix(&a));
        
        // retrieve Font Dicts if it exists.
        let mut private_dict_list = vec![];
        let mut subrs_list = vec![];
        if let Some(fdarray_entry) = top_dict.get(&Operator::FDArray) {
            let fdarray_offset = get!(fdarray_entry, 0).to_usize()?;
            let (_, fdarray_data_list) = index(offset!(self.data, fdarray_offset))?;
            for fdarray_data in &fdarray_data_list {
                let fdarray_dict = dict(&fdarray_data)?;
                if let Some(ma) = fdarray_dict.get(&Operator::FontMatrix).and_then(|a| parse_font_matrix(&a)) {
                    font_matrix = Some(ma);
                }
                let private_dict_entry = get!(fdarray_dict, &Operator::Private);
                let (private_dict, subrs) = self.private_dict_and_subrs(&private_dict_entry)?;
                private_dict_list.push(Rc::new(private_dict));
                subrs_list.push(Rc::new(subrs));
            }
        }
        let mut private_dict = vec![];
        let mut subrs = vec![];
        if let Some(fdselect_entry) = top_dict.get(&Operator::FDSelect) {
            let fdselect_offset = get!(fdselect_entry, 0).to_usize()?;
            let fd_indices = fd_select(offset!(self.data, fdselect_offset), num_glyphs)?;
            for fd_idx in fd_indices {
                private_dict.push(Rc::clone(&private_dict_list[fd_idx]));
                subrs.push(Rc::clone(&subrs_list[fd_idx]));
            }
        } else {
            let private_dict_entry = top_dict.get(&Operator::Private)
                .expect("no private dict entry");

            let (private_dict_global, subrs_global) = self.private_dict_and_subrs(&private_dict_entry)?;
            let private_dict_global = Rc::new(private_dict_global);
            let subrs_global = Rc::new(subrs_global);
            for i in 0..num_glyphs {
                private_dict.push(Rc::clone(&private_dict_global));
                subrs.push(Rc::clone(&subrs_global));
            }
        }

        let offset = get!(top_dict, &Operator::CharStrings, 0).to_int()? as usize;
        let (_, char_strings) = index(slice!(self.data, offset ..))?;
        
        // num glyphs includes glyph 0 (.notdef)
        let num_glyphs = char_strings.len() as usize;
        
        Ok(CffSlot {
            cff: self,
            top_dict,
            private_dict,
            char_strings,
            subrs,
            num_glyphs,
            font_matrix: font_matrix.unwrap_or(Transform2F::from_scale(Vector2F::splat(0.001)))
        })
    }

    fn private_dict_and_subrs(&self, private_dict_entry: &[Value]) -> Result<(Dict, Index<'a>), FontError> {
        let private_dict_size = get!(private_dict_entry, 0).to_usize()?;
        let private_dict_offset = get!(private_dict_entry, 1).to_usize()?;
        let private_dict_data = get!(self.data, private_dict_offset .. private_dict_offset + private_dict_size);
        let private_dict = dict(private_dict_data)?;
        //info!("private dict: {:?}", private_dict);

        let (_, subrs) = private_dict.get(&Operator::Subrs)
            .and_then(|arr| arr.get(0))
            .map(|item| {
                let private_subroutines_offset = item.to_usize()?;
                index(offset!(self.data, private_dict_offset + private_subroutines_offset))
            })
            .transpose()?
            .unwrap_or_default();

        Ok((private_dict, subrs))
    }
}

fn fd_select(data: &[u8], num_glyphs: usize) -> Result<Vec<usize>, FontError> {
    let (data, fmt) = be_u8(data)?;
    match fmt {
        0 => count(map(be_u8, |i| i as usize), num_glyphs)(data).get(),
        3 => {
            let (data, nranges) = map(be_u16, |i| i as usize)(data)?;
            let (data, range3) = count(range3_record, nranges)(data)?;
            let (data, sentinel) = map(be_u16, |i| i as usize)(data)?;
            let mut indexes = vec![0; sentinel];
            let mut stop = sentinel;
            if get!(range3, 0).0 != 0 {
                error!("the first range must have a first GID of 0")
            }
            for (first, fd) in range3.into_iter().rev() {
                get!(mut indexes, first..stop).fill(fd);
                stop = first;
            }
            Ok(indexes)
        }
        _ => error!("invalid FDSelect format: {}", fmt),
    }
}

fn range3_record(data: &[u8]) -> R<(usize, usize)> {
    let (data, first) = map(be_u16, |f| f as usize)(data)?;
    let (data, fd) = map(be_u8, |f| f as usize)(data)?;
    Ok((data, (first, fd)))
}

impl<'a> CffSlot<'a> {
    pub fn font_matrix(&self) -> Transform2F {
        self.font_matrix
    }
    pub fn bbox(&self) -> Option<RectF> {
        self.top_dict.get(&Operator::FontBBox)
            .and_then(|arr| TupleElements::from_iter(arr.iter().map(|&v| v.to_float())))
            .map(|(a, b, c, d)| {
                RectF::from_points(Vector2F::new(a, b), Vector2F::new(c, d))
            })
    }
    // -> (outline, width, lsb)
    pub fn outlines(&self) -> Result<impl Iterator<Item=Result<(Outline, f32, f32), FontError>> + '_, FontError> {
        let n = self.top_dict.get(&Operator::CharstringType).map(|v| get!(v, 0).to_int()).transpose()?.unwrap_or(2);
        let char_string_type = match n {
            1 => CharstringType::Type1,
            2 => CharstringType::Type2,
            _ => panic!("invalid charstring type")
        };
        
        let global_subr_bias = match char_string_type {
            CharstringType::Type2 => bias(self.cff.subroutines.len() as usize),
            CharstringType::Type1 => 0
        };
        
        // build glyphs
        let mut state = State::new();
        Ok(self.char_strings.iter().enumerate().map(move |(id, data)| {
            trace!("charstring for glyph {}", id);
            let subr_bias = match char_string_type {
                CharstringType::Type2 => bias(self.subrs[id].len()),
                CharstringType::Type1 => 0
            };
            let context = Context {
                subr_bias,
                subrs: self.subrs[id].as_slice(),
                global_subrs: self.cff.subroutines.as_slice(),
                global_subr_bias
            };
            match char_string_type {
                CharstringType::Type1 => {
                    t!(type1::charstring(data, &context, &mut state));
                },
                CharstringType::Type2 => {
                    t!(type2::charstring(data, &context, &mut state));
                }
            }
            let default_width = self.private_dict[id].get(&Operator::DefaultWidthX)
                .map(|a| Ok(get!(a, 0).to_float())).
                transpose()?
                .unwrap_or(0.);
            let nominal_width = self.private_dict[id].get(&Operator::NominalWidthX)
                .map(|a| Ok(get!(a, 0).to_float()))
                .transpose()?
                .unwrap_or(0.);
            
            trace!("glyph {} {:?} {:?}", id, state.char_width, state.delta_width);
            let width = match (state.char_width, state.delta_width) {
                (Some(w), None) => w,
                (None, None) => default_width,
                (None, Some(delta)) => delta + nominal_width,
                (Some(_), Some(_)) => panic!("BUG: both char_width and delta_width set")
            };
            let lsb = state.lsb.unwrap_or_default();
            let path = state.take_path();
            state.clear();
            Ok((path, width, lsb))
        }))
    }
    pub fn weight(&self) -> Option<u16> {
        None
        /*
        self.private_dict.get(&Operator::Weight).and_then(|a| match a[0].to_int() {
            386 => Some(300), // Light
            388 => Some(400), // Regular
            387 => Some(500), // Medium
            390 => Some(600), // Semibold
            384 => Some(700), // Bold
            383 => Some(900), // Black
            _ => None
        })
        */
    }
    fn parse_font(&self) -> Result<CffFont, FontError> {
        let glyph_name = |sid: SID| {
            if let Some(name) = STANDARD_STRINGS.get(sid as usize) {
                return Ok(name.clone());
            }
            if let Some(data) = self.cff.string_index.get(sid as usize - STANDARD_STRINGS.len()) {
                return std::str::from_utf8(data).map_err(|_| FontError::Other(format!("invalid glyph name {:?}", data)));
            }
            Err(FontError::Other(format!("SID out of bounds {} > {} standard strings + {} font string index entries",
                sid, STANDARD_STRINGS.len(), self.cff.string_index.len())))
        };
        let charset_offset: usize = self.top_dict.get(&Operator::Charset).map(|v| get!(v, 0).to_usize()).transpose()?.unwrap_or(0);
        let sids: Cow<[SID]> = match charset_offset {
            0 => ISO_ADOBE_CHARSET[..].into(),
            1 => EXPERT_CHARSET[..].into(),
            2 => EXPERT_SUBSET_CHARSET[..].into(),
            offset => {
                let charset = charset(get!(self.cff.data, offset ..), self.num_glyphs).get()?;
                
                // index = gid - 1 -> sid
                match charset {
                    Charset::Continous(sids) => sids,
                    Charset::Ranges(ranges) => ranges.into_iter()
                        .flat_map(|(sid, num)| (sid .. sid + num + 1))
                        .collect(),
                }.into()
            }
        };
        
        // sid -> gid
        let sid_map: HashMap<SID, u16> = once(0).chain(sids.iter().cloned()).enumerate()
            .map(|(gid, sid)| (sid as u16, gid as u16))
            .inspect(|&(sid, gid)| debug!("sid {} ({:?}) -> gid {}", sid, glyph_name(sid), gid))
            .collect();
        let name_map: HashMap<_, _> = once(0).chain(sids.iter().cloned()).enumerate()
            .filter_map(|(gid, sid)| Some((glyph_name(sid).ok()?.to_owned(), gid as u16)))
            .collect();
        
        let build_default = |encoding: &[SID; 256]| -> [u16; 256] {
            let mut cmap = [0u16; 256];
            for (codepoint, sid) in encoding.iter().enumerate() {
                if let Some(&gid) = sid_map.get(sid) {
                    cmap[codepoint as usize] = gid;
                }
            }
            cmap
        };
        
        let (cmap, encoding) = match self.top_dict.get(&Operator::Encoding).map(|a| get!(a, 0).to_int()).transpose()? {
            None | Some(0)
                => (build_default(&STANDARD_ENCODING), Some(Encoding::AdobeStandard)),
            Some(1)
                => (build_default(&EXPERT_ENCODING), Some(Encoding::AdobeExpert)),
            Some(offset) => {
                let mut cmap = [0u16; 256];
                let (codepoints, supplement) = parse_encoding(get!(self.cff.data, offset as _ ..)).get()?;
                // encodings start at gid 1
                match codepoints {
                    GlyphEncoding::Continous(codepoints) => codepoints.iter()
                        .enumerate().for_each(|(gid, &codepoint)| cmap[codepoint as usize] = gid as u16 + 1),
                    GlyphEncoding::Ranges(ranges) => ranges.iter()
                        .flat_map(|&(first, left)| (first ..).take(left as usize + 1))
                        .enumerate().for_each(|(gid, codepoint)| cmap[codepoint as usize] = gid as u16 + 1),
                }
                supplement.iter()
                    .for_each(|&(codepoint, sid)| cmap[codepoint as usize] = sid_map[&sid]);
                    
                (cmap, None)
            }
        };
        debug!("cmap:");
        for (i, &gid) in cmap.iter().enumerate() {
            if gid != 0 {
                if let Some(&sid) = sids.get(gid as usize - 1) {
                    debug!("{} -> gid={}, sid={}, name={:?}", i, gid, sid, glyph_name(sid));
                }
            }
        }
        
        let glyphs: Vec<_> = self.outlines()?.map(|r| r.map(|(outline, width, lsb)| {
            Glyph {
                metrics: HMetrics {
                    advance: width,
                    lsb
                },
                path: outline
            }
        })).collect::<Result<_, _>>()?;
        
        let mut unicode_map = HashMap::with_capacity(glyphs.len());
        for n in 0..glyphs.len()-1 {
            if let Some(c) = sids.get(n).and_then(|&sid| glyph_name(sid).ok()).and_then(|name| glyphname_to_unicode(name)) {
                unicode_map.insert(c, (n+1) as u16);
            }
        }
        //info!("unicode_map: {:?}", unicode_map);
        
        Ok(CffFont {
            glyphs,
            font_matrix: self.font_matrix(),
            codepoint_map: cmap,
            name_map,
            unicode_map,
            encoding,
            bbox: self.bbox(),
            vmetrics: None,
            name: Name::default(),
            info: Info {
                weight: None,
            },
            sid_map,
            cid: self.top_dict.contains_key(&Operator::ROS)
        })
    }
}

fn dict(mut input: &[u8]) -> Result<HashMap<Operator, Vec<Value>>, FontError> {
    let mut map = HashMap::new();
    while input.len() > 0 {
        let mut args = Vec::new();
        while let Ok((i, arg)) = value(input) {
            args.push(arg);
            input = i;
        }
        let (i, key) = operator(input)?;
        map.insert(key, args);
        
        input = i;
    }

    Ok(map)
}

enum CharstringType {
    Type1,
    Type2
}

    
fn index(i: &[u8]) -> ParseResult<Vec<&[u8]>> {
    let (i, n) = map(be_u16, |n| n as usize)(i)?;
    debug!("n={}", n);
    if n != 0 {
        let (i, offSize) = t!(be_u8(i));
        let (i, offsets) = t!(count2(|i| offset(offSize)(i).map(|(i, o)| (i, o - 1)), n+1)(i));
        let (i, data) = t!(take(offsets[n])(i));
        
        let items = offsets.windows(2).map(|w| Ok(slice!(data, w[0] as usize .. w[1] as usize))).collect::<Result<_, _>>()?;
        Ok((i, items))
    } else {
        Ok((i, vec![]))
    }
}

fn offset(size: u8) -> impl Fn(&[u8]) -> ParseResult<u32> {
    move |i| Ok(match size {
        1 => map(be_u8, |n| n as u32)(i)?,
        2 => map(be_u16, |n| n as u32)(i)?,
        3 => be_u24(i)?,
        4 => be_u32(i)?,
        n => key!(n),
    })
}

fn float(data: &[u8]) -> ParseResult<f32> {
    let mut pos = 0;
    let mut next_nibble = || -> u8 {
        let nibble = (data[pos/2] >> (4 * (1 - (pos & 1)) as u8)) & 0xf;
        pos += 1;
        nibble
    };
    
    let mut is_negaive = false;
    let mut num_digits = 0;
    let mut n: u128 = 0;
    let mut p: i32 = 0;
    let mut power_negative = false;
    let mut decimal_point = None;
    loop {
        match next_nibble() {
            d @ 0 ..= 9 => {
                n = 10 * n + d as u128;
                num_digits += 1;
            }
            0xa => decimal_point = Some(num_digits),
            b @ 0xb | b @ 0xc => { // positive 10^x
                power_negative = b == 0xc;
                loop {
                    match next_nibble() {
                        d @ 0 ..= 9 => p = 10 * p + d as i32,
                        0xf => break,
                        b => key!(b),
                    }
                }
            },
            0xd => reserved!(0xd),
            0xe => is_negaive = true,
            0xf => break,
            _ => unreachable!()
        }
    }
    
    let mut value = n as f32;
    if is_negaive {
        value = -value;
    }
    let mut power = 0;
    if let Some(dp) = decimal_point {
        power += dp - num_digits;
    }
    if p != 0 {
        if power_negative {
            p *= -1;
        }
        power += p;
    }
    if power != 0 {
        value *= 10.0f32.powi(power);
    }
    Ok((&data[(pos+1)/2 ..], value))
}


fn value(input: &[u8]) -> ParseResult<Value> {
    let (i, b0) = be_u8(input)?;
    
    Ok(match b0 {
        22 ..= 27 => reserved!(b0),
        28 => map(be_i16, |n| n.into())(i)?,
        29 => map(be_i32, |n| n.into())(i)?,
        30 => float(i).map(|(i, f)| (i, f.into()))?,
        31 => reserved!(b0),
        b0 @ 32 ..= 246 => (i, (b0 as i32 - 139).into()),
        b0 @ 247 ..= 250 => map(be_u8, |b1| ((b0 as i32 - 247) * 256 + b1 as i32 + 108).into())(i)?,
        b0 @ 251 ..= 254 => map(be_u8, |b1| (-(b0 as i32 - 251) * 256 - b1 as i32 - 108).into())(i)?,
        255 => reserved!(b0),
        b0 => reserved!(b0),
    })
}

#[allow(dead_code)] 
#[derive(Debug, PartialEq, Eq, Hash)]
enum Operator {
    Version,
    Notice,
    Copyleft,
    FullName,
    FamilyName,
    Weight,
    IsFixedPitch,
    ItalicAngle,
    UnderlinePosition,
    UnderlineThickness,
    PaintType,
    CharstringType,
    FontMatrix,
    UniqueID,
    FontBBox,
    StrokeWidth,
    XUID,
    Charset,
    Encoding,
    CharStrings,
    Private,
    SyntheticBase,
    PostScript,
    BaseFontName,
    BaseFontBlend,
    ROS,
    CIDFontVersion,
    CIDFontRevision,
    CIDFontType,
    CIDCount,
    UIDBase,
    FDArray,
    FDSelect,
    FontName,
    
    BlueValues,
    OtherBlues,
    FamilyBlues,
    FamilyOtherBlues,
    BlueScale,
    BlueShift,
    BlueFuzz,
    StdHW,
    StdVW,
    StemSnapH,
    StemSnapV,
    ForceBold,
    LanguageGroup,
    ExpansionFactor,
    InitialRandomSeed,
    Subrs,
    DefaultWidthX,
    NominalWidthX,
    BCD,

    Reserved,
}

fn operator(input: &[u8]) -> ParseResult<Operator> {
    use Operator::*;
    
    let (i, b) = be_u8(input)?;
    let (i, v) = match b {
        0 => (i, Version),
        1 => (i, Notice),
        2 => (i, FullName),
        3 => (i, FamilyName),
        4 => (i, Weight),
        5 => (i, FontBBox),
        6 => (i, BlueValues),
        7 => (i, OtherBlues),
        8 => (i, FamilyBlues),
        9 => (i, FamilyOtherBlues),
        10 => (i, StdHW),
        11 => (i, StdVW),
        12 => {
            let (i, b) = be_u8(i)?;
            match b {
                0 => (i, Copyleft),
                1 => (i, IsFixedPitch),
                2 => (i, ItalicAngle),
                3 => (i, UnderlinePosition),
                4 => (i, UnderlineThickness),
                5 => (i, PaintType),
                6 => (i, CharstringType),
                7 => (i, FontMatrix),
                8 => (i, StrokeWidth),
                9 => (i, BlueScale),
                10 => (i, BlueShift),
                11 => (i, BlueFuzz),
                12 => (i, StemSnapH),
                13 => (i, StemSnapV),
                14 => (i, ForceBold),
                15 | 16 => (i, Reserved),
                17 => (i, LanguageGroup),
                18 => (i, ExpansionFactor),
                19 => (i, InitialRandomSeed),
                20 => (i, SyntheticBase),
                21 => (i, PostScript),
                22 => (i, BaseFontName),
                23 => (i, BaseFontBlend),
                24 ..= 29 => (i, Reserved),
                30 => (i, ROS),
                31 => (i, CIDFontVersion),
                32 => (i, CIDFontRevision),
                33 => (i, CIDFontType),
                34 => (i, CIDCount),
                35 => (i, UIDBase),
                36 => (i, FDArray),
                37 => (i, FDSelect),
                38 => (i, FontName),
                255 => (i, Reserved),
                n => {
                    key!(n);
                }
            }
        }
        13 => (i, UniqueID),
        14 => (i, XUID),
        15 => (i, Charset),
        16 => (i, Encoding),
        17 => (i, CharStrings),
        18 => (i, Private),
        19 => (i, Subrs),
        20 => (i, DefaultWidthX),
        21 => (i, NominalWidthX),
        22 ..= 27 => (i, Reserved),
        30 => (i, BCD),
        31 => (i, Reserved),
        255 => (i, Reserved),
        n => {
            key!(n);
        }
    };
    Ok((i, v))
}

type SID = u16;

#[derive(Debug)]
enum Charset {
    Continous(Vec<SID>),
    Ranges(Vec<(SID, u16)>), // start, num-1
}

fn ranges<'a, F>(count_parser: F, num_glyphs: usize) -> impl Fn(&'a [u8]) -> R<'a, Vec<(SID, u16)>> where
    F: Fn(&'a [u8])-> R<'a, u16>
{
    move |mut input: &[u8]| {
        let mut total = 0;
        let mut vec = Vec::new();
        loop {
            let (i, sid) = be_u16(input)?;
            let (i, n) = count_parser(i)?;
            vec.push((sid, n));
            
            total += n as usize + 1;
            input = i;
            
            if total >= num_glyphs - 1 {
                break;
            }
        }
        Ok((input, vec))
    }
}
fn charset(i: &[u8], num_glyphs: usize) -> R<Charset> {
    let (i, format) = be_u8(i)?;
    
    match format {
        0 => {
            map(count(be_u16, num_glyphs as usize - 1), |a| Charset::Continous(a))(i)
        },
        1 => {
            map(ranges(map(be_u8, |n| n as u16), num_glyphs), |r| Charset::Ranges(r))(i)
        }
        2 => {
            map(ranges(be_u16, num_glyphs), |r| Charset::Ranges(r))(i)
        },
        _ => panic!("invalid charset format")
    }
}

#[derive(Debug)]
enum GlyphEncoding {
    Continous(Vec<u8>),
    Ranges(Vec<(u8, u8)>), // start, num-1
}

fn parse_encoding(i: &[u8]) -> R<(GlyphEncoding, Vec<(u8, SID)>)> {
    let (i, format) = be_u8(i)?;
    
    let (i, encoding) = match format & 0x7F {
        0 => {
            let (i, num) = be_u8(i)?;
            map(count(be_u8, num as usize), |a| GlyphEncoding::Continous(a))(i)?
        },
        1 => {
            let (i, num) = be_u8(i)?;
            map(count(tuple((be_u8, be_u8)), num as usize), |a| GlyphEncoding::Ranges(a))(i)?
        }
        _ => panic!("invalid charset format")
    };
    if format & 0x80 != 0 {
        let (i, n) = be_u8(i)?;
        let (i, supplement) = count(tuple((be_u8, be_u16)), n as usize)(i)?;
        Ok((i, (encoding, supplement)))
    } else {
        Ok((i, (encoding, vec![])))
    }
}

pub static STANDARD_STRINGS: [&'static str; 391] = [
/*   0 */ ".notdef",
/*   1 */ "space",
/*   2 */ "exclam",
/*   3 */ "quotedbl",
/*   4 */ "numbersign",
/*   5 */ "dollar",
/*   6 */ "percent",
/*   7 */ "ampersand",
/*   8 */ "quoteright",
/*   9 */ "parenleft",
/*  10 */ "parenright",
/*  11 */ "asterisk",
/*  12 */ "plus",
/*  13 */ "comma",
/*  14 */ "hyphen",
/*  15 */ "period",
/*  16 */ "slash",
/*  17 */ "zero",
/*  18 */ "one",
/*  19 */ "two",
/*  20 */ "three",
/*  21 */ "four",
/*  22 */ "five",
/*  23 */ "six",
/*  24 */ "seven",
/*  25 */ "eight",
/*  26 */ "nine",
/*  27 */ "colon",
/*  28 */ "semicolon",
/*  29 */ "less",
/*  30 */ "equal",
/*  31 */ "greater",
/*  32 */ "question",
/*  33 */ "at",
/*  34 */ "A",
/*  35 */ "B",
/*  36 */ "C",
/*  37 */ "D",
/*  38 */ "E",
/*  39 */ "F",
/*  40 */ "G",
/*  41 */ "H",
/*  42 */ "I",
/*  43 */ "J",
/*  44 */ "K",
/*  45 */ "L",
/*  46 */ "M",
/*  47 */ "N",
/*  48 */ "O",
/*  49 */ "P",
/*  50 */ "Q",
/*  51 */ "R",
/*  52 */ "S",
/*  53 */ "T",
/*  54 */ "U",
/*  55 */ "V",
/*  56 */ "W",
/*  57 */ "X",
/*  58 */ "Y",
/*  59 */ "Z",
/*  60 */ "bracketleft",
/*  61 */ "backslash",
/*  62 */ "bracketright",
/*  63 */ "asciicircum",
/*  64 */ "underscore",
/*  65 */ "quoteleft",
/*  66 */ "a",
/*  67 */ "b",
/*  68 */ "c",
/*  69 */ "d",
/*  70 */ "e",
/*  71 */ "f",
/*  72 */ "g",
/*  73 */ "h",
/*  74 */ "i",
/*  75 */ "j",
/*  76 */ "k",
/*  77 */ "l",
/*  78 */ "m",
/*  79 */ "n",
/*  80 */ "o",
/*  81 */ "p",
/*  82 */ "q",
/*  83 */ "r",
/*  84 */ "s",
/*  85 */ "t",
/*  86 */ "u",
/*  87 */ "v",
/*  88 */ "w",
/*  89 */ "x",
/*  90 */ "y",
/*  91 */ "z",
/*  92 */ "braceleft",
/*  93 */ "bar",
/*  94 */ "braceright",
/*  95 */ "asciitilde",
/*  96 */ "exclamdown",
/*  97 */ "cent",
/*  98 */ "sterling",
/*  99 */ "fraction",
/* 100 */ "yen",
/* 101 */ "florin",
/* 102 */ "section",
/* 103 */ "currency",
/* 104 */ "quotesingle",
/* 105 */ "quotedblleft",
/* 106 */ "guillemotleft",
/* 107 */ "guilsinglleft",
/* 108 */ "guilsinglright",
/* 109 */ "fi",
/* 110 */ "fl",
/* 111 */ "endash",
/* 112 */ "dagger",
/* 113 */ "daggerdbl",
/* 114 */ "periodcentered",
/* 115 */ "paragraph",
/* 116 */ "bullet",
/* 117 */ "quotesinglbase",
/* 118 */ "quotedblbase",
/* 119 */ "quotedblright",
/* 120 */ "guillemotright",
/* 121 */ "ellipsis",
/* 122 */ "perthousand",
/* 123 */ "questiondown",
/* 124 */ "grave",
/* 125 */ "acute",
/* 126 */ "circumflex",
/* 127 */ "tilde",
/* 128 */ "macron",
/* 129 */ "breve",
/* 130 */ "dotaccent",
/* 131 */ "dieresis",
/* 132 */ "ring",
/* 133 */ "cedilla",
/* 134 */ "hungarumlaut",
/* 135 */ "ogonek",
/* 136 */ "caron",
/* 137 */ "emdash",
/* 138 */ "AE",
/* 139 */ "ordfeminine",
/* 140 */ "Lslash",
/* 141 */ "Oslash",
/* 142 */ "OE",
/* 143 */ "ordmasculine",
/* 144 */ "ae",
/* 145 */ "dotlessi",
/* 146 */ "lslash",
/* 147 */ "oslash",
/* 148 */ "oe",
/* 149 */ "germandbls",
/* 150 */ "onesuperior",
/* 151 */ "logicalnot",
/* 152 */ "mu",
/* 153 */ "trademark",
/* 154 */ "Eth",
/* 155 */ "onehalf",
/* 156 */ "plusminus",
/* 157 */ "Thorn",
/* 158 */ "onequarter",
/* 159 */ "divide",
/* 160 */ "brokenbar",
/* 161 */ "degree",
/* 162 */ "thorn",
/* 163 */ "threequarters",
/* 164 */ "twosuperior",
/* 165 */ "registered",
/* 166 */ "minus",
/* 167 */ "eth",
/* 168 */ "multiply",
/* 169 */ "threesuperior",
/* 170 */ "copyright",
/* 171 */ "Aacute",
/* 172 */ "Acircumflex",
/* 173 */ "Adieresis",
/* 174 */ "Agrave",
/* 175 */ "Aring",
/* 176 */ "Atilde",
/* 177 */ "Ccedilla",
/* 178 */ "Eacute",
/* 179 */ "Ecircumflex",
/* 180 */ "Edieresis",
/* 181 */ "Egrave",
/* 182 */ "Iacute",
/* 183 */ "Icircumflex",
/* 184 */ "Idieresis",
/* 185 */ "Igrave",
/* 186 */ "Ntilde",
/* 187 */ "Oacute",
/* 188 */ "Ocircumflex",
/* 189 */ "Odieresis",
/* 190 */ "Ograve",
/* 191 */ "Otilde",
/* 192 */ "Scaron",
/* 193 */ "Uacute",
/* 194 */ "Ucircumflex",
/* 195 */ "Udieresis",
/* 196 */ "Ugrave",
/* 197 */ "Yacute",
/* 198 */ "Ydieresis",
/* 199 */ "Zcaron",
/* 200 */ "aacute",
/* 201 */ "acircumflex",
/* 202 */ "adieresis",
/* 203 */ "agrave",
/* 204 */ "aring",
/* 205 */ "atilde",
/* 206 */ "ccedilla",
/* 207 */ "eacute",
/* 208 */ "ecircumflex",
/* 209 */ "edieresis",
/* 210 */ "egrave",
/* 211 */ "iacute",
/* 212 */ "icircumflex",
/* 213 */ "idieresis",
/* 214 */ "igrave",
/* 215 */ "ntilde",
/* 216 */ "oacute",
/* 217 */ "ocircumflex",
/* 218 */ "odieresis",
/* 219 */ "ograve",
/* 220 */ "otilde",
/* 221 */ "scaron",
/* 222 */ "uacute",
/* 223 */ "ucircumflex",
/* 224 */ "udieresis",
/* 225 */ "ugrave",
/* 226 */ "yacute",
/* 227 */ "ydieresis",
/* 228 */ "zcaron",
/* 229 */ "exclamsmall",
/* 230 */ "Hungarumlautsmall",
/* 231 */ "dollaroldstyle",
/* 232 */ "dollarsuperior",
/* 233 */ "ampersandsmall",
/* 234 */ "Acutesmall",
/* 235 */ "parenleftsuperior",
/* 236 */ "parenrightsuperior",
/* 237 */ "twodotenleader",
/* 238 */ "onedotenleader",
/* 239 */ "zerooldstyle",
/* 240 */ "oneoldstyle",
/* 241 */ "twooldstyle",
/* 242 */ "threeoldstyle",
/* 243 */ "fouroldstyle",
/* 244 */ "fiveoldstyle",
/* 245 */ "sixoldstyle",
/* 246 */ "sevenoldstyle",
/* 247 */ "eightoldstyle",
/* 248 */ "nineoldstyle",
/* 249 */ "commasuperior",
/* 250 */ "threequartersemdash",
/* 251 */ "periodsuperior",
/* 252 */ "questionsmall",
/* 253 */ "asuperior",
/* 254 */ "bsuperior",
/* 255 */ "centsuperior",
/* 256 */ "dsuperior",
/* 257 */ "esuperior",
/* 258 */ "isuperior",
/* 259 */ "lsuperior",
/* 260 */ "msuperior",
/* 261 */ "nsuperior",
/* 262 */ "osuperior",
/* 263 */ "rsuperior",
/* 264 */ "ssuperior",
/* 265 */ "tsuperior",
/* 266 */ "ff",
/* 267 */ "ffi",
/* 268 */ "ffl",
/* 269 */ "parenleftinferior",
/* 270 */ "parenrightinferior",
/* 271 */ "Circumflexsmall",
/* 272 */ "hyphensuperior",
/* 273 */ "Gravesmall",
/* 274 */ "Asmall",
/* 275 */ "Bsmall",
/* 276 */ "Csmall",
/* 277 */ "Dsmall",
/* 278 */ "Esmall",
/* 279 */ "Fsmall",
/* 280 */ "Gsmall",
/* 281 */ "Hsmall",
/* 282 */ "Ismall",
/* 283 */ "Jsmall",
/* 284 */ "Ksmall",
/* 285 */ "Lsmall",
/* 286 */ "Msmall",
/* 287 */ "Nsmall",
/* 288 */ "Osmall",
/* 289 */ "Psmall",
/* 290 */ "Qsmall",
/* 291 */ "Rsmall",
/* 292 */ "Ssmall",
/* 293 */ "Tsmall",
/* 294 */ "Usmall",
/* 295 */ "Vsmall",
/* 296 */ "Wsmall",
/* 297 */ "Xsmall",
/* 298 */ "Ysmall",
/* 299 */ "Zsmall",
/* 300 */ "colonmonetary",
/* 301 */ "onefitted",
/* 302 */ "rupiah",
/* 303 */ "Tildesmall",
/* 304 */ "exclamdownsmall",
/* 305 */ "centoldstyle",
/* 306 */ "Lslashsmall",
/* 307 */ "Scaronsmall",
/* 308 */ "Zcaronsmall",
/* 309 */ "Dieresissmall",
/* 310 */ "Brevesmall",
/* 311 */ "Caronsmall",
/* 312 */ "Dotaccentsmall",
/* 313 */ "Macronsmall",
/* 314 */ "figuredash",
/* 315 */ "hypheninferior",
/* 316 */ "Ogoneksmall",
/* 317 */ "Ringsmall",
/* 318 */ "Cedillasmall",
/* 319 */ "questiondownsmall",
/* 320 */ "oneeighth",
/* 321 */ "threeeighths",
/* 322 */ "fiveeighths",
/* 323 */ "seveneighths",
/* 324 */ "onethird",
/* 325 */ "twothirds",
/* 326 */ "zerosuperior",
/* 327 */ "foursuperior",
/* 328 */ "fivesuperior",
/* 329 */ "sixsuperior",
/* 330 */ "sevensuperior",
/* 331 */ "eightsuperior",
/* 332 */ "ninesuperior",
/* 333 */ "zeroinferior",
/* 334 */ "oneinferior",
/* 335 */ "twoinferior",
/* 336 */ "threeinferior",
/* 337 */ "fourinferior",
/* 338 */ "fiveinferior",
/* 339 */ "sixinferior",
/* 340 */ "seveninferior",
/* 341 */ "eightinferior",
/* 342 */ "nineinferior",
/* 343 */ "centinferior",
/* 344 */ "dollarinferior",
/* 345 */ "periodinferior",
/* 346 */ "commainferior",
/* 347 */ "Agravesmall",
/* 348 */ "Aacutesmall",
/* 349 */ "Acircumflexsmall",
/* 350 */ "Atildesmall",
/* 351 */ "Adieresissmall",
/* 352 */ "Aringsmall",
/* 353 */ "AEsmall",
/* 354 */ "Ccedillasmall",
/* 355 */ "Egravesmall",
/* 356 */ "Eacutesmall",
/* 357 */ "Ecircumflexsmall",
/* 358 */ "Edieresissmall",
/* 359 */ "Igravesmall",
/* 360 */ "Iacutesmall",
/* 361 */ "Icircumflexsmall",
/* 362 */ "Idieresissmall",
/* 363 */ "Ethsmall",
/* 364 */ "Ntildesmall",
/* 365 */ "Ogravesmall",
/* 366 */ "Oacutesmall",
/* 367 */ "Ocircumflexsmall",
/* 368 */ "Otildesmall",
/* 369 */ "Odieresissmall",
/* 370 */ "OEsmall",
/* 371 */ "Oslashsmall",
/* 372 */ "Ugravesmall",
/* 373 */ "Uacutesmall",
/* 374 */ "Ucircumflexsmall",
/* 375 */ "Udieresissmall",
/* 376 */ "Yacutesmall",
/* 377 */ "Thornsmall",
/* 378 */ "Ydieresissmall",
/* 379 */ "001.000",
/* 380 */ "001.001",
/* 381 */ "001.002",
/* 382 */ "001.003",
/* 383 */ "Black",
/* 384 */ "Bold",
/* 385 */ "Book",
/* 386 */ "Light",
/* 387 */ "Medium",
/* 388 */ "Regular",
/* 389 */ "Roman",
/* 390 */ "Semibold"
];

// char -> SID
pub static STANDARD_ENCODING: [SID; 256] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 0, 111, 112, 113, 114, 0, 115, 116, 117, 118, 119, 120, 121, 122, 0, 123, 0, 124, 125, 126, 127, 128, 129, 130, 131, 0, 132, 133, 0, 134, 135, 136, 137, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 138, 0, 139, 0, 0, 0, 0, 140, 141, 142, 143, 0, 0, 0, 0, 0, 144, 0, 0, 0, 145, 0, 0, 146, 147, 148, 149, 0, 0, 0, 0];

static EXPERT_ENCODING: [SID; 256] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 229, 230, 0, 231, 232, 233, 234, 235, 236, 237, 238, 13, 14, 15, 99, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 27, 28, 249, 250, 251, 252, 0, 253, 254, 255, 256, 257, 0, 0, 0, 258, 0, 0, 259, 260, 261, 262, 0, 0, 263, 264, 265, 0, 266, 109, 110, 267, 268, 269, 0, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 304, 305, 306, 0, 0, 307, 308, 309, 310, 311, 0, 312, 0, 0, 313, 0, 0, 314, 315, 0, 0, 316, 317, 318, 0, 0, 0, 158, 155, 163, 319, 320, 321, 322, 323, 324, 325, 0, 0, 326, 150, 164, 169, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 378];

static ISO_ADOBE_CHARSET: [SID; 228] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228];

static EXPERT_CHARSET: [SID; 165] = [1, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 13, 14, 15, 99, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 27, 28, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 109, 110, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 158, 155, 163, 319, 320, 321, 322, 323, 324, 325, 326, 150, 164, 169, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 378];

static EXPERT_SUBSET_CHARSET: [SID; 86] =[1, 231, 232, 235, 236, 237, 238, 13, 14, 15, 99, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 27, 28, 249, 250, 251, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 109, 110, 267, 268, 269, 270, 272, 300, 301, 302, 305, 314, 315, 158, 155, 163, 320, 321, 322, 323, 324, 325, 326, 150, 164, 169, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346];
