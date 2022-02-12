#![cfg_attr(feature="unstable", feature(thread_local, type_alias_impl_trait))]
#[macro_use] extern crate log;
#[macro_use] extern crate slotmap;

use std::fmt;
use std::convert::TryInto;
use std::any::TypeId;
use nom::{IResult, Err::*, error::VerboseError};
use tuple::{TupleElements};
use pdf_encoding::Encoding;

#[cfg(feature="svg")]
pub use svg::SvgGlyph;

use pathfinder_geometry::{rect::RectF, vector::Vector2F, transform2d::Transform2F};
use pathfinder_content::outline::{Outline, Contour};

#[derive(Clone)]
pub struct Glyph {
    /// unit 1em
    pub metrics: HMetrics,
    
    /// transform by font_matrix to scale it to 1em
    pub path: Outline,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct GlyphId(pub u32);

#[derive(Copy, Clone)]
pub struct VMetrics {
    pub line_gap: f32,
    pub ascent: f32,
    pub descent: f32,
}
#[derive(Copy, Clone, Default)]
pub struct HMetrics {
    pub lsb: f32,
    pub advance: f32
}

#[derive(Default, Debug, Clone)]
pub struct Name {
    pub family: Option<String>,
    pub subfamily: Option<String>,
    pub postscript_name: Option<String>,
    pub full_name: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Info {
    pub weight: Option<u16>,
}

pub trait Font: 'static {
    /// Return the "number of glyphs" in the font.
    ///
    /// This may or may not correlate to the actual number of "real glyphs".
    /// It does however define the highest valid glyph id (*gid*) as `num_glyphs() - 1`
    fn num_glyphs(&self) -> u32;
    
    /// The transformation to get from glyph space (which all methods use) into text space with a unit of 1em.
    fn font_matrix(&self) -> Transform2F;
    
    /// Get the glyph identified by `gid`.
    ///
    /// Note, that a *gid* is only meaningful within one font and cannot be transfered to another font.
    fn glyph(&self, gid: GlyphId) -> Option<Glyph>;
    
    #[cfg(feature="svg")]
    fn svg_glyph(&self, gid: GlyphId) -> Option<&SvgGlyph> {
        None
    }

    /// Get the *gid* for the given codepoint in the "native encoding" of this font.
    ///
    /// (see `encoding()` to find out which that is).
    /// Returns None if there is no "native encoding", or the font does not contain a glyph for this codepoint.
    fn gid_for_codepoint(&self, _codepoint: u32) -> Option<GlyphId> {
        None
    }
    
    /// Get the *gid* for the glyph with the given *name*.
    ///
    /// Returns None if the underlying font does not define any names, or does not contain a glyph with this name.
    fn gid_for_name(&self, _name: &str) -> Option<GlyphId> {
        None
    }
    
    /// Get the *gid* for the glyph that corresponds to the single unicode scalar `codepoint`.
    ///
    /// Returns None if the font if the codepoint cannot be mapped to a glyph for whatever reason.
    fn gid_for_unicode_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        self.encoding()
            .and_then(|encoding| encoding.reverse_map())
            .and_then(|reverse| reverse.get(codepoint))
            .and_then(|cp| self.gid_for_codepoint(cp as u32))
    }
    
    /// The "native encoding" of this font.
    ///
    /// Returns None if this term does not apply or it isn't defined.
    fn encoding(&self) -> Option<Encoding> {
        None
    }
    
    /// The *gid* of the `.notdef' glyph.
    fn get_notdef_gid(&self) -> GlyphId {
        GlyphId(0)
    }
    
    /// The *bounding box* of all glyphs.
    ///
    /// No glyph **should** contain contours outside this rectangle.
    fn bbox(&self) -> Option<RectF> {
        None
    }
    
    /// Vertical metrics of the font (common across all glyphs)
    fn vmetrics(&self) -> Option<VMetrics> {
        None
    }
    
    /// Kerning distance for the given glyph pair
    fn kerning(&self, _left: GlyphId, _right: GlyphId) -> f32 {
        0.0
    }
    fn name(&self) -> &Name;

    fn info(&self) -> &Info;

    #[doc(hidden)]
    // this function must return the type id of the impl
    unsafe fn _type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
}
impl dyn Font + Sync + Send {
    pub fn downcast_ref<T: Font>(&self) -> Option<&T> {
        unsafe {
            if self._type_id() == TypeId::of::<T>() {
                Some(&*(self as *const dyn Font as *const T))
            } else {
                None
            }
        }
    }
    pub fn downcast_box<T: Font>(self: Box<Self>) -> Result<Box<T>, Box<Self>> {
        unsafe {
            if self._type_id() == TypeId::of::<T>() {
                let ptr = Box::into_raw(self);
                Ok(Box::from_raw(ptr.cast()))
            } else {
                Err(self)
            }
        }
    }
}

#[macro_use]
mod macros;
#[macro_use]
mod error;
mod truetype;
mod cff;
mod type1;
mod type2;
mod postscript;
pub mod opentype;
mod parsers;
mod eexec;

#[cfg(feature="woff")]
mod woff;

#[cfg(feature="svg")]
mod svg;

pub use error::FontError;
pub use truetype::TrueTypeFont;
pub use cff::CffFont;
pub use type1::Type1Font;
pub use opentype::{OpenTypeFont};

#[cfg(feature="woff")]
pub use woff::{parse_woff, parse_woff2};

pub type R<'a, T> = IResult<&'a [u8], T, VerboseError<&'a [u8]>>;
pub type ParseResult<'a, T> = Result<(&'a [u8], T), FontError>;

#[derive(Copy, Clone)]
pub enum Value {
    Int(i32),
    Float(f32)
}
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => i.fmt(f),
            Value::Float(x) => x.fmt(f)
        }
    }
}

impl Into<f32> for Value {
    #[inline]
    fn into(self) -> f32 {
        self.to_float()
    }
}
impl From<i16> for Value {
    #[inline]
    fn from(v: i16) -> Value {
        Value::Int(v as i32)
    }
}
impl From<i32> for Value {
    #[inline]
    fn from(v: i32) -> Value {
        Value::Int(v)
    }
}
impl From<f32> for Value {
    #[inline]
    fn from(v: f32) -> Value {
        Value::Float(v)
    }
}
impl Value {
    #[inline]
    fn to_int(self) -> Result<i32, FontError> {
        match self {
            Value::Int(i) => Ok(i),
            Value::Float(_) => Err(FontError::TypeError("tried to cast a float to int"))
        }
    }
    #[inline]
    fn to_uint(self) -> Result<u32, FontError> {
        match self {
            Value::Int(i) if i >= 0 => Ok(i as u32),
            Value::Int(_) => Err(FontError::TypeError("expected a unsigned int")),
            Value::Float(_) => Err(FontError::TypeError("tried to cast a float to int"))
        }
    }
    #[inline]
    fn to_usize(self) -> Result<usize, FontError> {
        match self {
            Value::Int(i) if i >= 0 => Ok(i as usize),
            Value::Int(_) => Err(FontError::TypeError("expected a unsigned int")),
            Value::Float(_) => Err(FontError::TypeError("tried to cast a float to int"))
        }
    }
    #[inline]
    fn to_float(self) -> f32 {
        match self {
            Value::Int(i) => i as f32,
            Value::Float(f) => f
        }
    }
}

#[inline]
fn v(x: impl Into<f32>, y: impl Into<f32>) -> Vector2F {
    Vector2F::new(x.into(), y.into())
}

pub trait TryIndex {
    fn try_index(&self, idx: usize) -> Option<&[u8]>;
}
impl TryIndex for () {
    #[inline]
    fn try_index(&self, _idx: usize) -> Option<&[u8]> {
        None
    }
}
impl TryIndex for Vec<Option<Vec<u8>>> {
    #[inline]
    fn try_index(&self, idx: usize) -> Option<&[u8]> {
        match self.get(idx) {
            Some(Some(ref v)) => Some(&**v),
            _ => None
        }
    }
}
impl TryIndex for Vec<Vec<u8>> {
    #[inline]
    fn try_index(&self, idx: usize) -> Option<&[u8]> {
        self.get(idx).map(|v| &**v)
    }
}
impl<'a> TryIndex for Vec<&'a [u8]> {
    #[inline]
    fn try_index(&self, idx: usize) -> Option<&[u8]> {
        self.get(idx).map(|v| *v)
    }
}
impl<'a> TryIndex for &'a [&'a [u8]] {
    #[inline]
    fn try_index(&self, idx: usize) -> Option<&[u8]> {
        self.get(idx).map(|v| *v)
    }
}
    

pub struct Context<T=(), U=()> {
    pub subr_bias: i32,
    pub subrs: T,
    pub global_subrs: U,
    pub global_subr_bias: i32,
}

impl<T, U> Context<T, U> where T: TryIndex, U: TryIndex {
    #[inline]
    pub fn subr(&self, idx: i32) -> Result<&[u8], FontError> {
        match self.subrs.try_index((idx + self.subr_bias) as usize) {
            Some(sub) => Ok(sub),
            None => error!("requested subroutine {} not found", idx)
        }
    }
    #[inline]
    pub fn global_subr(&self, idx: i32) -> Result<&[u8], FontError> {
        match self.global_subrs.try_index((idx + self.global_subr_bias) as usize) {
            Some(sub) => Ok(sub),
            None => error!("requested global subroutine {} not found", idx)
        }
    }
}

pub struct State {
    pub stack: Vec<Value>,
    pub outline: Outline,
    pub contour: Contour,
    pub current: Vector2F,
    pub lsb: Option<f32>,
    pub char_width: Option<f32>,
    pub done: bool,
    pub stem_hints: u32,
    pub delta_width: Option<f32>,
    pub first_stack_clearing_operator: bool,
    pub flex_sequence: Option<Vec<Vector2F>>
}

impl State {
    #[inline]
    pub fn new() -> State {
        State {
            stack: Vec::new(),
            outline: Outline::new(),
            contour: Contour::new(),
            current: Vector2F::default(),
            lsb: None,
            char_width: None,
            done: false,
            stem_hints: 0,
            delta_width: None,
            first_stack_clearing_operator: true,
            flex_sequence: None
        }
    }
    #[inline]
    pub fn clear(&mut self) {
        self.stack.clear();
        self.outline.clear();
        self.contour.clear();
        self.current = Vector2F::default();
        self.lsb = None;
        self.char_width = None;
        self.done = false;
        self.stem_hints = 0;
        self.delta_width = None;
        self.first_stack_clearing_operator = true;
        self.flex_sequence = None;
    }

    #[inline]
    fn flush(&mut self) {
        if !self.contour.is_empty() {
            self.contour.close();
            self.outline.push_contour(self.contour.clone());
            self.contour.clear();
        }
    }
    #[inline]
    pub fn into_path(mut self) -> Outline {
        self.flush();
        self.outline
    }
    #[inline]
    pub fn take_path(&mut self) -> Outline {
        self.flush();
        let outline = self.outline.clone();
        outline
    }
    #[inline]
    pub fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into());
    }
    #[inline]
    pub fn pop(&mut self) -> Result<Value, FontError> {
        Ok(expect!(self.stack.pop(), "no value on the stack"))
    }
    #[inline]
    fn pop_tuple<T>(&mut self) -> T where
        T: TupleElements<Element=Value>
    {
        let range = self.stack.len() - T::N ..;
        T::from_iter(self.stack.drain(range)).unwrap()
    }
    /// get stack[0 .. T::N] as a tuple
    /// does not modify the stack
    #[inline]
    pub fn args<T>(&mut self) -> T where
        T: TupleElements<Element=Value>
    {
        trace!("get {} args from {:?}", T::N, self.stack);
        T::from_iter(self.stack.iter().cloned()).unwrap()
    }
}

pub trait IResultExt {
    type Item;
    fn get(self) -> Result<Self::Item, FontError>;
}

fn print_err(e: nom::Err<VerboseError<&[u8]>>) -> ! {
    match e {
        Incomplete(_) => panic!("need more data"),
        Error(v) | Failure(v) => {
            for (i, e) in v.errors {
                println!("{:?} {:?}", &i[.. i.len().min(20)], e);
                println!("{:?}", String::from_utf8_lossy(&i[.. i.len().min(20)]));
            }
            panic!()
        }
    }
}

impl<T> IResultExt for IResult<&[u8], T, VerboseError<&[u8]>> {
    type Item = T;
    #[inline]
    fn get(self) -> Result<T, FontError> {
        match self {
            Ok((_, t)) => Ok(t),
            Err(e) => Err(FontError::from(e))
        }
    }
}

#[derive(Debug, Clone)]
pub enum FontType {
    OpenType,
    TrueTypeCollection,
    Type1,
    Type1Pfb,
    Type1Pfa,
    TrueType,
    Woff,
    Woff2,
    Cff,
}

pub fn font_type(data: &[u8]) -> Option<FontType> {
    let t = match data.get(0..4)? {
        &[0x80, 1, _, _] => FontType::Type1Pfb,
        b"OTTO" | [0,1,0,0] => FontType::OpenType,
        b"ttcf" | b"typ1" => FontType::TrueTypeCollection,
        b"true" => FontType::TrueType,
        b"%!PS" => FontType::Type1,
        b"wOFF" => FontType::Woff,
        b"wOF2" => FontType::Woff2,
        &[1, _, _, _] => FontType::Cff,
        &[37, 33, _, _] => FontType::Type1Pfa,
        _ => return None
    };
    Some(t)
}

pub fn parse(data: &[u8]) -> Result<Box<dyn Font + Send + Sync + 'static>, FontError> {
    let magic: &[u8; 4] = slice!(data, 0 .. 4).try_into().unwrap();
    info!("font magic: {:?} ({:?})", magic, String::from_utf8_lossy(&*magic));
    Ok(match magic {
        &[0x80, 1, _, _] => Box::new(t!(Type1Font::parse_pfb(data))) as _,
        b"OTTO" | [0,1,0,0] => Box::new(t!(OpenTypeFont::parse(data))) as _,
        b"ttcf" | b"typ1" => error!("FontCollections not implemented"), // Box::new(TrueTypeFont::parse(data, 0)) as _,
        b"true" => Box::new(t!(TrueTypeFont::parse(data))) as _,
        b"%!PS" => Box::new(t!(Type1Font::parse_postscript(data))) as _,

        #[cfg(feature="woff")]
        b"wOFF" => Box::new(t!(woff::parse_woff(data))) as _,

        #[cfg(feature="woff")]
        b"wOF2" => Box::new(t!(woff::parse_woff2(data))) as _,
        
        &[1, _, _, _] => Box::new(t!(CffFont::parse(data, 0))) as _,
        &[37, 33, _, _] => Box::new(t!(Type1Font::parse_pfa(data))) as _,
        magic => return Err(FontError::UnknownMagic(*magic))
    })
}

use std::ops::RangeInclusive;
#[derive(Debug, Clone)]
pub struct FontInfo {
    pub name: Name,
    pub typ: FontType,
    pub codepoints: Vec<RangeInclusive<u32>>,
}

pub fn font_info(data: &[u8]) -> Option<FontInfo> {
    let magic: &[u8; 4] = data[0 .. 4].try_into().unwrap();
    info!("font magic: {:?} ({:?})", magic, String::from_utf8_lossy(&*magic));
    match magic {
        b"OTTO" | [0,1,0,0] => OpenTypeFont::info(data).ok(),
        _ => None
    }
}
