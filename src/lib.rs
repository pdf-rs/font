#![cfg_attr(feature="unstable", feature(thread_local, type_alias_impl_trait))]
#[macro_use] extern crate log;
#[macro_use] extern crate slotmap;

use std::fmt;
use std::convert::TryInto;
use nom::{IResult, Err::*, error::VerboseError};
use tuple::TupleElements;
use pdf_encoding::Encoding;

#[cfg(feature="svg")]
pub use svg::SvgGlyph;

use pathfinder_geometry::{rect::RectF, vector::Vector2F, transform2d::Transform2F};

#[derive(Clone)]
pub struct Glyph<E: Encoder> {
    /// unit 1em
    pub metrics: HMetrics,
    
    /// transform by font_matrix to scale it to 1em
    pub shape: Shape<E>,
}

#[derive(Clone)]
pub enum Shape<E: Encoder> {
    Simple(E::GlyphRef),
    Compound(Vec<(GlyphId, Transform2F)>),
    Empty
}
impl<E: Encoder> Shape<E> {
    pub fn is_empty(&self) -> bool {
        match self {
            Shape::Empty => true,
            Shape::Compound(v) => v.is_empty(),
            Shape::Simple(_) => false
        }
    }
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

pub trait Font<E: Encoder> {
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
    fn glyph(&self, gid: GlyphId) -> Option<&Glyph<E>>;
    
    fn is_empty_glyph(&self, gid: GlyphId) -> bool;

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
}

pub trait Pen {
    fn move_to(&mut self, p: Vector2F);
    fn line_to(&mut self, p: Vector2F);
    fn quad_to(&mut self, p1: Vector2F, p2: Vector2F);
    fn cubic_to(&mut self, p1: Vector2F, p2: Vector2F, p3: Vector2F);
    fn close(&mut self);
}

pub trait Encoder {
    type Pen<'a>: Pen;
    type GlyphRef: Clone;

    fn encode_shape<'f, O, E>(&mut self, f: impl for<'a> FnMut(&mut Self::Pen<'a>) -> Result<O, E> + 'f) -> Result<(O, Self::GlyphRef), E>;
}

pub mod pathfinder_impl {
    use pathfinder_content::outline::{Outline, Contour};
    use pathfinder_geometry::vector::Vector2F;

    use crate::Pen;
    pub struct PathBuilder {
        outline: Outline,
        current_contour: Contour,
    }
    impl PathBuilder {
        pub fn new() -> Self {
            PathBuilder { outline: Outline::new(), current_contour: Contour::new() }
        }
        pub fn finish(mut self) -> Outline {
            if self.current_contour.len() > 0 {
                self.outline.push_contour(self.current_contour.clone());
            }
            self.outline
        }
    }

    impl Pen for PathBuilder {
        fn move_to(&mut self, p: Vector2F) {
            if self.current_contour.len() > 0 {
                self.outline.push_contour(self.current_contour.clone());
                self.current_contour.clear();
            }
            self.current_contour.push_endpoint(p);
        }

        fn line_to(&mut self, p: Vector2F) {
            self.current_contour.push_endpoint(p);
        }

        fn quad_to(&mut self, p1: Vector2F, p2: Vector2F) {
            self.current_contour.push_quadratic(p1, p2);
        }

        fn cubic_to(&mut self, p1: Vector2F, p2: Vector2F, p3: Vector2F) {
            self.current_contour.push_cubic(p1, p2, p3);
        }

        fn close(&mut self) {
            self.current_contour.close();
            self.outline.push_contour(self.current_contour.clone());
            self.current_contour.clear();
        }
    }
}

#[macro_use]
mod macros;
#[macro_use]
mod error;

#[cfg(feature="opentype")]
mod truetype;
#[cfg(feature="cff")]
mod cff;
#[cfg(feature="type1")]
mod type1;
#[cfg(feature="type2")]
mod type2;
#[cfg(feature="postscript")]
mod postscript;
#[cfg(feature="opentype")]
pub mod opentype;
mod parsers;
mod eexec;

#[cfg(feature="woff")]
mod woff;

#[cfg(feature="svg")]
mod svg;

pub use error::FontError;
#[cfg(feature="opentype")]
pub use truetype::TrueTypeFont;
#[cfg(feature="cff")]
pub use cff::CffFont;
#[cfg(feature="type1")]
pub use type1::Type1Font;
#[cfg(feature="opentype")]
pub use opentype::OpenTypeFont;

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

pub struct State<'a, P: Pen> {
    pub stack: Vec<Value>,
    pub pen: &'a mut P,
    pub current: Vector2F,
    pub lsb: Option<f32>,
    pub char_width: Option<f32>,
    pub done: bool,
    pub stem_hints: u32,
    pub delta_width: Option<f32>,
    pub first_stack_clearing_operator: bool,
    pub flex_sequence: Option<Vec<Vector2F>>
}

impl<'a, P: Pen> State<'a, P> {
    #[inline]
    pub fn new(pen: &'a mut P, mut stack: Vec<Value>) -> State<P> {
        stack.clear();
        State {
            stack,
            pen,
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
    pub fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into());
    }
    #[inline]
    pub fn pop(&mut self) -> Result<Value, FontError> {
        Ok(expect!(self.stack.pop(), "no value on the stack"))
    }
    #[inline]
    fn pop_tuple<T>(&mut self) -> Result<T, FontError> where
        T: TupleElements<Element=Value>
    {
        let range = self.stack.len() - T::N ..;
        Ok(expect!(T::from_iter(self.stack.drain(range)), "not enoug data on the stack"))
    }
    /// get stack[0 .. T::N] as a tuple
    /// does not modify the stack
    #[inline]
    pub fn args<T>(&mut self) -> Result<T, FontError> where
        T: TupleElements<Element=Value>
    {
        trace!("get {} args from {:?}", T::N, self.stack);
        Ok(expect!(T::from_iter(self.stack.iter().cloned()), "not enough data on the stack"))
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

pub enum FontVariant<E: Encoder> {
    #[cfg(feature="opentype")]
    OpenType(OpenTypeFont<E>),
    #[cfg(feature="opentype")]
    TrueType(TrueTypeFont<E>),
    #[cfg(feature="type1")]
    Type1(Type1Font<E>),
    #[cfg(feature="cff")]
    Cff(CffFont<E>),
}

macro_rules! impl_variants {
    ($(#[$meta:meta] $name:ident ($inner:ty),)*) => {
        impl<E: Encoder + 'static> Font<E> for FontVariant<E> {
            fn num_glyphs(&self) -> u32 {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.num_glyphs(), 
                )* }
            }
        
            fn font_matrix(&self) -> Transform2F {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.font_matrix(), 
                )* }
            }
        
            fn glyph(&self, gid: GlyphId) -> Option<&Glyph<E>> {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.glyph(gid), 
                )* }
            }
        
            fn is_empty_glyph(&self, gid: GlyphId) -> bool {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.is_empty_glyph(gid), 
                )* }
            }

            #[cfg(feature="svg")]
            fn svg_glyph(&self, gid: GlyphId) -> Option<&SvgGlyph> {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.svg_glyph(gid), 
                )* }
            }

            fn gid_for_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.gid_for_codepoint(codepoint), 
                )* }
            }

            fn gid_for_name(&self, name: &str) -> Option<GlyphId> {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.gid_for_name(name), 
                )* }
            }
            fn gid_for_unicode_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.gid_for_unicode_codepoint(codepoint), 
                )* }
            }
            fn encoding(&self) -> Option<Encoding> {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.encoding(), 
                )* }
            }
            fn get_notdef_gid(&self) -> GlyphId {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.get_notdef_gid(), 
                )* }
            }
            fn bbox(&self) -> Option<RectF> {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.bbox(), 
                )* }
            }
            fn vmetrics(&self) -> Option<VMetrics> {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.vmetrics(), 
                )* }
            }
            fn kerning(&self, left: GlyphId, right: GlyphId) -> f32 {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.kerning(left, right), 
                )* }
            }
        
            fn name(&self) -> &Name {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.name(), 
                )* }
            }
        
            fn info(&self) -> &Info {
                match self { $(
                    #[$meta]
                    FontVariant::$name(inner) => inner.info(), 
                )* }
            }
        }
    };
}
impl_variants!{
    #[cfg(feature="opentype")]
    OpenType(OpenTypeFont<E>),
    #[cfg(feature="opentype")]
    TrueType(TrueTypeFont<E>),
    #[cfg(feature="type1")]
    Type1(Type1Font<E>),
    #[cfg(feature="cff")]
    Cff(CffFont<E>),
}

pub fn parse<E: Encoder>(data: &[u8], encoder: &mut E) -> Result<FontVariant<E>, FontError> {
    let magic: &[u8; 4] = slice!(data, 0 .. 4).try_into().unwrap();
    info!("font magic: {:?} ({:?})", magic, String::from_utf8_lossy(&*magic));
    Ok(match magic {
        #[cfg(feature="type1")]
        &[0x80, 1, _, _] => FontVariant::Type1(t!(Type1Font::parse_pfb(data, encoder))),
        
        #[cfg(feature="opentype")]
        b"OTTO" | [0,1,0,0] => FontVariant::OpenType(t!(OpenTypeFont::parse(data, encoder))),
        
        b"ttcf" | b"typ1" => error!("FontCollections not implemented"), // Box::new(TrueTypeFont::parse(data, 0)) as _,
        
        #[cfg(feature="opentype")]
        b"true" => FontVariant::TrueType(t!(TrueTypeFont::parse(data, encoder))),
        
        #[cfg(feature="type1")]
        b"%!PS" => FontVariant::Type1(t!(Type1Font::parse_postscript(data, encoder))),

        #[cfg(feature="woff")]
        b"wOFF" => FontVariant::OpenType(t!(woff::parse_woff(data, encoder))),

        #[cfg(feature="woff")]
        b"wOF2" => FontVariant::OpenType(t!(woff::parse_woff2(data, encoder))),

        #[cfg(feature="cff")]
        &[1, _, _, _] => FontVariant::Cff(t!(CffFont::parse(data, 0, encoder))),
        
        #[cfg(feature="type1")]
        &[37, 33, _, _] => FontVariant::Type1(t!(Type1Font::parse_pfa(data, encoder))),

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
    let magic: &[u8; 4] = data[0 .. 4].try_into().ok()?;
    info!("font magic: {:?} ({:?})", magic, String::from_utf8_lossy(&*magic));
    match magic {
        #[cfg(feature="opentype")]
        b"OTTO" | [0,1,0,0] => opentype::info(data).ok(),
        _ => None
    }
}
