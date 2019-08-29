#![feature(thread_local, const_vec_new)]
#[macro_use] extern crate log;
#[macro_use] extern crate slotmap;

use std::fmt;
use std::convert::TryInto;
use nom::{IResult, Err::*, error::VerboseError};
use tuple::{TupleElements};
use encoding::Encoding;
use vector::{Outline, Vector, PathBuilder, Transform, Rect};

#[derive(Clone)]
pub struct Glyph<O: Outline> {
    /// unit 1em
    pub metrics: HMetrics,
    
    /// transform by font_matrix to scale it to 1em
    pub path: O 
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct GlyphId(pub u32);

#[derive(Copy, Clone)]
pub struct VMetrics {
    pub line_gap: f32
}
#[derive(Copy, Clone, Default)]
pub struct HMetrics {
    pub lsb: Vector,
    pub advance: Vector
}
pub trait Font<O: Outline> {
    /// Return the "number of glyphs" in the font.
    ///
    /// This may or may not correlate to the actual number of "real glyphs".
    /// It does however define the highest valid glyph id (*gid*) as `num_glyphs() - 1`
    fn num_glyphs(&self) -> u32;
    
    /// The transformation to get from glyph space (which all methods use) into text space with a unit of 1em.
    fn font_matrix(&self) -> Transform;
    
    /// Get the glyph identified by `gid`.
    ///
    /// Note, that a *gid* is only meaningful within one font and cannot be transfered to another font.
    fn glyph(&self, gid: GlyphId) -> Option<Glyph<O>>;
    
    /// Get all glyphs in this font
    fn glyphs(&self) -> Glyphs<O> {
        Glyphs {
            glyphs: (0 .. self.num_glyphs()).map(|i| self.glyph(GlyphId(i)).unwrap()).collect()
        }
    }
    
    /// Get the *gid* for the given codepoint in the "native encoding" of this font.
    ///
    /// (see `encoding()` to find out which that is).
    /// Returns None if there is no "native encoding", or the font does not contain a glyph for this codepoint.
    fn gid_for_codepoint(&self, codepoint: u32) -> Option<GlyphId> {
        None
    }
    
    /// Get the *gid* for the glyph with the given *name*.
    ///
    /// Returns None if the underlying font does not define any names, or does not contain a glyph with this name.
    fn gid_for_name(&self, name: &str) -> Option<GlyphId> {
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
    fn bbox(&self) -> Option<Rect> {
        None
    }
    
    /// Vertical metrics of the font (common across all glyphs)
    fn vmetrics(&self) -> Option<VMetrics> {
        None
    }
    
    /// Kerning distance for the given glyph pair
    fn kerning(&self, left: GlyphId, right: GlyphId) -> f32 {
        0.0
    }
    fn full_name(&self) -> Option<&str> {
        None
    }
    fn postscript_name(&self) -> Option<&str> {
        None
    }
}
pub struct Glyphs<O: Outline> {
    glyphs: Vec<Glyph<O>>
}
impl<O: Outline> Glyphs<O> {
    #[inline]
    pub fn get(&self, gid: GlyphId) -> Option<&Glyph<O>> {
        self.glyphs.get(gid.0 as usize)
    }
}

mod truetype;
mod cff;
mod type1;
mod type2;
mod postscript;
mod opentype;
mod parsers;
mod eexec;
mod woff;
mod gpos;
pub mod layout;

pub use truetype::TrueTypeFont;
pub use cff::CffFont;
pub use type1::Type1Font;
pub use opentype::{OpenTypeFont};
use woff::{woff, woff2};

pub type R<'a, T> = IResult<&'a [u8], T, VerboseError<&'a [u8]>>;

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
    fn to_int(self) -> i32 {
        match self {
            Value::Int(i) => i,
            Value::Float(_) => panic!("tried to cast a float to int")
        }
    }
    #[inline]
    fn to_uint(self) -> u32 {
        match self {
            Value::Int(i) if i >= 0 => i as u32,
            Value::Int(_) => panic!("expected a unsigned int"),
            Value::Float(_) => panic!("tried to cast a float to int")
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
fn v(x: impl Into<f32>, y: impl Into<f32>) -> Vector {
    Vector::new(x.into(), y.into())
}

pub trait TryIndex {
    fn try_index(&self, idx: usize) -> Option<&[u8]>;
}
impl TryIndex for () {
    #[inline]
    fn try_index(&self, idx: usize) -> Option<&[u8]> {
        None
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
    pub fn subr(&self, idx: i32) -> &[u8] {
        self.subrs.try_index((idx + self.subr_bias) as usize).expect("requested subroutine not found")
    }
    #[inline]
    pub fn global_subr(&self, idx: i32) -> &[u8] {
        self.global_subrs.try_index((idx + self.global_subr_bias) as usize).expect("requested global subroutine not found")
    }
}

pub struct State<O: Outline> {
    pub stack: Vec<Value>,
    pub path: PathBuilder<O>,
    pub current: Vector,
    pub lsb: Option<Vector>,
    pub char_width: Option<f32>,
    pub done: bool,
    pub stem_hints: u32,
    pub delta_width: Option<f32>,
    pub first_stack_clearing_operator: bool,
    pub flex_sequence: Option<Vec<Vector>>
}

impl<O: Outline> State<O> {
    #[inline]
    pub fn new() -> State<O> {
        State {
            stack: Vec::new(),
            path: PathBuilder::new(),
            current: Vector::default(),
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
        self.path.clear();
        self.current = Vector::default();
        self.lsb = None;
        self.char_width = None;
        self.done = false;
        self.stem_hints = 0;
        self.delta_width = None;
        self.first_stack_clearing_operator = true;
        self.flex_sequence = None;
    }
    #[inline]
    pub fn into_path(self) -> O {
        self.path.into_outline()
    }
    #[inline]
    pub fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into());
    }
    #[inline]
    pub fn pop(&mut self) -> Value {
        self.stack.pop().expect("no value on the stack")
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
    fn get(self) -> Self::Item;
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
    fn get(self) -> T {
        match self {
            Ok((_, t)) => t,
            Err(e) => print_err(e),
        }
    }
}

pub fn parse<O: Outline + 'static>(data: &[u8]) -> Box<dyn Font<O>> {
    let magic: &[u8; 4] = data[0 .. 4].try_into().unwrap();
    info!("font magic: {:?} ({:?})", magic, String::from_utf8_lossy(&*magic));
    match magic {
        &[0x80, 1, _, _] => Box::new(Type1Font::parse_pfb(data)) as _,
        b"OTTO" | [0,1,0,0] => Box::new(OpenTypeFont::parse(data)) as _,
        b"ttcf" | b"typ1" => unimplemented!(), // Box::new(TrueTypeFont::parse(data, 0)) as _,
        b"true" => Box::new(TrueTypeFont::parse(data)) as _,
        b"%!PS" => Box::new(Type1Font::parse_postscript(data)) as _,
        b"wOFF" => woff(data),
        b"wOF2" => woff2(data),
        &[1, _, _, _] => Box::new(CffFont::parse(data, 0)) as _,
        magic => panic!("unknown magic {:?}", magic)
    }
}
