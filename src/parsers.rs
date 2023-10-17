use std::fmt;
use std::marker::PhantomData;
use std::hash::Hash;
use nom::{
    bytes::complete::{take, take_till, take_till1, take_while, take_while_m_n, tag},
    number::complete::{be_u8, be_u16},
    sequence::{delimited, tuple, preceded, terminated},
    combinator::{opt, map, recognize, map_res},
    character::complete::{one_of, digit0, digit1},
    branch::alt,
    multi::many0,
    error::{make_error, ErrorKind, VerboseError, ParseError},
    Err::Failure
};
use decorum::R32;
use crate::{R, FontError, ParseResult};
use indexmap::IndexMap;

fn special_char(b: u8) -> bool {
    match b {
        b'(' | b')' | b'<' | b'>' | b'[' | b']' | b'{' | b'}' | b'/' | b'%' => true,
        _ => false
    }
}

pub fn name(i: &[u8]) -> R<&[u8]> {
    alt((
        tag("["), tag("]"),
        take_till1(|b| word_sep(b) || special_char(b))
    ))(i)
}

pub fn literal_name(i: &[u8]) -> R<&[u8]> {
    preceded(
        tag("/"),
        take_till(|b| word_sep(b) || special_char(b))
    )(i)
}
#[test]
fn test_literal() {
    assert_eq!(
        literal_name(&b"/FontBBox{-180 -293 1090 1010}readonly def"[..]),
        Ok((&b"{-180 -293 1090 1010}readonly def"[..], &b"FontBBox"[..]))
    );
    assert_eq!(
        literal_name(&b"/.notdef "[..]),
        Ok((&b" "[..], &b".notdef"[..]))
    );
}

pub fn string(i: &[u8]) -> R<Vec<u8>> {
    delimited(
        tag("("),
        delimited_literal,
        tag(")")
    )(i)
}

pub fn integer(i: &[u8]) -> R<i32> {
    map_res(
        recognize(tuple((
            opt(one_of("+-")),
            digit1
        ))),
        |s| std::str::from_utf8(s).unwrap().parse()
    )(i)
}

pub fn plus_minus(i: &[u8]) -> R<&[u8]> {
    alt((tag("+"), tag("-")))(i)
}
pub fn float(i: &[u8]) -> R<f32> {
    map_res(
        recognize(tuple((
            opt(plus_minus),
            digit0,
            tag("."),
            digit0,
            opt(tuple((
                alt((tag("e"), tag("E"))),
                opt(plus_minus),
                digit1
            ))) 
        ))),
        |s| std::str::from_utf8(s).unwrap().parse::<f32>()
    )(i)
}
pub fn delimited_literal(i: &[u8]) -> R<Vec<u8>> {
    let mut level = 0;
    let mut out = Vec::new();
    let mut pos = 0;
    while let Some(&b) = i.get(pos) {
        match b {
            b')' => {
                if level == 0 {
                    break;
                }
                level -= 1;
                out.push(b);
                pos += 1;
            },
            b'(' => {
                level += 1;
                out.push(b);
                pos += 1;
            }
            b'\\' => {
                if let Some(&c) = i.get(pos+1) {
                    let r = match c {
                        b'n' => b'\n',
                        b'r' => b'\r',
                        b't' => b'\t',
                        b'b' => 8,
                        b'f' => 12,
                        b @ b'\n' | b @ b'\r' => {
                            match (b, i.get(pos+2)) {
                                (b'\n', Some(b'\r')) | (b'\r', Some(b'\n')) => pos += 3,
                                _ => pos += 2,
                            }
                            continue;
                        }
                        c => c
                    };
                    out.push(r);
                    pos += 2;
                } else {
                    break;
                }
            },
            _ => {
                out.push(b);
                pos += 1;
            }
        }
    }
    Ok((&i[pos ..], out))
}

pub fn take_until_and_consume(filter: impl Fn(u8) -> bool) -> impl Fn(&[u8]) -> R<&[u8]> {
    move |i: &[u8]| {
        let end = i.iter()
            .position(|&b| filter(b))
            .unwrap_or(i.len());
            
        let next = end + i[end ..].iter()
            .position(|&b| !filter(b))
            .unwrap_or(i.len());
        
        Ok((&i[next ..], &i[.. end]))
    }
}

pub fn line_sep(b: u8) -> bool {
    match b {
        b'\r' | b'\n' => true,
        _ => false
    }
}
pub fn word_sep(b: u8) -> bool {
    match b {
        b' ' | b'\t' | b'\r' | b'\n' => true,
        _ => false
    }
}
pub fn space(i: &[u8]) -> R<&[u8]> {
    take_while(word_sep)(i)
}

pub fn comment(i: &[u8]) -> R<&[u8]> {
    preceded(tag("%"), take_until_and_consume(line_sep))(i)
}

#[derive(PartialEq, Eq)]
pub enum Token<'a> {
    Int(i32),
    Real(R32),
    Literal(&'a [u8]),
    Name(&'a [u8]),
    String(Vec<u8>),
    Procedure(Vec<Token<'a>>)
}

impl<'a> fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Int(i) => i.fmt(f),
            Token::Real(r) => r.fmt(f),
            Token::Literal(ref s) => write!(f, "/{}", String::from_utf8_lossy(&s)),
            Token::Name(ref s) => write!(f, "{}", String::from_utf8_lossy(&s)),
            Token::String(ref data) => write!(f, "({:?})", String::from_utf8_lossy(data)),
            Token::Procedure(ref vec) => f.debug_set().entries(vec).finish(),
        }
    }
}

fn procedure(i: &[u8]) -> R<Vec<Token>> {
    delimited(
        tag("{"),
        many0(
            preceded(
                space,
                token
            ),
        ),
        preceded(
            space,
            tag("}")
        )
    )(i)
}
#[test]
fn test_procedure() {
    use crate::IResultExt;
    assert_eq!(
        procedure("{-180 -293 1090 1010}readonly ".as_bytes()).get().unwrap(),
        vec![
            Token::Int(-180),
            Token::Int(-293),
            Token::Int(1090),
            Token::Int(1010)
        ]
    );
    assert_eq!(
        procedure("{1 index exch /.notdef put} ".as_bytes()).get().unwrap(),
        vec![
            Token::Int(1),
            Token::Name("index".as_bytes()),
            Token::Name("exch".as_bytes()),
            Token::Literal(".notdef".as_bytes()),
            Token::Name("put".as_bytes()),
        ]
    );
}

pub fn token(i: &[u8]) -> R<Token> {
    terminated(
        alt((
            map(float, |f| Token::Real(f.into())),
            map(integer, |i| Token::Int(i)),
            map(literal_name, |s| Token::Literal(s)),
            map(procedure, |v| Token::Procedure(v)),
            map(string, |v| Token::String(v)),
            map(name, |s| Token::Name(s))
        )),
        take_while_m_n(0, 1, word_sep)
    )(i)
}

pub struct ParserIterator<'a, T, F> {
    parser: F,
    input: &'a [u8],
    _m: PhantomData<T>
}
impl<'a, T, F: Clone> Clone for ParserIterator<'a, T, F> {
    fn clone(&self) -> Self {
        ParserIterator {
            parser: self.parser.clone(),
            .. *self
        }
    }
}
pub fn iterator<'a, T, F>(input: &'a [u8], parser: F) -> ParserIterator<'a, T, F> where
    F: Fn(&'a [u8]) -> R<'a, T>
{
    ParserIterator { parser, input, _m: PhantomData }
}
impl<'a, T, F> Iterator for ParserIterator<'a, T, F> where
    F: Fn(&'a [u8]) -> R<'a, T>
{
    type Item = T;
    #[inline(always)]
    fn next(&mut self) -> Option<T> {
        match (self.parser)(self.input) {
            Ok((i, t)) => {
                self.input = i;
                Some(t)
            }
            Err(_) => None
        }
    }
}
pub fn iterator_n<'a, T, F>(input: &'a [u8], parser: F, n: impl Into<usize>) -> impl Iterator<Item=T> + 'a where
    F: 'a + Fn(&'a [u8]) -> R<'a, T>, T: 'a
{
    ParserIterator { parser, input, _m: PhantomData }.take(n.into())
}

#[inline(always)]
pub fn varint_u32(i: &[u8]) -> ParseResult<u32> {
    let (mut i, b0) = be_u8(i)?;
    let mut acc = match b0 {
        0x80 => error!("0x80 not allowed"),
        b if b < 0x80 => {
            return Ok((i, b as u32))
        }
        b => (b & 0x7F) as u32
    };
    for _ in 1 .. 5 {
        let b = parse(&mut i, be_u8)?;
        
        require_eq!(acc & 0xFE_00_00_00, 0);
        
        acc = acc << 7 | (b & 0x7F) as u32;
        if b & 0x80 == 0 {
            break;
        }
    }
    Ok((i, acc))
}

#[inline(always)]
pub fn varint_u16(i: &[u8]) -> R<u16> {
    let (i, b0) = be_u8(i)?;
    Ok(match b0 {
        253 => be_u16(i)?,
        254 => map(be_u8, |n| n as u16 + 2*253)(i)?,
        255 => map(be_u8, |n| n as u16 + 253)(i)?,
        n => (i, n as u16)
    })
}

pub fn hex_string(input: &[u8]) -> ParseResult<Vec<u8>> {
    let mut data = Vec::new();
    let mut odd = None;
    let mut add_digit = |digit: u8| {
        odd = match odd {
            Some(high) => {
                data.push(high << 4 | digit);
                None
            }
            None => Some(digit)
        };
    };
    for (idx, &b) in input.iter().enumerate() {
        match b {
            b @ b'0' ..= b'9' => add_digit(b - b'0'),
            b @ b'A' ..= b'F' => add_digit(b - b'A' + 10),
            b' ' | b'\n' | b'\t' => {},
            _ => return Ok((slice!(input, idx ..), data))
        }
    }
    Ok((input, data))
}

#[inline(always)]
pub fn parse<'a, T, E>(input: &mut &'a [u8], parser: impl Fn(&'a [u8]) -> Result<(&'a [u8], T), E>) -> Result<T, E> {
    let (i, t) = parser(*input)?;
    *input = i;
    Ok(t)
}

pub fn count<'a, T>(parser: impl Fn(&'a [u8]) -> ParseResult<T>, count: usize) -> impl Fn(&'a [u8]) -> ParseResult<Vec<T>>
{
    move |mut i: &[u8]| {
        let mut vec = Vec::with_capacity(count);
        for _ in 0 .. count {
            let t = parse(&mut i, &parser)?; // don't steal my parser!
            vec.push(t);
        }
        Ok((i, vec))
    }
}
pub fn count_map<'a, K, V>(parser: impl Fn(&[u8]) -> ParseResult<(K, V)>, count: usize) -> impl Fn(&[u8]) -> ParseResult<IndexMap<K, V>>
    where K: Hash + Eq
{
    move |mut i: &[u8]| {
        let mut map = IndexMap::with_capacity(count);
        for _ in 0 .. count {
            let (k, v) = parse(&mut i, &parser)?; // don't steal my parser!
            map.insert(k, v);
        }
        Ok((i, map))
    }
}
pub fn offset(i: &[u8]) -> R<Offset> {
    let (i, offset) = be_u16(i)?;
    Ok((i, Offset(offset)))
}
pub struct Offset(pub u16);
impl Offset {
    pub fn of<'a>(&self, data: &'a [u8]) -> Result<&'a [u8], FontError> {
        let off = self.0 as usize;
        if off != 0 && off <= data.len() {
            Ok(&data[off ..])
        } else {
            Err(FontError::OutOfBounds("offset", self.0 as _))
        }
    }
}
impl NomParser for Offset {
    type Output = Self;
    fn parse2(i: &[u8]) -> ParseResult<Self::Output> {
        let (i, offset) = be_u16(i)?;
        Ok((i, Offset(offset)))
    }
}
impl FixedSize for Offset {
    const SIZE: usize = 2;
}
pub fn array_iter<'a, P: Parser + FixedSize>(input: &'a [u8], count: usize)
-> ParseResult<'a, impl Iterator<Item=Result<P::Output, FontError>> + ExactSizeIterator + 'a> {
    let (ours, remaining) = input.split_at(P::SIZE * count);
    let iter = ours.chunks(P::SIZE).map(|chunk| P::parse(chunk));
    Ok((remaining, iter))
}
pub fn array<P: Parser + FixedSize, N: Into<usize>>(count: N) -> impl Fn(&[u8]) -> R<ArrayBase<P>> {
    let len = count.into();
    move |input| {
        let n = P::SIZE * len;
        if input.len() < n {
            return Err(nom::Err::Failure(VerboseError::from_error_kind(input, ErrorKind::Complete)));
        }
        let (data, remaining) = input.split_at(n);
        Ok((remaining, ArrayBase {
            data,
            len,
            _m: PhantomData
        }))
    }
}

pub trait Array: Sized {
    type Item;
    type Iter: Iterator<Item=Result<Self::Item, FontError>> + ExactSizeIterator;

    fn len(&self) -> usize;
    fn get(&self, idx: usize) -> Result<Self::Item, FontError>;
    fn iter(&self) -> Self::Iter;
    fn map<F>(self, f: F) -> ArrayMap<Self, F> {
        ArrayMap { base: self, map: f }
    }
}

#[derive(Clone)]
pub struct ArrayBase<'a, P> {
    data: &'a [u8],
    len: usize,
    _m: PhantomData<P>,
}
pub struct ArrayBaseIter<'a, P: Parser> {
    data: &'a [u8],
    _m: PhantomData<P>,
}
impl<'a, P: Parser + FixedSize> Iterator for ArrayBaseIter<'a, P> {
    type Item = Result<P::Output, FontError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.data.len() < P::SIZE {
            return None;
        }
        let (this, remaining) = self.data.split_at(P::SIZE);
        self.data = remaining;
        Some(P::parse(this))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.data.len() / P::SIZE;
        (n, Some(n))
    }
}
impl<'a, P: Parser + FixedSize> ExactSizeIterator for ArrayBaseIter<'a, P> {}

impl<'a, P: Parser + FixedSize> Array for ArrayBase<'a, P> {
    type Item = P::Output;
    type Iter = ArrayBaseIter<'a, P>; //impl Iterator<Item=Result<Self::Item, FontError>> + ExactSizeIterator;

    fn len(&self) -> usize {
        self.len
    }
    fn get(&self, idx: usize) -> Result<Self::Item, FontError> {
        let off = P::SIZE * idx;
        P::parse(slice!(self.data, off .. off + P::SIZE))
    }
    fn iter(&self) -> Self::Iter {
        ArrayBaseIter { data: self.data, _m: PhantomData }
        //self.data.chunks_exact(P::SIZE).map(|chunk| P::parse(chunk))
    }
}
impl<'a, P: Parser + FixedSize> KnownSize for ArrayBase<'a, P> {
    fn size(&self) -> usize {
        self.len * P::SIZE
    }
}
pub struct ArrayMap<A, F> {
    base: A,
    map: F,
}
impl<A: Array, F, T> Array for ArrayMap<A, F> where F: Fn(A::Item) -> Result<T, FontError> + Clone
{
    type Item = T;
    type Iter = ArrayMapIter<A, F>;

    fn len(&self) -> usize {
        self.base.len()
    }

    fn get(&self, idx: usize) -> Result<Self::Item, FontError> {
        self.base.get(idx).and_then(|v| (self.map)(v))
    }
    fn iter(&self) -> Self::Iter {
        ArrayMapIter { base: self.base.iter(), map: self.map.clone() }
    }
}
impl<A: FixedSize, F> KnownSize for ArrayMap<A, F> {
    fn size(&self) -> usize {
        self.base.size()
    }
}
pub struct ArrayMapIter<A: Array, F> {
    base: A::Iter,
    map: F,
}
impl<A: Array, F, T> Iterator for ArrayMapIter<A, F> where F: Fn(A::Item) -> Result<T, FontError> {
    type Item = Result<T, FontError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.base.next().map(|r| r.and_then(|v| (self.map)(v)))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.base.size_hint()
    }
}
impl<A: Array, F, T> ExactSizeIterator for ArrayMapIter<A, F> where F: Fn(A::Item) -> Result<T, FontError> {}


pub trait Parser {
    type Output;
    fn parse(data: &[u8]) -> Result<Self::Output, FontError>;
}
pub trait NomParser {
    type Output;
    fn parse2(i: &[u8]) -> ParseResult<Self::Output>;
}
pub trait FixedSize {
    const SIZE: usize;
}
pub trait KnownSize {
    fn size(&self) -> usize;
}
impl<T: FixedSize> KnownSize for T {
    fn size(&self) -> usize {
        Self::SIZE
    }
}
impl<P: NomParser> Parser for P {
    type Output = <P as NomParser>::Output;
    fn parse(data: &[u8]) -> Result<Self::Output, FontError> {
        P::parse2(data).map(|(_, v)| v)
    }
}
pub fn pascal_string(i: &[u8]) -> ParseResult<&[u8]> {
    let (i, len) = be_u8(i)?;
    let (i, data) = take(len)(i)?;
    Ok((i, data))
}