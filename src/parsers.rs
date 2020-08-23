use std::fmt;
use std::marker::PhantomData;
use std::hash::Hash;
use nom::{
    bytes::complete::{take_till, take_till1, take_while, take_while_m_n, tag},
    number::complete::{be_u8, be_u16},
    sequence::{delimited, tuple, preceded, terminated},
    combinator::{opt, map, recognize},
    character::complete::{one_of, digit0, digit1},
    branch::alt,
    multi::many0,
    error::{make_error, ErrorKind},
    Err::Failure
};
use decorum::R32;
use crate::{R};
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
    map(
        recognize(tuple((
            opt(one_of("+-")),
            digit1
        ))),
        |s| std::str::from_utf8(s).unwrap().parse().unwrap()
    )(i)
}

pub fn plus_minus(i: &[u8]) -> R<&[u8]> {
    alt((tag("+"), tag("-")))(i)
}
pub fn float(i: &[u8]) -> R<f32> {
    map(
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
        |s| std::str::from_utf8(s).unwrap().parse::<f32>().expect("overflow")
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
        procedure("{-180 -293 1090 1010}readonly ".as_bytes()).get(),
        vec![
            Token::Int(-180),
            Token::Int(-293),
            Token::Int(1090),
            Token::Int(1010)
        ]
    );
    assert_eq!(
        procedure("{1 index exch /.notdef put} ".as_bytes()).get(),
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
pub fn varint_u32(i: &[u8]) -> R<u32> {
    let (mut i, b0) = be_u8(i)?;
    let mut acc = match b0 {
        0x80 => return Err(Failure(make_error(i, ErrorKind::Verify))),
        b if b < 0x80 => {
            return Ok((i, b as u32))
        }
        b => (b & 0x7F) as u32
    };
    for _ in 1 .. 5 {
        let b = parse(&mut i, be_u8)?;
        
        if acc & 0xFE_00_00_00 != 0 {
            return Err(Failure(make_error(i, ErrorKind::Verify)));
        }
        
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
    match b0 {
        253 => be_u16(i),
        254 => map(be_u8, |n| n as u16 + 2*253)(i),
        255 => map(be_u8, |n| n as u16 + 253)(i),
        n => Ok((i, n as u16))
    }
}

pub fn hex_string(input: &[u8]) -> R<Vec<u8>> {
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
            _ => return Ok((&input[idx ..], data))
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

pub fn count_map<'a, K, V>(parser: impl Fn(&'a [u8]) -> R<'a, (K, V)>, count: usize) -> impl Fn(&'a [u8]) -> R<'a, IndexMap<K, V>>
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
    Offset::parse(i)
}
pub struct Offset(pub u16);
impl Offset {
    pub fn of<'a>(&self, data: &'a [u8]) -> &'a [u8] {
        &data[self.0 as usize ..]
    }
}
impl Parser for Offset {
    type Output = Self;
    fn parse(i: &[u8]) -> R<Self::Output> {
        let (i, offset) = be_u16(i)?;
        Ok((i, Offset(offset)))
    }
}
impl FixedSize for Offset {
    const SIZE: usize = 2;
}
pub fn array_iter<'a, P: Parser + FixedSize>(input: &'a [u8], count: usize) -> R<impl Iterator<Item=P::Output> + ExactSizeIterator + 'a> {
    let (ours, remaining) = input.split_at(P::SIZE * count);
    let iter = ours.chunks(P::SIZE).map(|chunk| P::parse(chunk).unwrap().1);
    Ok((remaining, iter))
}
pub fn array<P: Parser + FixedSize, N: Into<usize>>(count: N) -> impl Fn(&[u8]) -> R<Array<P>> {
    let len = count.into();
    move |input| {
        let (data, remaining) = input.split_at(P::SIZE * len);
        Ok((remaining, Array {
            data,
            len,
            _m: PhantomData
        }))
    }
}
pub fn array_map<P: Parser + FixedSize, N: Into<usize>, F: Copy>(count: N, f: F) -> impl Fn(&[u8]) -> R<ArrayMap<P, F>> {
    let array = array(count);
    move |input| {
        let (i, arr) = array(input)?;
        Ok((i, arr.map(f)))
    }
}

#[derive(Clone)]
pub struct Array<'a, P> {
    data: &'a [u8],
    len: usize,
    _m: PhantomData<P>,
}
impl<'a, P: Parser + FixedSize> Array<'a, P> {
    pub fn iter(&self) -> impl Iterator<Item=P::Output> + ExactSizeIterator + 'a {
        self.data.chunks(P::SIZE).map(|chunk| P::parse(chunk).unwrap().1)
    }
    pub fn map<F>(self, f: F) -> ArrayMap<'a, P, F> {
        ArrayMap {
            array: self,
            map: f
        }
    }
}
impl<'a, P: Parser + FixedSize> KnownSize for Array<'a, P> {
    fn size(&self) -> usize {
        self.len * P::SIZE
    }
}
pub struct ArrayMap<'a, P, F> {
    array: Array<'a, P>,
    map: F,
}
impl<'a, P, F, T> ArrayMap<'a, P, F>
    where P: Parser + FixedSize + 'a, T: 'a, F: 'a + Fn(P::Output) -> T
{
    pub fn iter(self) -> impl Iterator<Item=T> + ExactSizeIterator + 'a {
        let map = self.map;
        self.array.iter().map(move |v| map(v))
    }
}
impl<'a, P: Parser + FixedSize, F> KnownSize for ArrayMap<'a, P, F> {
    fn size(&self) -> usize {
        self.array.size()
    }
}


pub trait Parser {
    type Output;
    fn parse(data: &[u8])-> R<Self::Output>;
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
