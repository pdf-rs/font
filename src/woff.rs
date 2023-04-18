use std::convert::TryInto;
use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{Read, Cursor};
use inflate::inflate_bytes_zlib;
use brotli_decompressor::{Decompressor};

use crate::{
    R, IResultExt, FontError, ParseResult,
    truetype::{Shape, contour, compound, parse_shapes},
    parsers::{iterator, varint_u32, varint_u16, parse, count_map},
    opentype::{Tables, parse_head, parse_hhea, parse_maxp, parse_hmtx, parse_hmtx_woff2_format1, parse_loca, OpenTypeFont},
};
use pathfinder_content::outline::{Outline};
use pathfinder_geometry::vector::Vector2F;
use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16, be_i16, be_u32},
    multi::count,
    bits::{bits, complete::take as take_bits},
    sequence::tuple,
    combinator::map
};


pub fn parse_woff(data: &[u8]) -> Result<OpenTypeFont, FontError> {
    let (i, _) = tag(b"wOFF")(data)?;
    let (i, flavor) = take(4usize)(i)?;
    let (i, _length) = be_u32(i)?;
    let (i, num_tables) = be_u16(i)?;
    let (i, _reserved) = be_u16(i)?;
    let (i, _total_sfnt_size) = be_u32(i)?;
    let (i, _major_version) = be_u16(i)?;
    let (i, _minor_version) = be_u16(i)?;
    let (i, _meta_offset) = be_u32(i)?;
    let (i, _meta_length) = be_u32(i)?;
    let (i, _meta_orig_length) = be_u32(i)?;
    let (i, _priv_offset) = be_u32(i)?;
    let (i, _priv_length) = be_u32(i)?;
    
    let (mut i, tables_dir) = count(woff_dir_entry, num_tables as usize)(i)?;
    
    if flavor == b"ttcf" {
        unimplemented!()
    }
    
    let mut tables = HashMap::with_capacity(num_tables as usize);
    for entry in tables_dir {
        debug!("{}", String::from_utf8_lossy(&entry.tag));
        let data = if entry.comp_length < entry.orig_length {
            let compressed_data = parse(&mut i, take((entry.comp_length as usize + 3) & !3))?;
            let orig_data = inflate_bytes_zlib(slice!(compressed_data, .. entry.comp_length as usize)).expect("can't decompress data");
            require_eq!(orig_data.len(), entry.orig_length as usize);
            Cow::Owned(orig_data)
        } else {
            let chunk = parse(&mut i, take((entry.orig_length as usize + 3) & !3))?;
            Cow::Borrowed(slice!(chunk, .. entry.orig_length as usize))
        };
        tables.insert(entry.tag, data);
    }
    
    let tables = Tables { entries: tables };
    OpenTypeFont::from_tables(tables)
}

#[derive(Debug)]
struct WoffDirEntry {
    tag: [u8; 4],
    offset: u32,
    comp_length: u32,
    orig_length: u32,
}

fn woff_dir_entry(i: &[u8]) -> R<WoffDirEntry> {
    let (i, tag) = take(4usize)(i)?;
    let (i, offset) = be_u32(i)?;
    let (i, comp_length) = be_u32(i)?;
    let (i, orig_length) = be_u32(i)?;
    let (i, _checksum) = be_u32(i)?;
    Ok((i, WoffDirEntry {
        tag: tag.try_into().unwrap(), offset, comp_length, orig_length
    }))
}
    
pub fn parse_woff2(i: &[u8]) -> Result<OpenTypeFont, FontError> {
    let (i, _) = tag(b"wOF2")(i)?;
    let (i, flavor) = take(4usize)(i)?;
    let (i, _length) = be_u32(i)?;
    let (i, num_tables) = be_u16(i)?;
    let (i, _reserved) = be_u16(i)?;
    let (i, _total_sfnt_size) = be_u32(i)?;
    let (i, total_compressed_size) = be_u32(i)?;
    let (i, _major_version) = be_u16(i)?;
    let (i, _minor_version) = be_u16(i)?;
    let (i, _meta_offset) = be_u32(i)?;
    let (i, _meta_length) = be_u32(i)?;
    let (i, _meta_orig_length) = be_u32(i)?;
    let (i, _priv_offset) = be_u32(i)?;
    let (i, _priv_length) = be_u32(i)?;

    let (i, entry_tables) = count_map(woff2_table_entry, num_tables as usize)(i)?;
    
    if flavor == b"ttcf" {
        error!("FontCollections are not implemented yet");
    }
    
    let mut decompressor = Decompressor::new(Cursor::new(slice!(i,  .. total_compressed_size as usize)), 1024);
    let mut entries = HashMap::new();
    for (&tag, entry) in &entry_tables {
        debug!("tag: {:?} ({:?}) {:?}", tag, std::str::from_utf8(&tag), entry);
        let mut buf = vec![0; entry.length as usize];
        decompressor.read_exact(&mut buf).expect("decode failed");
        entries.insert(tag, buf);
    }
    require_eq!(decompressor.bytes().count(), 0);
    
    let tables = Tables { entries };
    
    let hmtx = tables.get(b"hmtx").map(|hmtx_data| {
        let head = parse_head(tables.get(b"head").expect("no head"))?;
        let hhea = parse_hhea(tables.get(b"hhea").expect("no hhea"))?;
        let maxp = parse_maxp(tables.get(b"maxp").expect("no maxp"))?;
        match entry_tables[b"hmtx"].flags {
            0 => parse_hmtx(&hmtx_data, &hhea, &maxp).get(),
            1 => parse_hmtx_woff2_format1(&hmtx_data, &head, &hhea, &maxp).get(),
            f => error!("invalid flag for hmtx: {}", f)
        }
    }).transpose()?;
    
    let glyf = tables.get(b"glyf").map(|glyf_data| {
        match entry_tables[b"glyf"].flags {
            0 => parse_glyf_t0(&glyf_data),
            3 => {
                let head = parse_head(tables.get(b"head").expect("no head"))?;
                let maxp = parse_maxp(tables.get(b"maxp").expect("no maxp"))?;
                let loca = parse_loca(tables.get(b"loca").expect("no loca"), &head, &maxp)?;
                parse_shapes(&loca, glyf_data)
            }
            f => error!("invalid flag for glyf: {}", f)
        }
    }).transpose()?;

    OpenTypeFont::from_hmtx_glyf_and_tables(hmtx, glyf, tables)
}

fn parse_glyf_t0(i: &[u8]) -> Result<Vec<Shape>, FontError> {
    let (i, _) = tag([0u8; 4])(i)?;
    let (i, num_glyphs) = be_u16(i)?;
    let (i, _index_format) = be_u16(i)?;
    let (i, n_contour_stream_size) = be_u32(i)?;
    let (i, n_points_stream_size) = be_u32(i)?;
    let (i, flag_stream_size) = be_u32(i)?;
    let (i, glyph_stream_size) = be_u32(i)?;
    let (i, composite_stream_size) = be_u32(i)?;
    let (i, bbox_stream_size) = be_u32(i)?;
    let (i, instruction_stream_size) = be_u32(i)?;
    
    let (i, n_contour_stream) = take(n_contour_stream_size)(i)?;
    let (i, n_points_stream) = take(n_points_stream_size)(i)?;
    let (i, flag_stream) = take(flag_stream_size)(i)?;
    let (i, mut glyph_stream) = take(glyph_stream_size)(i)?;
    let (i, mut composite_stream) = take(composite_stream_size)(i)?;
    
    let bbox_bitmap_len = (num_glyphs as u32 + 7) / 8;
    let (i, _bbox_bitmap) = take(bbox_bitmap_len)(i)?;
    let (i, _bbox_stream) = take(bbox_stream_size - bbox_bitmap_len)(i)?;
    let (i, _instruction_stream) = take(instruction_stream_size)(i)?;
    
    let contours = iterator(n_contour_stream, be_i16);
    let mut points = iterator(n_points_stream, varint_u16);
    let mut flags = iterator(flag_stream, be_u8);
    
    let mut glyphs = Vec::with_capacity(num_glyphs as usize);
    
    for n_contour in contours {
        match n_contour {
            0 => glyphs.push(Shape::Empty),
            -1 => glyphs.push(parse(&mut composite_stream, compound)?),
            n if n > 0 => {
                let mut outline = Outline::new();
                let (mut x, mut y) = (0i16, 0i16);
                for n_points in (&mut points).take(n_contour as usize) {
                    let mut counter = 0;
                    let points = (&mut flags).take(n_points as usize).map(|flag| {
                        let on_curve = flag & 0x80 == 0;
                        let triplet = TRIPLET_LUT[(flag & 0x7F) as usize];
                        use nom::error::{ErrorKind};
                        //debug!("{:02x} {:02x} {:02x} {:02x}", glyph_stream[0], glyph_stream[1], glyph_stream[2], glyph_stream[3]);
                        let (vx, vy): (i16, i16) = parse(&mut glyph_stream,
                            bits::<_, _, (_, ErrorKind), (_, ErrorKind), _>(
                                tuple((
                                    take_bits(triplet.x_bits()),
                                    take_bits(triplet.y_bits())
                                ))
                            )
                        )?;
                        let sign = |is_minus| if is_minus { -1 } else { 1 };
                        x += sign(triplet.x_minus()) * (vx + triplet.dx() as i16);
                        y += sign(triplet.y_minus()) * (vy + triplet.dy() as i16);
                        counter += 1;
                        
                        (on_curve, Vector2F::new(x as f32, y as f32))
                    });
                    if let Some(contour) = contour(points) {
                        outline.push_contour(contour);
                    }
                    // don't care about it… but it needs to be removed from the stream
                    require_eq!(counter, n_points);
                }
                glyphs.push(Shape::Simple(outline));
                let _num_instructions = parse(&mut glyph_stream, varint_u16)?;
            }
            _ => panic!("invalid number of contours")
        }
    }
    
    Ok(glyphs)
}

#[derive(Debug)]
struct Entry {
    flags: u8,
    length: u32,
    orig_length: u32
}

fn woff2_table_entry(i: &[u8]) -> ParseResult<([u8; 4], Entry)> {
    let (i, b0) = be_u8(i)?;
    let table_type = b0 & 63;
    let flags = b0 >> 6;
    
    let (i, tag) = match table_type {
        63 => map(take(4usize), |s: &[u8]| s.try_into().unwrap())(i)?,
        n => (i, TABLE_TAGS[n as usize])
    };
    
    let (i, orig_length) = varint_u32(i)?;
    let is_null_transform = match &tag {
        b"glyf" => flags == 3,
        b"loca" => flags == 3,
        b"hmtx" => flags == 0,
        _ => flags == 0
    };
    
    let (i, length) = match is_null_transform {
        true => (i, orig_length),
        false => varint_u32(i)?
    };
    
    Ok((i, (tag, Entry {
        flags,
        length,
        orig_length
    })))
}

static TABLE_TAGS: [[u8; 4]; 63] = [
    *b"cmap", *b"head", *b"hhea", *b"hmtx", *b"maxp", *b"name", *b"OS/2", *b"post",
    *b"cvt ", *b"fpgm", *b"glyf", *b"loca", *b"prep", *b"CFF ", *b"VORG", *b"EBDT",
    *b"EBLC", *b"gasp", *b"hdmx", *b"kern", *b"LTSH", *b"PCLT", *b"VDMX", *b"vhea",
    *b"vmtx", *b"BASE", *b"GDEF", *b"GPOS", *b"GSUB", *b"EBSC", *b"JSTF", *b"MATH",
    *b"CBDT", *b"CBLC", *b"COLR", *b"CPAL", *b"SVG ", *b"sbix", *b"acnt", *b"avar",
    *b"bdat", *b"bloc", *b"bsln", *b"cvar", *b"fdsc", *b"feat", *b"fmtx", *b"fvar",
    *b"gvar", *b"hsty", *b"just", *b"lcar", *b"mort", *b"morx", *b"opbd", *b"prop",
    *b"trak", *b"Zapf", *b"Silf", *b"Glat", *b"Gloc", *b"Feat", *b"Sill"
];

macro_rules! is_minus {
    (-) => (1);
    (+) => (0);
}

macro_rules! lut {
    ($($bytes:tt $xbits:tt $ybits:tt $dx:tt $dy:tt $xsign:tt $ysign:tt; )*) => (
        [ $( Triplet (
            ($bytes - 2)      <<  0 |        // 2 bits
            ($xbits / 4)      <<  2 |        // 3 bits
            ($ybits / 4)      <<  5 |        // 3 bits
            ($dx)             <<  8 |        // 11 bits 
            ($dy)             << 19 |        // 11 bits
            is_minus!($xsign) << 30 |        // 1 bit
            is_minus!($ysign) << 31          // 1 bit
        ) ),* ]
    )
}

#[derive(Copy, Clone)]
pub struct Triplet(u32);
impl Triplet {
    pub fn num_bytes(&self) -> usize {
        2 + (self.0 & 0b11) as usize
    }
    pub fn x_bits(&self) -> u8 {
        4 * ((self.0 >> 2) & 0b111) as u8
    }
    pub fn y_bits(&self) -> u8 {
        4 * ((self.0 >> 5) & 0b111) as u8
    }
    pub fn dx(&self) -> u16 {
        ((self.0 >> 8) & 0b111_1111_1111) as u16
    }
    pub fn dy(&self) -> u16 {
        ((self.0 >> 19) & 0b111_1111_1111) as u16
    }
    pub fn x_minus(&self) -> bool {
        self.0 & (1 << 30) != 0
    }
    pub fn y_minus(&self) -> bool {
        self.0 & (1 << 31) != 0
    }
}

pub static TRIPLET_LUT: [Triplet; 128] = lut!( /*
bytes   xbits   ybits   dx      dy      xsign   ysign */
2	0	8	0	0	+	-;
2	0	8	0	0	+	+;
2	0	8	0	256	+	-;
2	0	8	0	256	+	+;
2	0	8	0	512	+	-;
2	0	8	0	512	+	+;
2	0	8	0	768	+	-;
2	0	8	0	768	+	+;
2	0	8	0	1024	+	-;
2	0	8	0	1024	+	+;
2	8	0	0	0	-	+;
2	8	0	0	0	+	+;
2	8	0	256	0	-	+;
2	8	0	256	0	+	+;
2	8	0	512	0	-	+;
2	8	0	512	0	+	+;
2	8	0	768	0	-	+;
2	8	0	768	0	+	+;
2	8	0	1024	0	-	+;
2	8	0	1024	0	+	+;
2	4	4	1	1	-	-;
2	4	4	1	1	+	-;
2	4	4	1	1	-	+;
2	4	4	1	1	+	+;
2	4	4	1	17	-	-;
2	4	4	1	17	+	-;
2	4	4	1	17	-	+;
2	4	4	1	17	+	+;
2	4	4	1	33	-	-;
2	4	4	1	33	+	-;
2	4	4	1	33	-	+;
2	4	4	1	33	+	+;
2	4	4	1	49	-	-;
2	4	4	1	49	+	-;
2	4	4	1	49	-	+;
2	4	4	1	49	+	+;
2	4	4	17	1	-	-;
2	4	4	17	1	+	-;
2	4	4	17	1	-	+;
2	4	4	17	1	+	+;
2	4	4	17	17	-	-;
2	4	4	17	17	+	-;
2	4	4	17	17	-	+;
2	4	4	17	17	+	+;
2	4	4	17	33	-	-;
2	4	4	17	33	+	-;
2	4	4	17	33	-	+;
2	4	4	17	33	+	+;
2	4	4	17	49	-	-;
2	4	4	17	49	+	-;
2	4	4	17	49	-	+;
2	4	4	17	49	+	+;
2	4	4	33	1	-	-;
2	4	4	33	1	+	-;
2	4	4	33	1	-	+;
2	4	4	33	1	+	+;
2	4	4	33	17	-	-;
2	4	4	33	17	+	-;
2	4	4	33	17	-	+;
2	4	4	33	17	+	+;
2	4	4	33	33	-	-;
2	4	4	33	33	+	-;
2	4	4	33	33	-	+;
2	4	4	33	33	+	+;
2	4	4	33	49	-	-;
2	4	4	33	49	+	-;
2	4	4	33	49	-	+;
2	4	4	33	49	+	+;
2	4	4	49	1	-	-;
2	4	4	49	1	+	-;
2	4	4	49	1	-	+;
2	4	4	49	1	+	+;
2	4	4	49	17	-	-;
2	4	4	49	17	+	-;
2	4	4	49	17	-	+;
2	4	4	49	17	+	+;
2	4	4	49	33	-	-;
2	4	4	49	33	+	-;
2	4	4	49	33	-	+;
2	4	4	49	33	+	+;
2	4	4	49	49	-	-;
2	4	4	49	49	+	-;
2	4	4	49	49	-	+;
2	4	4	49	49	+	+;
3	8	8	1	1	-	-;
3	8	8	1	1	+	-;
3	8	8	1	1	-	+;
3	8	8	1	1	+	+;
3	8	8	1	257	-	-;
3	8	8	1	257	+	-;
3	8	8	1	257	-	+;
3	8	8	1	257	+	+;
3	8	8	1	513	-	-;
3	8	8	1	513	+	-;
3	8	8	1	513	-	+;
3	8	8	1	513	+	+;
3	8	8	257	1	-	-;
3	8	8	257	1	+	-;
3	8	8	257	1	-	+;
3	8	8	257	1	+	+;
3	8	8	257	257	-	-;
3	8	8	257	257	+	-;
3	8	8	257	257	-	+;
3	8	8	257	257	+	+;
3	8	8	257	513	-	-;
3	8	8	257	513	+	-;
3	8	8	257	513	-	+;
3	8	8	257	513	+	+;
3	8	8	513	1	-	-;
3	8	8	513	1	+	-;
3	8	8	513	1	-	+;
3	8	8	513	1	+	+;
3	8	8	513	257	-	-;
3	8	8	513	257	+	-;
3	8	8	513	257	-	+;
3	8	8	513	257	+	+;
3	8	8	513	513	-	-;
3	8	8	513	513	+	-;
3	8	8	513	513	-	+;
3	8	8	513	513	+	+;
4	12	12	0	0	-	-;
4	12	12	0	0	+	-;
4	12	12	0	0	-	+;
4	12	12	0	0	+	+;
5	16	16	0	0	-	-;
5	16	16	0	0	+	-;
5	16	16	0	0	-	+;
5	16	16	0	0	+	+;
);
