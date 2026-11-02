use crate::{R, FontError};
use nom::{
    number::complete::{be_u16, be_u32},
};
use std::borrow::Cow;
use svg_dom::{Svg, Item};
use std::collections::HashMap;
use std::sync::Arc;
use std::fmt;

#[derive(Clone)]
pub struct SvgGlyph {
    pub svg: Arc<Svg>,
    pub item: Arc<Item>,
}
impl fmt::Debug for SvgGlyph {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SVG")
    }
}

#[derive(Clone)]
pub struct SvgTable {
    pub glyphs: HashMap<u16, SvgGlyph>
}

/// OT §SVG: a document may be gzip-compressed. Decompress it if it is, otherwise hand the
/// bytes to the SVG parser unchanged.
fn document_data(data: &[u8]) -> Result<Cow<[u8]>, FontError> {
    if data.len() >= 2 && data[0] == 0x1f && data[1] == 0x8b {
        Ok(Cow::Owned(decompress_gzip(data)?))
    } else {
        Ok(Cow::Borrowed(data))
    }
}

/// Skips the gzip header (FEXTRA/FNAME/FCOMMENT/FHCRC included), inflates the deflate
/// body and ignores the CRC32/ISIZE trailer.
fn decompress_gzip(data: &[u8]) -> Result<Vec<u8>, FontError> {
    let flags = match data.get(3) {
        Some(&flags) => flags,
        None => return Err(FontError::Other("SVG: gzip header too short".into())),
    };
    let mut header = 10;
    if flags & 0x04 != 0 { // FEXTRA
        let len = data.get(header .. header + 2)
            .map(|bytes| u16::from_le_bytes([bytes[0], bytes[1]]) as usize)
            .unwrap_or(0);
        header += 2 + len;
    }
    for bit in [0x08u8, 0x10] { // FNAME, FCOMMENT: zero-terminated strings
        if flags & bit != 0 {
            match data.get(header ..).and_then(|rest| rest.iter().position(|&b| b == 0)) {
                Some(end) => header += end + 1,
                None => return Err(FontError::Other("SVG: truncated gzip name".into())),
            }
        }
    }
    if flags & 0x02 != 0 { // FHCRC
        header += 2;
    }
    let body = match data.get(header ..) {
        // The last eight bytes are CRC32 and ISIZE; `inflate_bytes` stops at the end of
        // the deflate stream anyway, so they are simply cut off.
        Some(body) if body.len() > 8 => &body[.. body.len() - 8],
        _ => return Err(FontError::Other("SVG: truncated gzip stream".into())),
    };
    inflate::inflate_bytes(body)
        .map_err(|e| FontError::Other(format!("SVG: gzip: {e}")))
}

pub fn parse_svg(data: &[u8]) -> Result<SvgTable, FontError> {
    let (i, version) = be_u16(data)?;
    let (i, document_list_offset) = be_u32(i)?;
    let (i, _reserved) = be_u32(i)?;

    read_document_list(slice!(data, document_list_offset as usize ..))
}

fn read_document_list(input: &[u8]) -> Result<SvgTable, FontError> {
    let mut glyphs = HashMap::new();

    let (mut data, num_entries) = be_u16(input)?;
    for _ in 0 .. num_entries {
        let (i, start_gid) = be_u16(data)?;
        let (i, end_gid) = be_u16(i)?;
        let (i, data_offset) = be_u32(i)?;
        let (i, data_len) = be_u32(i)?;
        data = i;

        let svg_data = slice!(input, data_offset as usize .. data_offset as usize + data_len as usize);
        let svg_data = document_data(svg_data)?;
        
        // std::fs::write(format!("/tmp/font/{}.svg", start_gid), svg_data);
        let svg = match Svg::from_data(&svg_data) {
            Ok(svg) => Arc::new(svg),
            Err(e) => {
                error!("SVG error: {:?}", e)
            }
        };
        for gid in start_gid ..= end_gid {
            let glyph_id = format!("glyph{}", gid);
            
            match svg.get_item(&glyph_id) {
                Some(item) => {
                    glyphs.insert(gid, SvgGlyph {
                        svg: svg.clone(),
                        item: item.clone()
                    });
                }
                None => {
                    warn!("missing SVG glyph: {}", glyph_id);
                }
            }
        }
    }
    Ok(SvgTable { glyphs })
}
