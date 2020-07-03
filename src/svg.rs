use crate::{R};
use nom::{
    number::complete::{be_u16, be_u32},
};
use svg_dom::{Svg, Item};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone)]
pub struct SvgGlyph {
    pub svg: Arc<Svg>,
    pub item: Arc<Item>,
}

#[derive(Clone)]
pub struct SvgTable {
    pub glyphs: HashMap<u16, SvgGlyph>
}

pub fn parse_svg(data: &[u8]) -> R<SvgTable> {
    let (i, version) = be_u16(data)?;
    let (i, document_list_offset) = be_u32(i)?;
    let (i, _reserved) = be_u32(i)?;

    let (_, svg) = read_document_list(&data[document_list_offset as usize ..])?;
    Ok((i, svg))
}

fn read_document_list(input: &[u8]) -> R<SvgTable> {
    let mut glyphs = HashMap::new();

    let (mut data, num_entries) = be_u16(input)?;
    for _ in 0 .. num_entries {
        let (i, start_gid) = be_u16(data)?;
        let (i, end_gid) = be_u16(i)?;
        let (i, data_offset) = be_u32(i)?;
        let (i, data_len) = be_u32(i)?;
        data = i;

        let svg_data = &input[data_offset as usize .. data_offset as usize + data_len as usize];
        
        // std::fs::write(format!("/tmp/font/{}.svg", start_gid), svg_data);
        let svg = match Svg::from_data(svg_data) {
            Ok(svg) => Arc::new(svg),
            Err(e) => {
                panic!("SVG error: {:?}", e)
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
    Ok((data, SvgTable { glyphs }))
}
