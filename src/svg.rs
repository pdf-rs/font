use crate::{Font, Glyph, Value, Context, State, type1, type2, IResultExt, R, VMetrics, HMetrics, GlyphId};
use nom::{
    number::complete::{be_u8, be_u16, be_i16, be_u24, be_u32, be_i32},
    bytes::complete::{take},
    multi::{count, many0},
    combinator::map,
    sequence::tuple,
    error::{make_error, ErrorKind},
    Err::*,
};
use pathfinder_renderer::scene::Scene;
use usvg::{Node, NodeExt, Tree, Options};
use pathfinder_svg::BuiltSVG;
use std::collections::HashMap;

#[derive(Clone)]
pub struct Svg {
    pub glyphs: HashMap<u16, Scene>
}

pub fn parse_svg(data: &[u8]) -> R<Svg> {
    let (i, version) = be_u16(data)?;
    let (i, document_list_offset) = be_u32(i)?;
    let (i, _reserved) = be_u32(i)?;

    let (_, svg) = read_document_list(&data[document_list_offset as usize ..])?;
    Ok((i, svg))
}

fn read_document_list(input: &[u8]) -> R<Svg> {
    let mut glyphs = HashMap::new();

    let (mut data, num_entries) = be_u16(input)?;
    for _ in 0 .. num_entries {
        let (i, start_gid) = be_u16(data)?;
        let (i, end_gid) = be_u16(i)?;
        let (i, data_offset) = be_u32(i)?;
        let (i, data_len) = be_u32(i)?;
        data = i;

        let svg_data = &input[data_offset as usize .. data_offset as usize + data_len as usize];
        { // DEBUG
            use std::io::Write;
            let stdout = std::io::stdout();
            let mut stdout = stdout.lock();
            stdout.write_all(svg_data).unwrap();
            stdout.write(b"\n").unwrap();
            stdout.flush().unwrap();
        }
        let svg = match Tree::from_data(svg_data, &Options::default()) {
            Ok(tree) => tree,
            Err(e) => {
                panic!("SVG error: {:?}", e)
            }
        };
        for gid in start_gid ..= end_gid {
            let glyph_id = format!("glyph{}", gid);
            let node = svg.node_by_id(&glyph_id).unwrap();
            let scene = BuiltSVG::from_tree(&node.tree()).scene;
            glyphs.insert(gid, scene);
        }
    }
    Ok((data, Svg { glyphs }))
}
