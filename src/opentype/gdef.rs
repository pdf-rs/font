use crate::{GlyphId, R, parsers::*};
use super::parse_class_def;
use nom::{
    number::complete::{be_u8, be_u16, be_u32, be_u24},
    bytes::complete::take,
    sequence::tuple,
};
use std::collections::HashMap;

fn parse_gdef(data: &[u8]) -> R<HashMap<u16, u16>> {
    let (i, major) = be_u16(data)?;
    let (i, minor) = be_u16(i)?;

    assert_eq!(major, 1);
    assert!(matches!(minor, 1 ..= 3));

    let (i, glyph_class_offset) = offset(i)?;
    let (i, attach_offset) = offset(i)?;
    let (i, _lig_caret_offset) = offset(i)?;
    let (i, mark_attach_class_def_offset) = offset(i)?;

    let mut mark_classes = HashMap::new();
    parse_class_def(mark_attach_class_def_offset.of(data), &mut mark_classes)?;

    Ok((i, mark_classes))
}