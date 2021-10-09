use crate::{R, parsers::*, FontError};
use super::parse_class_def;
use nom::{
    number::complete::{be_u16},
};
use std::collections::HashMap;

pub fn parse_gdef(data: &[u8]) -> Result<GDef, FontError> {
    let (i, major) = be_u16(data)?;
    let (i, minor) = be_u16(i)?;

    require_eq!(major, 1);
    require!(matches!(minor, 1 ..= 3));

    let (i, glyph_class_offset) = offset(i)?;
    let (i, attach_offset) = offset(i)?;
    let (i, _lig_caret_offset) = offset(i)?;
    let (i, mark_attach_class_def_offset) = offset(i)?;

    let mut mark_classes = HashMap::new();
    parse_class_def(mark_attach_class_def_offset.of(data).unwrap(), &mut mark_classes)?;

    Ok(GDef { mark_classes })
}

#[derive(Clone)]
pub struct GDef {
    mark_classes: HashMap<u16, u16>
}
impl GDef {
    pub fn mark_class(&self, gid: u16) -> Option<MarkClass> {
        Some(match *self.mark_classes.get(&gid)? {
            0 => MarkClass::Unassigned,
            1 => MarkClass::Base,
            2 => MarkClass::Ligature,
            3 => MarkClass::Mark,
            4 => MarkClass::Component,
            _ => return None
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MarkClass {
    Unassigned,
    Base,
    Ligature,
    Mark,
    Component
}
