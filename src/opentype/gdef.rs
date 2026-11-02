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
    require!(minor <= 3);

    let (i, glyph_class_offset) = offset(i)?;
    let (i, _attach_offset) = offset(i)?;
    let (i, _lig_caret_offset) = offset(i)?;
    let (i, mark_attach_class_def_offset) = offset(i)?;

    // Both class definitions are optional; offset 0 means "absent" (OT §GDEF).
    let mut glyph_classes = HashMap::new();
    if let Some(data) = glyph_class_offset.of_opt(data) {
        parse_class_def(data, &mut glyph_classes)?;
    }
    let mut mark_classes = HashMap::new();
    if let Some(data) = mark_attach_class_def_offset.of_opt(data) {
        parse_class_def(data, &mut mark_classes)?;
    }

    Ok(GDef { glyph_classes, mark_classes })
}

#[derive(Clone, Default)]
pub struct GDef {
    /// `glyphClassDef`: 1 base, 2 ligature, 3 mark, 4 component.
    glyph_classes: HashMap<u16, u16>,
    /// `markAttachClassDef`: font-defined mark attachment classes.
    mark_classes: HashMap<u16, u16>
}
impl GDef {
    /// The glyph class from `glyphClassDef` (OT §GDEF, "Glyph Class Definition Table").
    ///
    /// `markAttachClassDef` is *not* used here: its classes are chosen by the font and carry no
    /// Base/Ligature/Mark/Component meaning.
    pub fn mark_class(&self, gid: u16) -> Option<MarkClass> {
        Some(match *self.glyph_classes.get(&gid)? {
            0 => MarkClass::Unassigned,
            1 => MarkClass::Base,
            2 => MarkClass::Ligature,
            3 => MarkClass::Mark,
            4 => MarkClass::Component,
            _ => return None
        })
    }

    /// The font-defined mark attachment class from `markAttachClassDef`, if any.
    pub fn mark_attach_class(&self, gid: u16) -> Option<u16> {
        self.mark_classes.get(&gid).cloned().filter(|&class| class != 0)
    }

    /// Whether `lookup_flag` (OT §"Lookup Flags") tells the lookup to skip `gid`.
    ///
    /// The `IgnoreBaseGlyphs` / `IgnoreLigatures` / `IgnoreMarks` bits skip by glyph class, and a
    /// non-zero `markAttachmentType` (the high byte) skips every mark that is not of exactly that
    /// font-defined `markAttachClassDef` class.
    pub fn ignored(&self, lookup_flag: u16, gid: u16) -> bool {
        let class = match self.mark_class(gid) {
            Some(class) => class,
            None => return false
        };
        match class {
            MarkClass::Base if lookup_flag & 0x0002 != 0 => true,
            MarkClass::Ligature if lookup_flag & 0x0004 != 0 => true,
            MarkClass::Mark if lookup_flag & 0x0008 != 0 => true,
            MarkClass::Mark => {
                let attach_type = lookup_flag >> 8;
                attach_type != 0 && self.mark_attach_class(gid) != Some(attach_type)
            }
            _ => false
        }
    }
}

/// `GDef::ignored` for a font that may have no `GDEF` table at all: without one, every glyph
/// counts as a base glyph, so no lookup flag can skip it.
pub(crate) fn glyph_ignored(gdef: Option<&GDef>, lookup_flag: u16, gid: u16) -> bool {
    gdef.map_or(false, |gdef| gdef.ignored(lookup_flag, gid))
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MarkClass {
    Unassigned,
    Base,
    Ligature,
    Mark,
    Component
}
