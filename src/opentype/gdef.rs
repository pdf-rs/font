use crate::{R, parsers::*, FontError};
use super::{coverage_table, parse_class_def};
use nom::{
    number::complete::{be_u16, be_u32},
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

    // GDEF 1.2 added `markGlyphSetsDef`, the sets a lookup's `UseMarkFilteringSet` bit names.
    // Losing them only costs the marks a lookup meant to keep or drop, never the font.
    let mut mark_sets = Vec::new();
    if minor >= 2 {
        match offset(i).map(|(_, off)| off).and_then(|off| Ok(off.of_opt(data))) {
            Ok(Some(data)) => {
                mark_sets = parse_mark_glyph_sets(data).unwrap_or_else(|err| {
                    warn!("GDEF markGlyphSetsDef: {:?} — ignoring the mark filtering sets", err);
                    Vec::new()
                })
            }
            Ok(None) => (),
            Err(err) => debug!("GDEF markGlyphSetsDef offset: {:?}", err),
        }
    }

    Ok(GDef { glyph_classes, mark_classes, mark_sets })
}

/// `MarkGlyphSetsDef` (OT §GDEF): a list of coverage tables. A lookup whose
/// `UseMarkFilteringSet` bit is set names one of them by index and processes the marks it
/// contains only.
fn parse_mark_glyph_sets(data: &[u8]) -> Result<Vec<Vec<u16>>, FontError> {
    let (i, format) = be_u16(data)?;
    if format != 1 {
        return error!("unsupported mark glyph sets format {}", format);
    }
    let (i, count) = be_u16(i)?;
    let mut sets = Vec::with_capacity(count.min(64) as usize);
    for coverage_off in iterator_n(i, be_u32, count) {
        // One unreadable set costs that set, not the others.
        let set = match data.get(coverage_off as usize ..) {
            Some(coverage) => match coverage_table(coverage) {
                Ok(glyphs) => glyphs.collect(),
                Err(err) => {
                    warn!("mark glyph set: {:?} — treated as empty", err);
                    Vec::new()
                }
            },
            None => {
                warn!("mark glyph set coverage offset {} is past the end — treated as empty", coverage_off);
                Vec::new()
            }
        };
        sets.push(set);
    }
    Ok(sets)
}

#[derive(Clone, Default)]
pub struct GDef {
    /// `glyphClassDef`: 1 base, 2 ligature, 3 mark, 4 component.
    glyph_classes: HashMap<u16, u16>,
    /// `markAttachClassDef`: font-defined mark attachment classes.
    mark_classes: HashMap<u16, u16>,
    /// `markGlyphSetsDef`: one coverage list per index a lookup's `markFilteringSet` names.
    mark_sets: Vec<Vec<u16>>
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

    /// The index of the `markGlyphSetsDef` set that contains `gid`, if it is in one.
    pub fn mark_set(&self, gid: u16) -> Option<u16> {
        self.mark_sets.iter().position(|set| set.contains(&gid)).map(|index| index as u16)
    }

    /// Whether `flags` (OT §"Lookup Flags") tells the lookup to skip `gid`.
    ///
    /// The `IgnoreBaseGlyphs` / `IgnoreLigatures` / `IgnoreMarks` bits skip by glyph class; a
    /// non-zero `markAttachmentType` (the high byte) skips every mark that is not of exactly that
    /// font-defined `markAttachClassDef` class; and `UseMarkFilteringSet` (0x0010) makes a mark
    /// count only when it is in the set the lookup names.
    pub fn ignored(&self, flags: LookupFlags, gid: u16) -> bool {
        let class = match self.mark_class(gid) {
            Some(class) => class,
            None => return false
        };
        match class {
            MarkClass::Base if flags.flag & 0x0002 != 0 => true,
            MarkClass::Ligature if flags.flag & 0x0004 != 0 => true,
            MarkClass::Mark if flags.flag & 0x0008 != 0 => true,
            MarkClass::Mark if flags.flag & 0x0010 != 0 => {
                // OT §"Lookup Flags": with `UseMarkFilteringSet` the lookup processes the marks
                // of its one set and no others. A set the table does not have matches nothing,
                // which is also what a font that forgets the field means to say.
                self.mark_set(gid) != flags.mark_filtering_set
            }
            MarkClass::Mark => {
                let attach_type = flags.flag >> 8;
                attach_type != 0 && self.mark_attach_class(gid) != Some(attach_type)
            }
            _ => false
        }
    }
}

/// A lookup's `lookupFlag` word together with the field that follows it (OT §"Lookup Flags",
/// the `Lookup` table): when `UseMarkFilteringSet` (0x0010) is set, the `Lookup` carries a
/// `markFilteringSet` index after its subtable count, and only the marks of that GDEF set are
/// processed at all.
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub struct LookupFlags {
    pub flag: u16,
    pub mark_filtering_set: Option<u16>
}

/// `GDef::ignored` for a font that may have no `GDEF` table at all: without one, every glyph
/// counts as a base glyph, so no lookup flag can skip it.
pub(crate) fn glyph_ignored(gdef: Option<&GDef>, flags: LookupFlags, gid: u16) -> bool {
    gdef.map_or(false, |gdef| gdef.ignored(flags, gid))
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MarkClass {
    Unassigned,
    Base,
    Ligature,
    Mark,
    Component
}
