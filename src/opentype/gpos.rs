use std::collections::HashMap;
use nom::{
    bytes::complete::{take},
    number::complete::{be_i16, be_u16, be_u32},
    sequence::{tuple},
};
use crate::{R, GlyphId, GlyphPosition, FontError};
use crate::parsers::{*};
use crate::opentype::{Maxp, Tag, parse_extension, parse_lookup_list, coverage_table, parse_class_def};
use crate::opentype::gdef::{GDef, glyph_ignored};
use crate::opentype::gsub::{
    Context, Feature, Script, MAX_NESTING, enabled_lookups, parse_chain_context_subst,
    parse_context_subst, parse_feature_list, parse_script_list, visible_after, visible_before,
};
use itertools::{Itertools};


/// OT §GPOS: the positioning table. It keeps its lookups in the file's order, because that is
/// the order they are applied in, plus the flattened maps the accessors and `Font::kerning` read.
#[derive(Default, Clone)]
pub struct GPos {
    /// The advance adjustments of pair positioning, keyed by glyph ids — what `Font::kerning`
    /// uses.
    pub kern: KernTable,
    scripts: Vec<Script>,
    features: Vec<Feature>,
    lookups: Vec<Vec<PosSubtable>>,
    /// Per lookup: its `lookupFlag` (OT §"Lookup Flags"), which says which glyphs it skips.
    flags: Vec<u16>,
}

/// One subtable of a `GPOS` lookup.
///
/// Lookup lines 1-6 all fix one or two anchors or values; the last two forms (7, 8) are the
/// contextual ones and only say *which* lookups to apply inside a matched run — the
/// `Context` type is shared with `GSUB`, which has the same rule tables.
#[derive(Debug, Clone)]
enum PosSubtable {
    /// OT §GPOS "Single Adjustment" (1): the value record of one glyph.
    Single(HashMap<u16, ValueRecord>),
    /// OT §GPOS "Pair Adjustment" (2): both value records of a glyph or class pair.
    Pair(PairSets),
    /// OT §GPOS "Cursive Attachment" (3): entry and exit anchor of one glyph.
    Cursive(HashMap<u16, ((i16, i16), (i16, i16))>),
    /// OT §GPOS "Mark-to-Base Attachment" (4): (base, mark) -> offset.
    MarkToBase(HashMap<(u16, u16), (i16, i16)>),
    /// OT §GPOS "Mark-to-Ligature Attachment" (5): (ligature, mark) -> offset, from the first
    /// component of the ligature.
    MarkToLigature(HashMap<(u16, u16), (i16, i16)>),
    /// OT §GPOS "Mark-to-Mark Attachment" (6): (base mark, mark) -> offset.
    MarkToMark(HashMap<(u16, u16), (i16, i16)>),
    /// OT §GPOS "Context Positioning" (7) and "Chaining Context Positioning" (8).
    Context(Context),
}

/// The two halves of pair positioning: explicit glyph pairs (format 1) and the class matrix
/// (format 2). The class definitions themselves live in the `KernTable`.
#[derive(Debug, Clone, Default)]
struct PairSets {
    pairs: HashMap<(u16, u16), (ValueRecord, ValueRecord)>,
    class_pairs: HashMap<(u16, u16), (ValueRecord, ValueRecord)>,
}

impl GPos {
    pub fn from_kern(kern: KernTable) -> GPos {
        GPos {
            kern,
            ..GPos::default()
        }
    }
    /// Every subtable of every lookup, in lookup order.
    fn subtables(&self) -> impl Iterator<Item=&PosSubtable> {
        self.lookups.iter().flatten()
    }
    pub fn get_mark_to_base(&self, base: GlyphId, mark: GlyphId) -> Option<(i16, i16)> {
        let key = (glyph_key(base)?, glyph_key(mark)?);
        self.subtables().find_map(|subtable| match subtable {
            PosSubtable::MarkToBase(marks) => marks.get(&key).cloned(),
            _ => None,
        })
    }
    /// OT §GPOS "Single Adjustment": placement and advance adjustment of one glyph.
    pub fn get_single(&self, glyph: GlyphId) -> Option<ValueRecord> {
        let key = glyph_key(glyph)?;
        self.subtables().find_map(|subtable| match subtable {
            PosSubtable::Single(values) => values.get(&key).cloned(),
            _ => None,
        })
    }
    /// OT §GPOS "Cursive Attachment": `(entry, exit)` anchors of one glyph.
    pub fn get_cursive(&self, glyph: GlyphId) -> Option<((i16, i16), (i16, i16))> {
        let key = glyph_key(glyph)?;
        self.subtables().find_map(|subtable| match subtable {
            PosSubtable::Cursive(anchors) => anchors.get(&key).cloned(),
            _ => None,
        })
    }
    /// OT §GPOS "Mark-to-Ligature Attachment": where the mark is placed relative to the first
    /// component of the ligature.
    pub fn get_mark_to_ligature(&self, ligature: GlyphId, mark: GlyphId) -> Option<(i16, i16)> {
        let key = (glyph_key(ligature)?, glyph_key(mark)?);
        self.subtables().find_map(|subtable| match subtable {
            PosSubtable::MarkToLigature(marks) => marks.get(&key).cloned(),
            _ => None,
        })
    }
    /// OT §GPOS "Mark-to-Mark Attachment": where a mark is placed relative to another mark.
    pub fn get_mark_to_mark(&self, base: GlyphId, mark: GlyphId) -> Option<(i16, i16)> {
        let key = (glyph_key(base)?, glyph_key(mark)?);
        self.subtables().find_map(|subtable| match subtable {
            PosSubtable::MarkToMark(marks) => marks.get(&key).cloned(),
            _ => None,
        })
    }
    /// OT §GPOS "Pair Adjustment": both value records of `left` followed by `right` — the
    /// glyph pairs first, then the class pair the two glyphs fall into.
    pub fn get_pair(&self, left: GlyphId, right: GlyphId) -> Option<(ValueRecord, ValueRecord)> {
        let left_key = glyph_key(left)?;
        let right_key = glyph_key(right)?;
        let classes = self.kern.left_classes.get(&left_key)
            .and_then(|left_class| self.kern.right_classes.get(&right_key).map(|right_class| (*left_class, *right_class)));
        let mut class_pair = None;
        for subtable in self.subtables() {
            if let PosSubtable::Pair(sets) = subtable {
                if let Some(pair) = sets.pairs.get(&(left_key, right_key)) {
                    return Some(pair.clone());
                }
                if class_pair.is_none() {
                    class_pair = classes.and_then(|classes| sets.class_pairs.get(&classes).cloned());
                }
            }
        }
        class_pair
    }

    /// Applies the positioning of the default language system whose feature tag `enabled`
    /// accepts to a run of glyphs, in the font's default language system: one `GlyphPosition`
    /// per glyph, all-default for a feature that touches nothing.
    pub fn position(&self, glyphs: &[GlyphId], enabled: &dyn Fn(Tag) -> bool) -> Vec<GlyphPosition> {
        self.position_with(glyphs, enabled, None)
    }

    /// `position`, with the `GDEF` table the font carries: it tells a lookup which glyphs to
    /// skip (OT §"Lookup Flags"), which needs the glyph classes only `GDEF` has.
    pub fn position_with(&self, glyphs: &[GlyphId], enabled: &dyn Fn(Tag) -> bool, gdef: Option<&GDef>) -> Vec<GlyphPosition> {
        let mut out = vec![GlyphPosition::default(); glyphs.len()];
        let lookup_indices = enabled_lookups(&self.scripts, &self.features, enabled);
        if lookup_indices.is_empty() {
            return out;
        }
        // OT §"Lookups": every lookup runs, in lookup-list order; unlike a substitution a
        // positioning lookup never changes the length of the run, so nothing moves under us.
        for position in 0 .. glyphs.len() {
            for &lookup_idx in &lookup_indices {
                self.apply_lookup_at(lookup_idx, glyphs, &mut out, position, 0, gdef);
            }
        }
        out
    }

    /// Applies one lookup at `position`; `true` if one of its subtables applied there.
    fn apply_lookup_at(&self, lookup_idx: u16, glyphs: &[GlyphId], positions: &mut [GlyphPosition], position: usize, depth: u8, gdef: Option<&GDef>) -> bool {
        let subtables = match self.lookups.get(lookup_idx as usize) {
            Some(subtables) => subtables,
            None => return false,
        };
        let flag = self.flags.get(lookup_idx as usize).copied().unwrap_or(0);
        let glyph = match glyphs.get(position).and_then(|&glyph| glyph_key(glyph)) {
            Some(glyph) => glyph,
            None => return false,
        };
        // OT §"Lookup Flags": a lookup skips the glyph classes its flag names.
        if glyph_ignored(gdef, flag, glyph) {
            return false;
        }
        let skip = |gid: u16| glyph_ignored(gdef, flag, gid);
        // OT §"Lookups": only the first subtable of a lookup that matches is used.
        for subtable in subtables {
            let applied = match subtable {
                PosSubtable::Single(values) => match values.get(&glyph) {
                    Some(value) => {
                        add(positions, position, value);
                        true
                    }
                    None => false,
                },
                PosSubtable::Pair(sets) => self.apply_pair(sets, glyphs, positions, position, glyph, &skip),
                PosSubtable::Cursive(anchors) => self.apply_cursive(anchors, glyphs, positions, position, glyph, &skip),
                PosSubtable::MarkToBase(marks) | PosSubtable::MarkToLigature(marks) | PosSubtable::MarkToMark(marks) =>
                    self.apply_mark_attachment(marks, glyphs, positions, position, glyph, &skip),
                PosSubtable::Context(context) => self.apply_context(context, glyphs, positions, position, depth, flag, gdef),
            };
            if applied {
                return true;
            }
        }
        false
    }

    /// OT §GPOS "Pair Adjustment": the value records of the pair starting at `position` go on
    /// the first glyph and on the second one.
    fn apply_pair(&self, sets: &PairSets, glyphs: &[GlyphId], positions: &mut [GlyphPosition], position: usize, glyph: u16, skip: &dyn Fn(u16) -> bool) -> bool {
        let second_index = match visible_after(glyphs, position, 1, skip) {
            Some(index) => index,
            None => return false,
        };
        let second = match glyphs.get(second_index).and_then(|&glyph| glyph_key(glyph)) {
            Some(glyph) => glyph,
            None => return false,
        };
        let (first_value, second_value) = match sets.pairs.get(&(glyph, second)) {
            Some(pair) => pair.clone(),
            None => {
                let classes = self.kern.left_classes.get(&glyph)
                    .and_then(|left_class| self.kern.right_classes.get(&second).map(|right_class| (*left_class, *right_class)));
                match classes.and_then(|classes| sets.class_pairs.get(&classes)) {
                    Some(pair) => pair.clone(),
                    None => return false,
                }
            }
        };
        add(positions, position, &first_value);
        add(positions, second_index, &second_value);
        true
    }

    /// OT §GPOS "Cursive Attachment": the entry anchor of a glyph is laid onto the exit anchor
    /// of the glyph before it, which moves the glyph by the difference.
    fn apply_cursive(&self, anchors: &HashMap<u16, ((i16, i16), (i16, i16))>, glyphs: &[GlyphId], positions: &mut [GlyphPosition], position: usize, glyph: u16, skip: &dyn Fn(u16) -> bool) -> bool {
        let (entry, _exit) = match anchors.get(&glyph) {
            Some(anchors) => *anchors,
            None => return false,
        };
        let previous = match visible_before(glyphs, position, 0, skip).and_then(|index| glyphs.get(index)).and_then(|&glyph| glyph_key(glyph)) {
            Some(previous) => previous,
            None => return false,
        };
        let (_entry, exit) = match anchors.get(&previous) {
            Some(anchors) => *anchors,
            None => return false,
        };
        if let Some(slot) = positions.get_mut(position) {
            slot.x_offset = slot.x_offset.saturating_add(exit.0.saturating_sub(entry.0));
            slot.y_offset = slot.y_offset.saturating_add(exit.1.saturating_sub(entry.1));
        }
        true
    }

    /// OT §GPOS "Mark Attachment" (4, 5, 6): a mark moves to the anchor its base — base glyph,
    /// ligature or another mark — carries for its class. The maps hold that difference already.
    fn apply_mark_attachment(&self, marks: &HashMap<(u16, u16), (i16, i16)>, glyphs: &[GlyphId], positions: &mut [GlyphPosition], position: usize, glyph: u16, skip: &dyn Fn(u16) -> bool) -> bool {
        let mut n = 0;
        while let Some(index) = visible_before(glyphs, position, n, skip) {
            let base = match glyphs.get(index).and_then(|&glyph| glyph_key(glyph)) {
                Some(base) => base,
                None => return false,
            };
            if let Some(&(dx, dy)) = marks.get(&(base, glyph)) {
                if let Some(slot) = positions.get_mut(position) {
                    slot.x_offset = slot.x_offset.saturating_add(dx);
                    slot.y_offset = slot.y_offset.saturating_add(dy);
                }
                return true;
            }
            n += 1;
        }
        false
    }

    /// OT §GPOS "Context Positioning" (7, 8): run the lookups the first matching rule names at
    /// the glyphs of the run. Positioning never changes the length of the run.
    fn apply_context(&self, context: &Context, glyphs: &[GlyphId], positions: &mut [GlyphPosition], position: usize, depth: u8, flag: u16, gdef: Option<&GDef>) -> bool {
        if depth >= MAX_NESTING {
            debug!("GPOS: not following context lookups nested deeper than {}", MAX_NESTING);
            return false;
        }
        let skip = |gid: u16| glyph_ignored(gdef, flag, gid);
        let rule = match context.matched_rule(glyphs, position, &skip) {
            Some(rule) => rule,
            None => return false,
        };
        let targets: Vec<usize> = (0 ..= rule.input_len())
            .filter_map(|n| visible_after(glyphs, position, n, &skip))
            .collect();
        let mut applied = false;
        for &(sequence_index, lookup_index) in rule.records() {
            // OT §"Sequence Lookup Records": an index outside the run has nothing to apply to.
            let target = match targets.get(sequence_index as usize) {
                Some(&target) => target,
                None => continue,
            };
            applied |= self.apply_lookup_at(lookup_index, glyphs, positions, target, depth + 1, gdef);
        }
        applied
    }
}

/// Adds a value record to the position of one glyph, saturating: a font may pile adjustments
/// up, and a sum that does not fit an `i16` should not take the process down.
fn add(positions: &mut [GlyphPosition], position: usize, value: &ValueRecord) {
    if let Some(slot) = positions.get_mut(position) {
        slot.x_advance = slot.x_advance.saturating_add(value.x_advance);
        slot.y_advance = slot.y_advance.saturating_add(value.y_advance);
        slot.x_offset = slot.x_offset.saturating_add(value.x_placement);
        slot.y_offset = slot.y_offset.saturating_add(value.y_placement);
    }
}

/// The tables are keyed by u16 glyph ids; a larger id cannot be in one.
fn glyph_key(glyph: GlyphId) -> Option<u16> {
    if glyph.0 > u16::MAX as u32 { None } else { Some(glyph.0 as u16) }
}


#[derive(Default, Clone)]
pub struct KernTable {
    pub left_classes: HashMap<u16, u16>, // gid -> class id,
    pub right_classes: HashMap<u16, u16>, // gid -> class id,
    pub class_pairs: HashMap<(u16, u16), i16>,
    pub glyph_pairs: HashMap<(u16, u16), i16>
}
impl KernTable {
    pub fn get(&self, left: u16, right: u16) -> Option<i16> {
        if let Some(&kern) = self.glyph_pairs.get(&(left, right)) {
            return Some(kern);
        }
        let &left_class = self.left_classes.get(&left)?;
        let &right_class = self.right_classes.get(&right)?;
        let &kern = self.class_pairs.get(&(left_class, right_class))?;
        Some(kern)
    }
}

// figure out how to replace with a dedicated sparse 2d map
pub fn parse_gpos<'a>(data: &'a [u8], maxp: &Maxp) -> Result<GPos, FontError> {
    debug!("parse GPOS");
    let (i, major_version) = be_u16(data)?;
    require_eq!(major_version, 1);
    let (i, minor_version) = be_u16(i)?;
    
    let (i, script_list_off) = be_u16(i)?;
    let (i, feature_list_off) = be_u16(i)?;
    let (i, lookup_list_off) = be_u16(i)?;
    
    let (i, _feature_variations_offset) = match minor_version {
        0 => (i, 0),
        1 => be_u32(i)?,
        // OT §GPOS: a minor version we do not know may carry fields we cannot skip, but the
        // tables we do read are laid out the same way, so ignore the rest instead of failing.
        v => {
            warn!("unsupported GPOS version 1.{}, ignoring the rest of the header", v);
            (i, 0)
        }
    };
    
    let mut gpos = GPos::default();
    
    // OT §GPOS: script and feature lists are optional (offset 0 means "absent"); without them
    // no feature is enabled, so `position` leaves the run alone.
    if script_list_off != 0 {
        gpos.scripts = parse_script_list(slice!(data, script_list_off as usize ..))?;
    }
    if feature_list_off != 0 {
        gpos.features = parse_feature_list(slice!(data, feature_list_off as usize ..))?;
    }
    
    parse_lookup_list(slice!(data, lookup_list_off as usize ..), |lookup_idx, data, lookup_type, lookup_flag| {
        debug!("lookup type {}", lookup_type);
        while lookup_idx >= gpos.lookups.len() {
            gpos.lookups.push(Vec::new());
            gpos.flags.push(0);
        }
        gpos.flags[lookup_idx] = lookup_flag;
        parse_positioning(data, lookup_type, &mut gpos, maxp.num_glyphs)
    })?;
    
    Ok(gpos)
}

/// One subtable of a `GPOS` lookup. Single adjustment (1), pair adjustment (2), cursive (3),
/// mark-to-base (4), mark-to-ligature (5), mark-to-mark (6) and the contextual lookups (7, 8)
/// are read; `Extension` (9) wraps the ones that are, which is what large fonts use.
fn parse_positioning(data: &[u8], lookup_type: u16, gpos: &mut GPos, num_glyphs: u16) -> Result<(), FontError> {
    // A subtable belongs to the lookup `parse_gpos` has just opened a slot for.
    if gpos.lookups.is_empty() {
        gpos.lookups.push(Vec::new());
        gpos.flags.push(0);
    }
    let subtable = match lookup_type {
        2 => PosSubtable::Pair(parse_pair_adjustment(data, gpos)?),
        3 => PosSubtable::Cursive(parse_cursive(data)?),
        4 => PosSubtable::MarkToBase(parse_mark_attachment(data)?),
        5 => PosSubtable::MarkToLigature(parse_mark_to_ligature_attachment(data)?),
        6 => PosSubtable::MarkToMark(parse_mark_attachment(data)?),
        // Context · Apply a lookup inside a matched glyph run
        7 => PosSubtable::Context(parse_context_subst(data)?),
        // Chaining context · the same, with backtrack and lookahead
        8 => PosSubtable::Context(parse_chain_context_subst(data)?),
        // OT §GPOS "Extension Positioning": a wrapper around another lookup type.
        9 => {
            let (inner_type, inner) = parse_extension(data)?;
            if inner_type == 9 {
                // OT §GPOS "Extension Positioning": extensions may not wrap extensions.
                warn!("GPOS: an Extension lookup may not point at another Extension lookup");
                return Ok(());
            }
            return parse_positioning(inner, inner_type, gpos, num_glyphs);
        }
        1 => PosSubtable::Single(parse_single_adjustment(data)?),
        _ => return Ok(()),
    };
    if let Some(lookup) = gpos.lookups.last_mut() {
        lookup.push(subtable);
    }
    Ok(())
}


/// OT §GPOS "Value Records": a placement and an advance adjustment per axis; the value format
/// says which of the four are present. `Default` is "no adjustment", which is also what a
/// missing field means.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ValueRecord {
    pub x_placement: i16,
    pub y_placement: i16,
    pub x_advance: i16,
    pub y_advance: i16,
}

fn value_record(flags: u16) -> impl Fn(&[u8]) -> R<ValueRecord> {
    move |mut i: &[u8]| {
        let mut record = ValueRecord::default();
        if flags & 0x0001 != 0 {
            record.x_placement = parse(&mut i, be_i16)?;
        }
        if flags & 0x0002 != 0 {
            record.y_placement = parse(&mut i, be_i16)?;
        }
        if flags & 0x0004 != 0 {
            record.x_advance = parse(&mut i, be_i16)?;
        }
        if flags & 0x0008 != 0 {
            record.y_advance = parse(&mut i, be_i16)?;
        }
        let (i, _) = take((flags & 0x00F0).count_ones() * 2)(i)?;
        Ok((i, record))
    }
}

/// OT §GPOS "Pair Adjustment" (lookup type 2), formats 1 and 2. Fills the flat `i16` maps that
/// `Font::kerning` uses side by side with the value records, which keep the placements the
/// advance-only maps cannot hold.
fn parse_pair_adjustment<'a>(data: &'a [u8], gpos: &mut GPos) -> Result<PairSets, FontError> {
    let mut pairs = PairSets::default();
    let (i, format) = be_u16(data)?;
    match format {
        1 => {
            let (i, coverage_off) = offset(i)?;
            let (i, value_format_1) = be_u16(i)?;
            let (i, value_format_2) = be_u16(i)?;
            let (i, pair_set_count) = be_u16(i)?;

            let mut coverage = coverage_table(coverage_off.of(data)?)?;
            for offset in iterator_n(i, be_u16, pair_set_count) {
                let first_glyph = match coverage.next() {
                    Some(glyph) => glyph,
                    // fewer coverage entries than pair sets: ignore the rest
                    None => break
                };
                let i = slice!(data, offset as usize ..);
                let (i, pair_value_count) = be_u16(i)?;
                
                let iter = iterator_n(i, tuple((be_u16, value_record(value_format_1), value_record(value_format_2))), pair_value_count);
                for (second_glyph, value_record_1, value_record_2) in iter {
                    gpos.kern.glyph_pairs.insert((first_glyph, second_glyph), value_record_1.x_advance);
                    pairs.pairs.insert((first_glyph, second_glyph), (value_record_1, value_record_2));
                }
            }
        },
        2 => {
            let (i, _coverage_off) = be_u16(i)?;
            let (i, value_format_1) = be_u16(i)?;
            let (i, value_format_2) = be_u16(i)?;
            let (i, class_def_1_offset) = be_u16(i)?;
            let (i, class_def_2_offset) = be_u16(i)?;
            let (i, class_1_count) = be_u16(i)?;
            let (i, class_2_count) = be_u16(i)?;

            // The class matrix is indexed by the two ClassDefs; without them no glyph ever gets a
            // class and `KernTable::get` can never reach `class_pairs`.
            parse_class_def(Offset(class_def_1_offset).of(data)?, &mut gpos.kern.left_classes)?;
            parse_class_def(Offset(class_def_2_offset).of(data)?, &mut gpos.kern.right_classes)?;


            // `class_1_count * class_2_count` overflows u16 (300 x 300 classes is a panic in
            // debug builds), and a bogus count would let `iterator_n` run for billions of
            // iterations and `reserve` allocate gigabytes. A matrix cannot hold more values
            // than the subtable has bytes.
            let total = usize::from(class_1_count) * usize::from(class_2_count);
            let total = total.min(data.len());
            gpos.kern.class_pairs.reserve(total.min(1 << 16));

            let iter = (0 .. class_1_count).cartesian_product(0 .. class_2_count)
                .zip(iterator_n(i, tuple((value_record(value_format_1), value_record(value_format_2))), total));

            for ((class_1, class_2), (value_record_1, value_record_2)) in iter {
                if value_record_1 != ValueRecord::default() || value_record_2 != ValueRecord::default() {
                    pairs.class_pairs.insert((class_1, class_2), (value_record_1.clone(), value_record_2));
                }
                if value_record_1.x_advance != 0 {
                    gpos.kern.class_pairs.insert((class_1, class_2), value_record_1.x_advance);
                }
            }
        }
        n => return error!("unsupported pair adjustment format {}", n)
    }
    Ok(pairs)
}

parser!(int16 : be_i16 -> i16);
parser!(uint16 : be_u16 -> u16);

table!(MarkAnchorOff {
    uint16 mark_class,
    uint16 mark_anchor_off,
});

table!(AnchorTable {
    uint16 format,
    int16 x,
    int16 y,
});

// (mark class, (x, y))
fn parse_mark_array_table(data: &[u8]) -> Result<impl Array<Item=(u16, AnchorTable)> + '_, FontError> {
    let (i, mark_count) = be_u16(data)?;
    let (_, array) = array::<MarkAnchorOff, _>(mark_count)(i)?;
    let array = array.map(move |off: MarkAnchorOff| {
        let anchor = AnchorTable::parse(offset!(data, off.mark_anchor_off))?;
        Ok((off.mark_class, anchor))
    });
    Ok(array)
}

/// OT §GPOS "Mark-to-Base Attachment" (4) and "Mark-to-Mark Attachment" (6), format 1: the two
/// subtables have the same layout, so this fills either map with `base`/`mark` anchor deltas.
fn parse_mark_attachment(data: &[u8]) -> Result<HashMap<(u16, u16), (i16, i16)>, FontError> {
    let mut marks = HashMap::new();
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);
    let (i, mark_coverage_offset) = be_u16(i)?;
    let (i, base_coverage_offset) = be_u16(i)?;
    let (i, mark_class_count) = be_u16(i)?;
    let (i, mark_array_offset) = be_u16(i)?;
    let (i, base_array_offset) = be_u16(i)?;

    let mark_coverage = coverage_table(slice!(data, mark_coverage_offset as usize..))?;
    let base_coverage: Vec<_> = coverage_table(slice!(data, base_coverage_offset as usize..))?.collect();

    let base_array = parse_base_array(slice!(data, base_array_offset as usize..), mark_class_count)?;

    let mark_array = parse_mark_array_table(slice!(data, mark_array_offset as usize ..))?;
    for (mark_gid, m) in mark_coverage.zip(mark_array.iter()) {
        let (mark_class, mark_anchor) = m?;
        if mark_class >= mark_class_count {
            // A mark whose class the base array does not have: the font is malformed, but the
            // other marks are still usable, so skip this one instead of failing the whole table
            // (which used to drop kerning and mark attachment for the entire font).
            warn!("GPOS: mark glyph {} has class {} but markClassCount is {}",
                  mark_gid, mark_class, mark_class_count);
            continue;
        }
        for (base_nr, &base_gid) in base_coverage.iter().enumerate() {
            let index = base_nr * usize::from(mark_class_count) + usize::from(mark_class);
            let base_anchor = match base_array.get(index) {
                Ok(anchor) => anchor,
                // Fewer anchors than baseCount x markClassCount: skip what is missing.
                Err(_) => continue,
            };
            marks.insert((base_gid, mark_gid), (base_anchor.x - mark_anchor.x, base_anchor.y - mark_anchor.y));
        }
    }

    Ok(marks)
}

fn parse_base_array(data: &[u8], mark_class_count: u16) -> Result<impl Array<Item=AnchorTable> + '_, FontError> {
    let (i, base_count) = be_u16(data)?;
    let (_, arr) = array::<uint16, _>(usize::from(mark_class_count) * usize::from(base_count))(i)?;

    Ok(arr.map(move |off: u16| AnchorTable::parse(slice!(data, off as usize..))))
}

/// OT §GPOS "Single Adjustment" (lookup type 1), formats 1 and 2: one value record per glyph,
/// either the same for every covered glyph or one each.
fn parse_single_adjustment(data: &[u8]) -> Result<HashMap<u16, ValueRecord>, FontError> {
    let mut values = HashMap::new();
    let (i, format) = be_u16(data)?;
    let (i, coverage_offset) = be_u16(i)?;
    let (i, value_format) = be_u16(i)?;
    let coverage = coverage_table(slice!(data, coverage_offset as usize ..))?;
    match format {
        1 => {
            let (_, value) = value_record(value_format)(i)?;
            for glyph in coverage {
                values.insert(glyph, value.clone());
            }
        }
        2 => {
            let (i, value_count) = be_u16(i)?;
            for (glyph, value) in coverage.zip(iterator_n(i, value_record(value_format), value_count)) {
                values.insert(glyph, value);
            }
        }
        n => warn!("GPOS: unsupported single adjustment format {}, ignoring it", n),
    }
    Ok(values)
}

/// OT §GPOS "Cursive Attachment" (lookup type 3), format 1: an entry and an exit anchor per
/// glyph. A NULL offset means the glyph has no such anchor, which is stored as `(0, 0)`.
fn parse_cursive(data: &[u8]) -> Result<HashMap<u16, ((i16, i16), (i16, i16))>, FontError> {
    let mut anchors = HashMap::new();
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);
    let (i, coverage_offset) = be_u16(i)?;
    let (i, entry_exit_count) = be_u16(i)?;
    let coverage = coverage_table(slice!(data, coverage_offset as usize ..))?;
    for (glyph, (entry_offset, exit_offset)) in coverage.zip(iterator_n(i, tuple((be_u16, be_u16)), entry_exit_count)) {
        let entry = match Offset(entry_offset).of_opt(data) {
            Some(anchor) => anchor_coordinates(anchor)?,
            None => (0, 0),
        };
        let exit = match Offset(exit_offset).of_opt(data) {
            Some(anchor) => anchor_coordinates(anchor)?,
            None => (0, 0),
        };
        anchors.insert(glyph, (entry, exit));
    }
    Ok(anchors)
}

/// OT §GPOS "Anchor Tables": only the `(x, y)` pair is used. Formats 2 and 3 carry a contour
/// point as well, but the coordinates are the same leading fields.
fn anchor_coordinates(data: &[u8]) -> Result<(i16, i16), FontError> {
    let anchor = AnchorTable::parse(data)?;
    Ok((anchor.x, anchor.y))
}

/// OT §GPOS "Mark-to-Ligature Attachment" (lookup type 5), format 1. A ligature can carry an
/// anchor per component; this crate keeps the first component's, which is where a mark after
/// the whole ligature goes.
fn parse_mark_to_ligature_attachment(data: &[u8]) -> Result<HashMap<(u16, u16), (i16, i16)>, FontError> {
    let mut marks_to_ligatures = HashMap::new();
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);
    let (i, mark_coverage_offset) = be_u16(i)?;
    let (i, ligature_coverage_offset) = be_u16(i)?;
    let (i, mark_class_count) = be_u16(i)?;
    let (i, mark_array_offset) = be_u16(i)?;
    let (i, ligature_array_offset) = be_u16(i)?;

    let mark_coverage = coverage_table(slice!(data, mark_coverage_offset as usize ..))?;
    let ligature_coverage: Vec<u16> = coverage_table(slice!(data, ligature_coverage_offset as usize ..))?.collect();
    let mark_array = parse_mark_array_table(slice!(data, mark_array_offset as usize ..))?;

    // The marks with their class and anchor, once, instead of once per ligature.
    let mut marks = Vec::new();
    for (mark_gid, entry) in mark_coverage.zip(mark_array.iter()) {
        let (mark_class, mark_anchor) = entry?;
        if mark_class >= mark_class_count {
            warn!("GPOS: mark glyph {} has class {} but markClassCount is {}",
                  mark_gid, mark_class, mark_class_count);
            continue;
        }
        marks.push((mark_gid, mark_class, (mark_anchor.x, mark_anchor.y)));
    }

    let ligature_array = slice!(data, ligature_array_offset as usize ..);
    let (i, ligature_count) = be_u16(ligature_array)?;
    for (&ligature_gid, attach_offset) in ligature_coverage.iter().zip(iterator_n(i, be_u16, ligature_count)) {
        let attach = slice!(ligature_array, attach_offset as usize ..);
        if attach.len() < 2 {
            continue;
        }
        let (i, component_count) = be_u16(attach)?;
        if component_count == 0 {
            warn!("GPOS: ligature {} has componentCount 0, skipping it", ligature_gid);
            continue;
        }
        // The first ComponentRecord: one anchor offset per mark class.
        let anchors = match array::<uint16, _>(usize::from(mark_class_count))(i) {
            Ok((_, anchors)) => anchors,
            Err(_) => continue,
        };
        for &(mark_gid, mark_class, (mark_x, mark_y)) in &marks {
            let anchor = match anchors.get(usize::from(mark_class)) {
                // A NULL offset: no anchor for this class.
                Ok(0) => continue,
                Ok(offset) => match AnchorTable::parse(slice!(attach, offset as usize ..)) {
                    Ok(anchor) => anchor,
                    Err(_) => continue,
                },
                Err(_) => continue,
            };
            marks_to_ligatures.insert(
                (ligature_gid, mark_gid),
                (anchor.x - mark_x, anchor.y - mark_y),
            );
        }
    }
    Ok(marks_to_ligatures)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extension_lookup_reaches_pair_adjustment() {
        // OT §GPOS "Extension Positioning" (lookup type 9) around a PairPos format 1:
        // glyph 20 followed by 21 gets xAdvance -40.
        let mut data = Vec::new();
        data.extend_from_slice(&[0, 1]);       // extension format 1
        data.extend_from_slice(&[0, 2]);       // extensionLookupType = Pair Adjustment
        data.extend_from_slice(&[0, 0, 0, 8]); // extensionOffset
        data.extend_from_slice(&[0, 1]);       // PairPos format 1
        data.extend_from_slice(&[0, 18]);      // coverageOffset (from the subtable start)
        data.extend_from_slice(&[0, 4]);       // valueFormat1 = X_ADVANCE
        data.extend_from_slice(&[0, 0]);       // valueFormat2
        data.extend_from_slice(&[0, 1]);       // pairSetCount
        data.extend_from_slice(&[0, 12]);      // pairSetOffset[0]
        data.extend_from_slice(&[0, 1]);       // pairValueCount
        data.extend_from_slice(&[0, 21]);      // secondGlyph
        data.extend_from_slice(&(-40i16).to_be_bytes()); // xAdvance
        data.extend_from_slice(&[0, 1]);       // Coverage format 1
        data.extend_from_slice(&[0, 1]);       // glyphCount
        data.extend_from_slice(&[0, 20]);      // glyph

        let mut gpos = GPos::default();
        parse_positioning(&data, 9, &mut gpos, 100).expect("extension positioning parses");
        assert_eq!(gpos.kern.get(20, 21), Some(-40));
    }

    fn words_to_bytes(words: &[u16]) -> Vec<u8> {
        words.iter().flat_map(|w| w.to_be_bytes().to_vec()).collect()
    }

    #[test]
    fn pair_adjustment_with_huge_class_matrix_does_not_overflow() {
        // `class1Count * class2Count` in u16 overflowed for 300 x 300 classes. The value
        // formats are 0, so the matrix carries no data at all.
        let words: [u16; 14] = [
            2, 0,                 // PairPos format 2, coverage offset (unused)
            0, 0,                 // valueFormat1, valueFormat2
            16, 22,               // classDef1Offset, classDef2Offset
            300, 300,             // class1Count, class2Count
            1, 0, 0,              // classDef1: format 1, startGlyph 0, glyphCount 0
            1, 0, 0,              // classDef2
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        let pairs = parse_pair_adjustment(&data, &mut gpos).expect("pair adjustment parses");
        assert!(gpos.kern.class_pairs.is_empty());
        assert!(pairs.class_pairs.is_empty());
    }

    #[test]
    fn pair_adjustment_keeps_both_value_records() {
        // OT §GPOS "Pair Adjustment" (2), format 1: 20 followed by 21 gets xPlacement 3 and
        // xAdvance -40 on the first glyph and xAdvance 5 on the second.
        let words: [u16; 14] = [
            1, 22,                // format, coverageOffset
            5, 4,                 // valueFormat1 = X_PLACEMENT|X_ADVANCE, valueFormat2 = X_ADVANCE
            1, 12,                // pairSetCount, pairSetOffset[0]
            1, 21,                // pairValueCount, secondGlyph
            3, (-40i16) as u16,   // value record 1: xPlacement, xAdvance
            5,                    // value record 2: xAdvance
            1, 1, 20,             // coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 2, &mut gpos, 100).expect("pair adjustment parses");

        let (first, second) = gpos.get_pair(GlyphId(20), GlyphId(21)).expect("a pair");
        assert_eq!(first.x_placement, 3);
        assert_eq!(first.x_advance, -40);
        assert_eq!(second.x_advance, 5);
        // The advance-only map `Font::kerning` uses is still filled.
        assert_eq!(gpos.kern.get(20, 21), Some(-40));
    }

    #[test]
    fn mark_in_an_unknown_class_is_skipped_not_fatal() {
        // MarkToBaseFormat1 with markClassCount 1. Mark glyph 100 is in class 0 and attaches
        // to base 200 at (30,40) - (10,10) = (20,30); mark 101 claims class 5, which the base
        // array does not have and used to make the whole GPOS table fail to load.
        let words: [u16; 29] = [
            1, 44, 52, 1, 12, 34, // format, markCov, baseCov, markClassCount, markArray, baseArray
            2, 0, 10, 5, 16,      // MarkArray: 2 marks; class 0 -> +10, class 5 -> +16
            1, 10, 10,            // mark anchor: format 1, (10,10)
            1, 20, 20,            // mark anchor: format 1, (20,20)
            1, 4,                 // BaseArray: baseCount 1, anchor offset +4
            1, 30, 40,            // base anchor: format 1, (30,40)
            1, 2, 100, 101,       // mark coverage
            1, 1, 200,            // base coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 4, &mut gpos, 1000).expect("mark-to-base parses");
        assert_eq!(gpos.get_mark_to_base(GlyphId(200), GlyphId(100)), Some((20, 30)));
        assert_eq!(gpos.get_mark_to_base(GlyphId(200), GlyphId(101)), None);
    }

    #[test]
    fn single_adjustment_format2_reads_one_record_per_glyph() {
        // OT §GPOS "Single Adjustment" (1), format 2: glyph 30 gets xAdvance -10, glyph 31 +25.
        let words: [u16; 10] = [
            2, 12, 4, 2,          // format, coverageOffset, valueFormat = X_ADVANCE, valueCount
            (-10i16) as u16, 25,  // the two value records
            1, 2, 30, 31,         // coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 1, &mut gpos, 100).expect("single adjustment parses");
        assert_eq!(gpos.get_single(GlyphId(30)).map(|v| v.x_advance), Some(-10));
        assert_eq!(gpos.get_single(GlyphId(31)).map(|v| v.x_advance), Some(25));
        // A glyph outside the coverage has no adjustment.
        assert!(gpos.get_single(GlyphId(32)).is_none());
    }

    #[test]
    fn cursive_attachment_reads_entry_and_exit_anchors() {
        // OT §GPOS "Cursive Attachment" (3), format 1: glyph 40 enters at (5, 6) and exits at
        // (7, 8).
        let words: [u16; 14] = [
            1, 10, 1,       // format, coverageOffset, entryExitCount
            16, 22,         // entryAnchorOffset, exitAnchorOffset
            1, 1, 40,       // coverage
            1, 5, 6,        // entry anchor
            1, 7, 8,        // exit anchor
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 3, &mut gpos, 100).expect("cursive attachment parses");
        assert_eq!(gpos.get_cursive(GlyphId(40)), Some(((5, 6), (7, 8))));
    }

    #[test]
    fn mark_to_mark_attachment_fills_its_own_map() {
        // OT §GPOS "Mark-to-Mark Attachment" (6), format 1 — the same layout as mark-to-base:
        // mark 100 attaches to mark 200 at (30,40) - (10,10) = (20,30).
        let words: [u16; 23] = [
            1, 34, 40, 1, 12, 24, // format, mark1Cov, mark2Cov, classCount, mark1Array, mark2Array
            1, 0, 6,              // MarkArray: one mark, class 0, anchor at +6
            1, 10, 10,            // mark anchor (10,10)
            1, 4,                 // Mark2Array: one mark, anchor at +4
            1, 30, 40,            // mark2 anchor (30,40)
            1, 1, 100,            // mark1 coverage
            1, 1, 200,            // mark2 coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 6, &mut gpos, 100).expect("mark-to-mark parses");
        assert_eq!(gpos.get_mark_to_mark(GlyphId(200), GlyphId(100)), Some((20, 30)));
        // Mark-to-mark must not end up in the mark-to-base map.
        assert!(gpos.get_mark_to_base(GlyphId(200), GlyphId(100)).is_none());
    }

    #[test]
    fn mark_to_ligature_attachment_uses_the_first_component() {
        // OT §GPOS "Mark-to-Ligature Attachment" (5), format 1: ligature 200 has one component
        // whose class-0 anchor is (30,40); mark 100 attaches at (30,40) - (10,10) = (20,30).
        let words: [u16; 25] = [
            1, 38, 44, 1, 12, 24, // format, markCov, ligatureCov, classCount, markArray, ligatureArray
            1, 0, 6,              // MarkArray: one mark, class 0, anchor at +6
            1, 10, 10,            // mark anchor (10,10)
            1, 4,                 // LigatureArray: one ligature, attach table at +4
            1, 4,                 // LigatureAttach: one component, class-0 anchor at +4
            1, 30, 40,            // component anchor (30,40)
            1, 1, 100,            // mark coverage
            1, 1, 200,            // ligature coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 5, &mut gpos, 100).expect("mark-to-ligature parses");
        assert_eq!(gpos.get_mark_to_ligature(GlyphId(200), GlyphId(100)), Some((20, 30)));
    }

    /// The default language system of a hand-built table: script `latn`, one feature `kern`
    /// that names `lookups`.
    fn gpos_with(mut gpos: GPos, lookups: Vec<u16>) -> GPos {
        gpos.scripts = vec![Script {
            tag: Tag(*b"latn"),
            default_language: Some(crate::opentype::gsub::LanguageSystem::with_features(vec![0])),
            languages: Vec::new(),
        }];
        gpos.features = vec![Feature::new(b"kern", lookups)];
        gpos
    }

    fn kern_enabled(tag: Tag) -> bool {
        tag.0 == *b"kern"
    }

    #[test]
    fn single_adjustment_moves_the_advance_of_the_run() {
        // OT §GPOS "Single Adjustment": glyph 30 gets xAdvance -10, glyph 31 +25.
        let words: [u16; 10] = [
            2, 12, 4, 2,
            (-10i16) as u16, 25,
            1, 2, 30, 31,
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 1, &mut gpos, 100).expect("single adjustment parses");
        let gpos = gpos_with(gpos, vec![0]);

        let positions = gpos.position(&[GlyphId(30), GlyphId(31), GlyphId(32)], &kern_enabled);
        assert_eq!(positions[0].x_advance, -10);
        assert_eq!(positions[1].x_advance, 25);
        // A glyph the coverage does not list keeps its default position.
        assert_eq!(positions[2], GlyphPosition::default());
        // Nothing enabled: nothing moves.
        let positions = gpos.position(&[GlyphId(30)], &|_| false);
        assert_eq!(positions[0], GlyphPosition::default());
    }

    #[test]
    fn pair_adjustment_moves_both_glyphs() {
        // OT §GPOS "Pair Adjustment" (2), format 1: 20 followed by 21 gets xPlacement 3 and
        // xAdvance -40 on the first glyph and xAdvance 5 on the second.
        let words: [u16; 14] = [
            1, 22,
            5, 4,
            1, 12,
            1, 21,
            3, (-40i16) as u16,
            5,
            1, 1, 20,
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 2, &mut gpos, 100).expect("pair adjustment parses");
        let gpos = gpos_with(gpos, vec![0]);

        let positions = gpos.position(&[GlyphId(20), GlyphId(21)], &kern_enabled);
        assert_eq!(positions[0].x_offset, 3);
        assert_eq!(positions[0].x_advance, -40);
        assert_eq!(positions[1].x_advance, 5);
    }

    #[test]
    fn mark_to_base_offsets_the_mark() {
        // Base 200 carries the class-0 anchor (30,40), mark 100 its own at (10,10): the mark
        // has to move by (20,30). Mark 101 claims class 5, which has no anchor, so it does not
        // move at all.
        let words: [u16; 29] = [
            1, 44, 52, 1, 12, 34, // format, markCov, baseCov, markClassCount, markArray, baseArray
            2, 0, 10, 5, 16,      // MarkArray: class 0 -> +10, class 5 -> +16
            1, 10, 10,            // mark anchor (10,10)
            1, 20, 20,            // mark anchor (20,20)
            1, 4,                 // BaseArray: one base, anchor at +4
            1, 30, 40,            // base anchor (30,40)
            1, 2, 100, 101,       // mark coverage
            1, 1, 200,            // base coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 4, &mut gpos, 100).expect("mark-to-base parses");
        let gpos = gpos_with(gpos, vec![0]);

        let positions = gpos.position(&[GlyphId(200), GlyphId(100), GlyphId(101)], &kern_enabled);
        // The base itself stays where it is; the class-0 mark moves.
        assert_eq!(positions[0], GlyphPosition::default());
        assert_eq!(positions[1].x_offset, 20);
        assert_eq!(positions[1].y_offset, 30);
        // The mark in the class the base array does not have is not attached.
        assert_eq!(positions[2], GlyphPosition::default());
    }

    #[test]
    fn contextual_lookup_runs_the_lookup_it_names() {
        // Lookup 0 is a single adjustment for glyph 41 (xAdvance -7); lookup 1 is a context
        // positioning subtable that, for "40 41", applies lookup 0 at the second glyph.
        let single: [u16; 8] = [2, 10, 4, 1, (-7i16) as u16, 1, 1, 41];
        let context: [u16; 14] = [
            1, 8, 1, 14,          // format 1, coverageOffset, ruleSetCount, ruleSetOffset[0]
            1, 1, 40,             // coverage
            1, 4,                 // rule set: one rule at +4
            2, 1,                 // glyphCount 2, posCount 1
            41,                   // input sequence
            1, 0,                 // sequenceIndex 1, lookupListIndex 0
        ];
        let mut gpos = GPos::default();
        parse_positioning(&words_to_bytes(&single), 1, &mut gpos, 100).expect("single parses");
        gpos.lookups.push(Vec::new());
        gpos.flags.push(0);
        parse_positioning(&words_to_bytes(&context), 7, &mut gpos, 100).expect("context parses");
        let gpos = gpos_with(gpos, vec![1]);

        let positions = gpos.position(&[GlyphId(40), GlyphId(41)], &kern_enabled);
        assert_eq!(positions[1].x_advance, -7);
        // The context rule needs the glyph it covers in front: alone, 41 is not touched (the
        // feature only names the context lookup, not the single adjustment).
        let positions = gpos.position(&[GlyphId(41)], &kern_enabled);
        assert_eq!(positions[0], GlyphPosition::default());
    }

    #[test]
    fn ignore_marks_lookup_flag_skips_a_mark() {
        // OT §"Lookup Flags": a lookup with IgnoreMarks leaves mark glyphs alone. Glyph 100 is
        // a mark in the GDEF table and gets xAdvance -5 from the single adjustment.
        let words: [u16; 10] = [2, 12, 4, 2, (-5i16) as u16, (-5i16) as u16, 1, 2, 100, 101];
        let data = words_to_bytes(&words);
        let gdef_words: [u16; 11] = [
            1, 0, 12, 0, 0, 0,    // GDEF 1.0, glyphClassDefOffset 12
            2, 1, 100, 100, 3,    // classDef format 2: glyphs 100..100 -> class 3 (mark)
        ];
        let gdef = crate::opentype::gdef::parse_gdef(&words_to_bytes(&gdef_words)).expect("GDEF parses");

        let mut gpos = GPos::default();
        parse_positioning(&data, 1, &mut gpos, 100).expect("single adjustment parses");
        gpos.flags = vec![0x0008];
        let gpos = gpos_with(gpos, vec![0]);

        // Without the flag the mark moves.
        let positions = gpos.position(&[GlyphId(100)], &kern_enabled);
        assert_eq!(positions[0].x_advance, -5);
        // With it, the lookup skips the glyph.
        let positions = gpos.position_with(&[GlyphId(100)], &kern_enabled, Some(&gdef));
        assert_eq!(positions[0], GlyphPosition::default());
        // Glyph 101 is not a mark (no GDEF class), so IgnoreMarks does not hide it.
        let positions = gpos.position_with(&[GlyphId(101)], &kern_enabled, Some(&gdef));
        assert_eq!(positions[0].x_advance, -5);
    }
}
