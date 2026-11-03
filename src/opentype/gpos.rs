use std::collections::{HashMap, HashSet};
use nom::{
    bytes::complete::{take},
    number::complete::{be_i16, be_u16, be_u32},
    sequence::{tuple},
};
use crate::{R, GlyphId, GlyphPosition, FontError};
use crate::parsers::{*};
use crate::opentype::{Maxp, Tag, parse_extension, parse_lookup_list, coverage_table, parse_class_def};
use crate::opentype::gdef::{GDef, MarkClass, glyph_ignored, LookupFlags};
use crate::opentype::gsub::{
    Context, Feature, OpBudget, Script, MAX_NESTING, enabled_lookups, parse_chain_context_subst,
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
    /// Per lookup: its `lookupFlag` (OT §"Lookup Flags"), which says which glyphs it skips,
    /// and the `markFilteringSet` its `UseMarkFilteringSet` bit names.
    flags: Vec<LookupFlags>,
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
    Cursive(HashMap<u16, CursiveAnchors>),
    /// OT §GPOS "Mark-to-Base Attachment" (4): (base, mark) -> offset.
    MarkToBase(HashMap<(u16, u16), (i16, i16)>),
    /// OT §GPOS "Mark-to-Ligature Attachment" (5): an anchor per mark class per component.
    MarkToLigature(LigatureMarks),
    /// OT §GPOS "Mark-to-Mark Attachment" (6): (base mark, mark) -> offset.
    MarkToMark(HashMap<(u16, u16), (i16, i16)>),
    /// OT §GPOS "Context Positioning" (7) and "Chaining Context Positioning" (8).
    Context(Context),
}

/// OT §GPOS "Cursive Attachment" (3): the entry and exit anchor of one glyph. A NULL offset in
/// the table means the glyph has no such anchor — a hole in the chain — which is not the same as
/// the anchor `(0, 0)`; a real anchor there is legal and must not be confused with "absent".
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CursiveAnchors {
    pub entry: Option<(i16, i16)>,
    pub exit: Option<(i16, i16)>,
}

/// OT §GPOS "Mark-to-Ligature Attachment" (5): a ligature carries one anchor per mark class per
/// component, and the mark array says which class each mark belongs to.
///
/// A run of glyphs does not record which component of the ligature a mark sits over — the
/// substitution that made the ligature dropped that — so a mark takes the component given by the
/// number of marks between the ligature and itself: the first mark over a two-component ligature
/// belongs to the first component, the second to the second, and a third stays on the last.
#[derive(Debug, Clone, Default)]
struct LigatureMarks {
    /// mark glyph -> (mark class, anchor).
    marks: HashMap<u16, (u16, (i16, i16))>,
    /// ligature glyph -> per component, per mark class.
    ligatures: HashMap<u16, Vec<Vec<Option<(i16, i16)>>>>,
}

impl LigatureMarks {
    /// The offset of `mark` over `component` of `ligature`: the component's anchor minus the
    /// mark's, or `None` if either the ligature or the mark has no anchor for that class.
    fn delta(&self, ligature: u16, mark: u16, component: usize) -> Option<(i16, i16)> {
        let &(mark_class, mark_anchor) = self.marks.get(&mark)?;
        let components = self.ligatures.get(&ligature)?;
        let anchors = components.get(component).or_else(|| components.last())?;
        let (base_x, base_y) = anchors.get(usize::from(mark_class)).copied().flatten()?;
        Some((base_x - mark_anchor.0, base_y - mark_anchor.1))
    }
}

/// Which glyph a mark attaches to (OT §GPOS "Mark Attachment").
#[derive(Copy, Clone, PartialEq, Eq)]
enum AttachTo {
    /// Mark-to-base (4) and mark-to-ligature (5): the nearest preceding glyph that is not a
    /// mark of its own. The marks in between belong to the same base.
    Base,
    /// Mark-to-mark (6): the nearest preceding glyph that is a mark, which is how two accents
    /// stack on one letter.
    Mark,
}

/// One pair positioning subtable: explicit glyph pairs (format 1) or a class matrix (format 2).
///
/// The class definitions belong to their subtable, not to the `GPOS` table: two class-pair
/// subtables of the same font can assign a glyph entirely different classes, and the flat
/// `KernTable` that `Font::kerning` reads has room for only one of them, so the classes are
/// kept here and `KernTable` is filled for that accessor alone.
#[derive(Debug, Clone, Default)]
struct PairSets {
    pairs: HashMap<(u16, u16), (ValueRecord, ValueRecord)>,
    class_pairs: HashMap<(u16, u16), (ValueRecord, ValueRecord)>,
    /// Format 2 only: the glyphs that may start a pair (OT §"Pair Adjustment" — the coverage
    /// table is consulted even though the classes decide which value applies).
    coverage: HashSet<u16>,
    /// Whether this is a format 2 subtable at all; a format 1 subtable has no classes and no
    /// coverage, and a pair it does not list simply does not exist.
    class_based: bool,
    /// Format 2: `class1Count`, `class2Count` and how many matrix cells the subtable really
    /// holds (row-major). `class_pairs` keeps only the non-zero cells, so these say which zero
    /// cells are real: OT §GPOS "Pair Adjustment" ends the lookup at the first subtable that
    /// matches a pair, zero values or not.
    class_counts: (u16, u16),
    cells_read: usize,
    left_classes: HashMap<u16, u16>,
    right_classes: HashMap<u16, u16>,
    /// Whether the subtable carries a value record for the second glyph of a pair. OT §GPOS
    /// "Pair Adjustment": when it does, the lookup resumes at the glyph *after* the second one,
    /// because that record may have changed the advance the next pair is measured from.
    second_advance: bool,
}

impl PairSets {
    /// The value records of `left` followed by `right` in this subtable, or `None` if it has
    /// nothing to say about the pair.
    fn lookup(&self, left: u16, right: u16) -> Option<(ValueRecord, ValueRecord)> {
        if let Some(pair) = self.pairs.get(&(left, right)) {
            return Some(pair.clone());
        }
        if !self.class_based || !self.coverage.contains(&left) {
            return None;
        }
        // A glyph the subtable's ClassDefs do not mention is class 0, which has its own row
        // and column in the matrix.
        let left_class = self.left_classes.get(&left).copied().unwrap_or(0);
        let right_class = self.right_classes.get(&right).copied().unwrap_or(0);
        if let Some(pair) = self.class_pairs.get(&(left_class, right_class)) {
            return Some(pair.clone());
        }
        let (rows, columns) = self.class_counts;
        let cell = usize::from(left_class) * usize::from(columns) + usize::from(right_class);
        if left_class < rows && right_class < columns && cell < self.cells_read {
            return Some(Default::default());
        }
        None
    }
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
    /// OT §GPOS "Cursive Attachment": the entry and exit anchors of one glyph, each absent when
    /// the table has a NULL offset for it.
    pub fn get_cursive(&self, glyph: GlyphId) -> Option<CursiveAnchors> {
        let key = glyph_key(glyph)?;
        self.subtables().find_map(|subtable| match subtable {
            PosSubtable::Cursive(anchors) => anchors.get(&key).copied(),
            _ => None,
        })
    }
    /// OT §GPOS "Mark-to-Ligature Attachment": where the mark is placed relative to the first
    /// component of the ligature.
    pub fn get_mark_to_ligature(&self, ligature: GlyphId, mark: GlyphId) -> Option<(i16, i16)> {
        let key = (glyph_key(ligature)?, glyph_key(mark)?);
        self.subtables().find_map(|subtable| match subtable {
            PosSubtable::MarkToLigature(marks) => marks.delta(key.0, key.1, 0),
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
    /// OT §GPOS "Pair Adjustment": both value records of `left` followed by `right`. The
    /// subtables are tried in lookup order, each with its own coverage and classes.
    pub fn get_pair(&self, left: GlyphId, right: GlyphId) -> Option<(ValueRecord, ValueRecord)> {
        let left_key = glyph_key(left)?;
        let right_key = glyph_key(right)?;
        self.subtables().find_map(|subtable| match subtable {
            PosSubtable::Pair(sets) => sets.lookup(left_key, right_key),
            _ => None,
        })
    }

    /// The advance `left` gets when followed by `right`, which is what `Font::kerning` reports.
    ///
    /// Only the lookups the `kern` feature of the default language system names count, each
    /// contributing the first of its pair subtables that matches (a `vkrn` or `dist` lookup is
    /// not kerning). A font whose layout data came from the legacy `kern` table has no lookups
    /// and uses the flat maps; for a real `GPOS` table those maps are not consulted, because
    /// they merge the class numbers of unrelated subtables.
    pub fn kerning(&self, left: GlyphId, right: GlyphId) -> Option<i16> {
        let (left_key, right_key) = (glyph_key(left)?, glyph_key(right)?);
        if self.lookups.is_empty() {
            return self.kern.get(left_key, right_key);
        }
        let kern = |tag: Tag| tag.0 == *b"kern";
        let mut total: Option<i16> = None;
        // A table without script or feature lists (the lists are optional) cannot name a
        // `kern` feature; its pair lookups are all there is.
        let lookup_indices: Vec<u16> = if self.scripts.is_empty() || self.features.is_empty() {
            (0 .. self.lookups.len() as u16).collect()
        } else {
            enabled_lookups(&self.scripts, &self.features, &kern)
        };
        for lookup_idx in lookup_indices {
            let subtables = match self.lookups.get(lookup_idx as usize) {
                Some(subtables) => subtables,
                None => continue,
            };
            let found = subtables.iter().find_map(|subtable| match subtable {
                PosSubtable::Pair(sets) => sets.lookup(left_key, right_key),
                _ => None,
            });
            if let Some((first, _second)) = found {
                total = Some(total.unwrap_or(0).saturating_add(first.x_advance));
            }
        }
        total
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
        self.position_with_advances(glyphs, enabled, gdef, &|_glyph| 0)
    }

    /// `position_with`, plus the horizontal advance the font gives each glyph.
    ///
    /// OT §GPOS "Cursive Attachment" and "Mark Attachment" place a glyph on an *anchor* of
    /// another glyph, and an anchor is a point in that glyph's own coordinate system. The run
    /// being positioned carries no origins, only per-glyph advances, so the distance between the
    /// two origins has to be summed from them — that is what `advance` is for.
    pub fn position_with_advances(
        &self,
        glyphs: &[GlyphId],
        enabled: &dyn Fn(Tag) -> bool,
        gdef: Option<&GDef>,
        advance: &dyn Fn(GlyphId) -> i16,
    ) -> Vec<GlyphPosition> {
        let mut out = vec![GlyphPosition::default(); glyphs.len()];
        let lookup_indices = enabled_lookups(&self.scripts, &self.features, enabled);
        if lookup_indices.is_empty() {
            return out;
        }
        // OT §"Lookups": each lookup runs across the whole run before the next one does. A
        // positioning lookup never changes the length of the run, so nothing moves under us.
        let mut budget = OpBudget::new(glyphs.len());
        for &lookup_idx in &lookup_indices {
            let mut position = 0;
            while position < glyphs.len() {
                // A pair adjustment that carries a value record for the second glyph resumes
                // after that glyph (OT §GPOS "Pair Adjustment"); anything else moves on by one.
                position += match self.apply_lookup_at(lookup_idx, glyphs, &mut out, position, 0, gdef, advance, &mut budget) {
                    Some(step) => step.max(1),
                    None => 1,
                };
            }
        }
        out
    }


        /// Applies one lookup at `position`: how many glyphs of the run it used up, or `None` if no
    /// subtable of the lookup applied there.
    fn apply_lookup_at(
        &self,
        lookup_idx: u16,
        glyphs: &[GlyphId],
        positions: &mut [GlyphPosition],
        position: usize,
        depth: u8,
        gdef: Option<&GDef>,
        advance: &dyn Fn(GlyphId) -> i16,
        budget: &mut OpBudget,
    ) -> Option<usize> {
        // A context rule may name the lookup it sits in, so the recursion cap alone still allows
        // an exponential number of applications; the budget bounds the work per call.
        if depth > 0 && !budget.take() {
            return None;
        }
        let subtables = self.lookups.get(lookup_idx as usize)?;
        let flag = self.flags.get(lookup_idx as usize).copied().unwrap_or_default();
        let glyph = glyph_key(*glyphs.get(position)?)?;
        // OT §"Lookup Flags": a lookup skips the glyph classes its flag names.
        if glyph_ignored(gdef, flag, glyph) {
            return None;
        }
        let skip = |gid: u16| glyph_ignored(gdef, flag, gid);
        // OT §"Lookups": only the first subtable of a lookup that matches is used.
        for subtable in subtables {
            let applied = match subtable {
                PosSubtable::Single(values) => match values.get(&glyph) {
                    Some(value) => {
                        add(positions, position, value);
                        Some(1)
                    }
                    None => None,
                },
                PosSubtable::Pair(sets) => self.apply_pair(sets, glyphs, positions, position, glyph, &skip),
                PosSubtable::Cursive(anchors) =>
                    self.apply_cursive(anchors, glyphs, positions, position, glyph, &skip, flag, advance),
                PosSubtable::MarkToBase(marks) =>
                    self.apply_mark_attachment(marks, glyphs, positions, position, glyph, &skip, gdef, advance, AttachTo::Base),
                PosSubtable::MarkToMark(marks) =>
                    self.apply_mark_attachment(marks, glyphs, positions, position, glyph, &skip, gdef, advance, AttachTo::Mark),
                PosSubtable::MarkToLigature(marks) =>
                    self.apply_ligature_attachment(marks, glyphs, positions, position, glyph, &skip, gdef, advance),
                PosSubtable::Context(context) =>
                    self.apply_context(context, glyphs, positions, position, depth, flag, gdef, advance, budget),
            };
            if applied.is_some() {
                return applied;
            }
        }
        None
    }


        /// OT §GPOS "Pair Adjustment": the value records of the pair starting at `position` go on
    /// the first glyph and on the second one.
    fn apply_pair(&self, sets: &PairSets, glyphs: &[GlyphId], positions: &mut [GlyphPosition], position: usize, glyph: u16, skip: &dyn Fn(u16) -> bool) -> Option<usize> {
        let second_index = visible_after(glyphs, position, 1, skip)?;
        let second = glyph_key(*glyphs.get(second_index)?)?;
        let (first_value, second_value) = sets.lookup(glyph, second)?;
        add(positions, position, &first_value);
        add(positions, second_index, &second_value);
        // OT §GPOS "Pair Adjustment": a subtable with a value record for the second glyph resumes
        // at the glyph *after* it — the record may have changed the advance the next pair is
        // measured from — so the second glyph is used up.
        Some(if sets.second_advance { 2 } else { 1 })
    }


        /// OT §GPOS "Cursive Attachment": the entry anchor of a glyph is laid onto the exit anchor
    /// of the glyph before it. Both are points in their own glyph's coordinates, so the move is
    /// the anchor difference less the distance between the two origins.
    fn apply_cursive(
        &self,
        anchors: &HashMap<u16, CursiveAnchors>,
        glyphs: &[GlyphId],
        positions: &mut [GlyphPosition],
        position: usize,
        glyph: u16,
        skip: &dyn Fn(u16) -> bool,
        flag: LookupFlags,
        advance: &dyn Fn(GlyphId) -> i16,
    ) -> Option<usize> {
        let entry = anchors.get(&glyph)?.entry?;
        let previous = visible_before(glyphs, position, 0, skip)?;
        let previous_glyph = glyph_key(*glyphs.get(previous)?)?;
        let exit = anchors.get(&previous_glyph)?.exit?;
        let shift: i32 = (previous .. position)
            .map(|index| advance_at(glyphs, positions, index, advance))
            .sum();
        // OT §"Lookup Flags": `RIGHT_TO_LEFT` (0x0001) says the chain runs against the glyph
        // order, so the advance in between brings the origins closer instead of further apart.
        let dx = if flag.flag & 0x0001 != 0 {
            i32::from(exit.0) - i32::from(entry.0) + shift
        } else {
            i32::from(exit.0) - i32::from(entry.0) - shift
        };
        let dy = i32::from(exit.1) - i32::from(entry.1);
        // The previous glyph may itself have been moved by this chain (or by a placement); its
        // anchor moved with it, so the offset accumulates along the chain instead of every
        // glyph being placed relative to an unmoved neighbour.
        let (previous_x, previous_y) = positions.get(previous)
            .map_or((0, 0), |p| (i32::from(p.x_offset), i32::from(p.y_offset)));
        if let Some(slot) = positions.get_mut(position) {
            slot.x_offset = clamp_i16(i32::from(slot.x_offset) + dx + previous_x);
            slot.y_offset = clamp_i16(i32::from(slot.y_offset) + dy + previous_y);
        }
        Some(1)
    }


        /// OT §GPOS "Mark Attachment" (4 and 6): a mark moves to the anchor its base carries for its
    /// class; the map holds that difference already. The base is the nearest preceding base glyph
    /// (4) or the nearest preceding mark (6) — not just any earlier glyph that happens to have an
    /// anchor for this mark, which is what made a mark jump over its own base.
    fn apply_mark_attachment(
        &self,
        marks: &HashMap<(u16, u16), (i16, i16)>,
        glyphs: &[GlyphId],
        positions: &mut [GlyphPosition],
        position: usize,
        glyph: u16,
        skip: &dyn Fn(u16) -> bool,
        gdef: Option<&GDef>,
        advance: &dyn Fn(GlyphId) -> i16,
        attach: AttachTo,
    ) -> Option<usize> {
        let base_index = attach_index(glyphs, position, skip, gdef, attach)?;
        let base = glyph_key(*glyphs.get(base_index)?)?;
        let &(dx, dy) = marks.get(&(base, glyph))?;
        place_mark(positions, position, base_index, glyphs, dx, dy, advance);
        Some(1)
    }

    /// OT §GPOS "Mark-to-Ligature Attachment" (5): like mark-to-base, except that the ligature
    /// carries one anchor per component and the mark takes the one for its component.
    fn apply_ligature_attachment(
        &self,
        marks: &LigatureMarks,
        glyphs: &[GlyphId],
        positions: &mut [GlyphPosition],
        position: usize,
        glyph: u16,
        skip: &dyn Fn(u16) -> bool,
        gdef: Option<&GDef>,
        advance: &dyn Fn(GlyphId) -> i16,
    ) -> Option<usize> {
        let base_index = attach_index(glyphs, position, skip, gdef, AttachTo::Base)?;
        let ligature = glyph_key(*glyphs.get(base_index)?)?;
        // The run does not record which component a mark sits over; the marks already stacked on
        // the ligature are the components used up so far.
        let component = (base_index + 1 .. position)
            .filter(|&index| glyphs.get(index).and_then(|&gid| glyph_key(gid)).map_or(false, |gid| is_mark(gdef, gid)))
            .count();
        let (dx, dy) = marks.delta(ligature, glyph, component)?;
        place_mark(positions, position, base_index, glyphs, dx, dy, advance);
        Some(1)
    }


        /// OT §GPOS "Context Positioning" (7, 8): run the lookups the first matching rule names at
    /// the glyphs of the run. Positioning never changes the length of the run, so this consumes
    /// exactly one glyph.
    fn apply_context(&self, context: &Context, glyphs: &[GlyphId], positions: &mut [GlyphPosition], position: usize, depth: u8, flag: LookupFlags, gdef: Option<&GDef>, advance: &dyn Fn(GlyphId) -> i16, budget: &mut OpBudget) -> Option<usize> {
        if depth >= MAX_NESTING {
            debug!("GPOS: not following context lookups nested deeper than {}", MAX_NESTING);
            return None;
        }
        let skip = |gid: u16| glyph_ignored(gdef, flag, gid);
        let rule = context.matched_rule(glyphs, position, &skip)?;
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
            applied |= self
                .apply_lookup_at(lookup_index, glyphs, positions, target, depth + 1, gdef, advance, budget)
                .is_some();
        }
        if applied { Some(1) } else { None }
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

/// How many glyphs back a mark may look for its base. A real font stacks a few accents on a
/// letter, not dozens, and the bound keeps a run of nothing but marks from costing O(n²).
const MAX_MARK_CHAIN: usize = 64;

/// Whether `gid` is a mark, as `GDEF` says. Without `GDEF` nothing is known to be one.
fn is_mark(gdef: Option<&GDef>, gid: u16) -> bool {
    gdef.and_then(|gdef| gdef.mark_class(gid)) == Some(MarkClass::Mark)
}

/// The index of the glyph a mark attaches to: the nearest preceding glyph that is not a mark
/// (`AttachTo::Base`) or the nearest preceding mark (`AttachTo::Mark`).
///
/// OT §GPOS "Mark Attachment": the base of a mark is the closest preceding base glyph — the
/// marks between the two belong to the same base — and a mark-to-mark lookup stacks onto the
/// mark immediately before it. Searching further back for *any* glyph that happens to have an
/// anchor for this mark is not what the spec says.
fn attach_index(
    glyphs: &[GlyphId],
    position: usize,
    skip: &dyn Fn(u16) -> bool,
    gdef: Option<&GDef>,
    attach: AttachTo,
) -> Option<usize> {
    let gdef = match gdef {
        Some(gdef) => gdef,
        // Without `GDEF` no glyph is known to be a mark, so the nearest preceding glyph is the
        // only candidate there is.
        None => return visible_before(glyphs, position, 0, skip),
    };
    for n in 0 .. MAX_MARK_CHAIN {
        let index = visible_before(glyphs, position, n, skip)?;
        let mark = glyphs.get(index).and_then(|&gid| glyph_key(gid)).map_or(false, |gid| is_mark(Some(gdef), gid));
        match attach {
            AttachTo::Base if !mark => return Some(index),
            AttachTo::Mark if mark => return Some(index),
            // A mark-to-mark lookup whose nearest neighbour is not a mark has no base at all.
            AttachTo::Mark => return None,
            AttachTo::Base => {}
        }
    }
    None
}

/// The device-space advance of the glyph at `index`: what the font says plus any adjustment a
/// pair lookup has already put on it.
fn advance_at(glyphs: &[GlyphId], positions: &[GlyphPosition], index: usize, advance: &dyn Fn(GlyphId) -> i16) -> i32 {
    let font_advance = glyphs.get(index).map_or(0, |&glyph| i32::from(advance(glyph)));
    let adjusted = positions.get(index).map_or(0, |position| i32::from(position.x_advance));
    font_advance + adjusted
}

/// Places a mark at the anchor its base carries for it. The anchor difference is relative to the
/// two glyph origins, so the marks move back by the advance of the base and of every glyph in
/// between, which is what makes them sit over the base instead of to the right of it.
fn place_mark(
    positions: &mut [GlyphPosition],
    position: usize,
    base_index: usize,
    glyphs: &[GlyphId],
    dx: i16,
    dy: i16,
    advance: &dyn Fn(GlyphId) -> i16,
) {
    if position <= base_index {
        return;
    }
    let shift: i32 = (base_index .. position)
        .map(|index| advance_at(glyphs, positions, index, advance))
        .sum();
    let base_x_offset = positions.get(base_index).map_or(0, |base| i32::from(base.x_offset));
    let base_y_offset = positions.get(base_index).map_or(0, |base| i32::from(base.y_offset));
    if let Some(slot) = positions.get_mut(position) {
        slot.x_offset = clamp_i16(i32::from(dx) - shift + base_x_offset);
        slot.y_offset = clamp_i16(i32::from(dy) + base_y_offset);
    }
}

/// Keeps a position that does not fit an `i16` inside one instead of wrapping around.
fn clamp_i16(value: i32) -> i16 {
    value.clamp(i32::from(i16::MIN), i32::from(i16::MAX)) as i16
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
            gpos.flags.push(LookupFlags::default());
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
        gpos.flags.push(LookupFlags::default());
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

/// Reads one `ClassDef` of a pair adjustment subtable into `classes`. An offset of 0 means the
/// subtable has no class definition at all — every glyph is then class 0 — and an unreadable
/// one costs the classes, not the rest of the table.
fn parse_optional_class_def(data: &[u8], class_def_offset: u16, classes: &mut HashMap<u16, u16>, name: &str) {
    let class_def = match Offset(class_def_offset).of_opt(data) {
        Some(class_def) => class_def,
        None => {
            if class_def_offset != 0 {
                warn!("GPOS pair adjustment: unreadable {} offset {}", name, class_def_offset);
            }
            return;
        }
    };
    if let Err(err) = parse_class_def(class_def, classes) {
        warn!("GPOS pair adjustment: {} is unreadable: {:?}", name, err);
        classes.clear();
    }
}

/// OT §GPOS "Pair Adjustment" (lookup type 2), formats 1 and 2. Fills the flat `i16` maps that
/// `Font::kerning` uses side by side with the value records, which keep the placements the
/// advance-only maps cannot hold.
fn parse_pair_adjustment<'a>(data: &'a [u8], gpos: &mut GPos) -> Result<PairSets, FontError> {
    // The flat maps are filled only once the whole subtable parsed: a subtable that fails
    // halfway is skipped by `parse_lookup_list` and must not leave a part of itself behind.
    let mut flat = KernTable::default();
    let mut pairs = PairSets::default();
    let (i, format) = be_u16(data)?;
    match format {
        1 => {
            let (i, coverage_off) = offset(i)?;
            let (i, value_format_1) = be_u16(i)?;
            let (i, value_format_2) = be_u16(i)?;
            let (i, pair_set_count) = be_u16(i)?;
            pairs.second_advance = value_format_2 != 0;

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
                    flat.glyph_pairs.insert((first_glyph, second_glyph), value_record_1.x_advance);
                    pairs.pairs.insert((first_glyph, second_glyph), (value_record_1, value_record_2));
                }
            }
        },
        2 => {
            let (i, coverage_off) = offset(i)?;
            let (i, value_format_1) = be_u16(i)?;
            let (i, value_format_2) = be_u16(i)?;
            let (i, class_def_1_offset) = be_u16(i)?;
            let (i, class_def_2_offset) = be_u16(i)?;
            let (i, class_1_count) = be_u16(i)?;
            let (i, class_2_count) = be_u16(i)?;

            pairs.class_based = true;
            // OT §GPOS "Pair Adjustment": with a value record for the second glyph the lookup
            // resumes after that glyph, since the record may move the next pair's origin.
            pairs.second_advance = value_format_2 != 0;
            // OT §GPOS "Pair Adjustment" format 2: the coverage lists the glyphs that may start
            // a pair. It is not optional — a glyph it does not list has no pair here, however
            // the ClassDefs classify it.
            match coverage_off.of(data) {
                Ok(coverage) => pairs.coverage = coverage_table(coverage)?.collect(),
                Err(_) => warn!("GPOS pair adjustment: unreadable coverage offset {}", coverage_off.0),
            }

            // The class definitions are subtable-local: a second class-pair subtable may put the
            // same glyphs into entirely different classes. An offset of 0 means "no glyph in
            // that class definition", i.e. every glyph is class 0 — the default row and column
            // of the matrix — not a broken subtable.
            parse_optional_class_def(data, class_def_1_offset, &mut pairs.left_classes, "ClassDef1");
            parse_optional_class_def(data, class_def_2_offset, &mut pairs.right_classes, "ClassDef2");

            // The flat maps `Font::kerning` reads keep one table's view of the classes; the
            // first definition wins so a later subtable cannot silently change an earlier one.
            for (&glyph, &class) in &pairs.left_classes {
                flat.left_classes.insert(glyph, class);
            }
            for (&glyph, &class) in &pairs.right_classes {
                flat.right_classes.insert(glyph, class);
            }


            // `class_1_count * class_2_count` overflows u16 (300 x 300 classes is a panic in
            // debug builds), and a bogus count would let `iterator_n` run for billions of
            // iterations and `reserve` allocate gigabytes. A matrix cannot hold more values
            // than the subtable has bytes.
            let total = usize::from(class_1_count) * usize::from(class_2_count);
            let total = total.min(data.len());
            flat.class_pairs.reserve(total.min(1 << 16));
            pairs.class_counts = (class_1_count, class_2_count);
            pairs.cells_read = 0;

            let iter = (0 .. class_1_count).cartesian_product(0 .. class_2_count)
                .zip(iterator_n(i, tuple((value_record(value_format_1), value_record(value_format_2))), total));

            // With both value formats 0 a record is zero bytes long, so every cell of the matrix
            // exists however short the data is.
            let all_cells = value_format_1 == 0 && value_format_2 == 0;
            for ((class_1, class_2), (value_record_1, value_record_2)) in iter {
                pairs.cells_read += 1;
                if value_record_1 != ValueRecord::default() || value_record_2 != ValueRecord::default() {
                    pairs.class_pairs.insert((class_1, class_2), (value_record_1.clone(), value_record_2));
                }
                if value_record_1.x_advance != 0 {
                    flat.class_pairs.insert((class_1, class_2), value_record_1.x_advance);
                }
            }
            if all_cells {
                pairs.cells_read = usize::MAX;
            }
        }
        n => return error!("unsupported pair adjustment format {}", n)
    }
    // Merge into the legacy flat view (first definition of a glyph's class wins).
    gpos.kern.glyph_pairs.extend(flat.glyph_pairs);
    for (glyph, class) in flat.left_classes { gpos.kern.left_classes.entry(glyph).or_insert(class); }
    for (glyph, class) in flat.right_classes { gpos.kern.right_classes.entry(glyph).or_insert(class); }
    gpos.kern.class_pairs.extend(flat.class_pairs);
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
/// glyph. A NULL offset means the glyph has no such anchor — a hole in the chain — which is not
/// the same as an anchor at `(0, 0)`, so it is kept as `None`.
fn parse_cursive(data: &[u8]) -> Result<HashMap<u16, CursiveAnchors>, FontError> {
    let mut anchors = HashMap::new();
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);
    let (i, coverage_offset) = be_u16(i)?;
    let (i, entry_exit_count) = be_u16(i)?;
    let coverage = coverage_table(slice!(data, coverage_offset as usize ..))?;
    for (glyph, (entry_offset, exit_offset)) in coverage.zip(iterator_n(i, tuple((be_u16, be_u16)), entry_exit_count)) {
        let entry = match Offset(entry_offset).of_opt(data) {
            Some(anchor) => Some(anchor_coordinates(anchor)?),
            None => None,
        };
        let exit = match Offset(exit_offset).of_opt(data) {
            Some(anchor) => Some(anchor_coordinates(anchor)?),
            None => None,
        };
        if entry.is_none() && exit.is_none() {
            continue;
        }
        anchors.insert(glyph, CursiveAnchors { entry, exit });
    }
    Ok(anchors)
}


/// OT §GPOS "Anchor Tables": only the `(x, y)` pair is used. Formats 2 and 3 carry a contour
/// point as well, but the coordinates are the same leading fields.
fn anchor_coordinates(data: &[u8]) -> Result<(i16, i16), FontError> {
    let anchor = AnchorTable::parse(data)?;
    Ok((anchor.x, anchor.y))
}

/// OT §GPOS "Mark-to-Ligature Attachment" (lookup type 5), format 1. A ligature carries one
/// anchor per mark class per component, and the mark array says which class each mark belongs
/// to; the component is decided when the mark is placed (see `LigatureMarks`).
fn parse_mark_to_ligature_attachment(data: &[u8]) -> Result<LigatureMarks, FontError> {
    let mut ligature_marks = LigatureMarks::default();
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
    for (mark_gid, entry) in mark_coverage.zip(mark_array.iter()) {
        let (mark_class, mark_anchor) = entry?;
        if mark_class >= mark_class_count {
            warn!("GPOS: mark glyph {} has class {} but markClassCount is {}",
                  mark_gid, mark_class, mark_class_count);
            continue;
        }
        ligature_marks.marks.insert(mark_gid, (mark_class, (mark_anchor.x, mark_anchor.y)));
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
        // One ComponentRecord per component: one anchor offset per mark class. A NULL offset
        // means the component has no anchor for that class.
        let mut components = Vec::with_capacity(usize::from(component_count.min(64)));
        let mut rest = i;
        for _ in 0 .. component_count {
            let offsets = match array::<uint16, _>(usize::from(mark_class_count))(rest) {
                Ok((after, offsets)) => (after, offsets),
                // Short data: keep the components read so far rather than the whole table.
                Err(_) => break,
            };
            rest = offsets.0;
            let mut anchors: Vec<Option<(i16, i16)>> = Vec::new();
            for offset in offsets.1.iter() {
                let anchor = match offset {
                    Ok(offset) if offset != 0 => attach
                        .get(offset as usize ..)
                        .and_then(|bytes| AnchorTable::parse(bytes).ok())
                        .map(|anchor| (anchor.x, anchor.y)),
                    // A NULL offset: no anchor for this class. Short data: the same.
                    _ => None,
                };
                anchors.push(anchor);
            }
            components.push(anchors);
        }
        if components.is_empty() {
            continue;
        }
        ligature_marks.ligatures.insert(ligature_gid, components);
    }
    Ok(ligature_marks)
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
        let words: [u16; 17] = [
            2, 28,                // PairPos format 2, coverageOffset
            0, 0,                 // valueFormat1, valueFormat2
            16, 22,               // classDef1Offset, classDef2Offset
            300, 300,             // class1Count, class2Count
            1, 0, 0,              // classDef1: format 1, startGlyph 0, glyphCount 0
            1, 0, 0,              // classDef2
            1, 1, 20,             // coverage: format 1, glyphCount 1, glyph 20
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        let pairs = parse_pair_adjustment(&data, &mut gpos).expect("pair adjustment parses");
        assert!(gpos.kern.class_pairs.is_empty());
        assert!(pairs.class_pairs.is_empty());
        // Glyph 20 is covered and both of its classes exist: the (empty, all-zero) matrix cell
        // matches, so this subtable ends the lookup with a zero adjustment. A glyph that is not
        // covered still never matches.
        assert_eq!(pairs.lookup(20, 21).map(|(a, _)| a.x_advance), Some(0));
        assert!(pairs.lookup(21, 21).is_none());
    }

    fn c2_subtable_first() -> Vec<u8> {
        let words: [u16; 23] = [
            2, 40,                // format 2, coverageOffset
            4, 0,                 // valueFormat1 = X_ADVANCE, valueFormat2
            24, 32,               // classDef1Offset, classDef2Offset
            2, 2,                 // class1Count, class2Count
            0, 0, 0, (-40i16) as u16, // matrix [0][0] [0][1] [1][0] [1][1]
            1, 20, 1, 1,          // classDef1: glyph 20 -> class 1
            1, 21, 1, 1,          // classDef2: glyph 21 -> class 1
            1, 1, 20,             // coverage: glyph 20
        ];
        words_to_bytes(&words)
    }

    fn c2_subtable_second() -> Vec<u8> {
        let words: [u16; 24] = [
            2, 40,                // format 2, coverageOffset
            4, 0,                 // valueFormat1 = X_ADVANCE, valueFormat2
            24, 32,               // classDef1Offset, classDef2Offset
            2, 2,                 // class1Count, class2Count
            0, (-3i16) as u16, 0, 0, // matrix [0][0] [0][1] [1][0] [1][1]
            1, 30, 1, 1,          // classDef1: glyph 30 -> class 1, glyph 20 -> class 0
            1, 21, 1, 1,          // classDef2: glyph 21 -> class 1
            1, 2, 20, 30,         // coverage: glyphs 20 and 30
        ];
        words_to_bytes(&words)
    }

    #[test]
    fn class_pairs_are_local_to_their_subtable() {
        // Audit C2: two class-pair subtables of one lookup. Both cover glyph 20 and both
        // classify glyph 21 as class 1, but the first puts glyph 20 in class 1 (cell -40) and
        // the second leaves it in class 0 (cell -3). Classes are subtable-local, so the second
        // subtable must not inherit the first one's ClassDef1.
        let first = c2_subtable_first();
        let second = c2_subtable_second();

        let mut gpos = GPos::default();
        let one = parse_pair_adjustment(&first, &mut gpos).expect("first subtable");
        let two = parse_pair_adjustment(&second, &mut gpos).expect("second subtable");

        assert_eq!(one.lookup(20, 21).map(|(a, _)| a.x_advance), Some(-40));
        // Classes (0, 1) here: with the first subtable's ClassDef1 merged in this was (1, 1).
        assert_eq!(two.lookup(20, 21).map(|(a, _)| a.x_advance), Some(-3));
        // Glyph 30 is covered and in class 1; the cell is 0, which still matches: the first
        // subtable that matches a pair ends the lookup, zero or not (audit R3).
        assert_eq!(two.lookup(30, 21).map(|(a, _)| a.x_advance), Some(0));
    }

    #[test]
    fn class_zero_kerns_and_uncovered_glyphs_do_not() {
        // A glyph absent from both class definitions is class 0, which is a perfectly good
        // class; a glyph outside the subtable's coverage never matches.
        let words: [u16; 24] = [
            2, 40,                // format 2, coverageOffset
            4, 0,                 // valueFormat1 = X_ADVANCE, valueFormat2
            24, 32,               // classDef1Offset, classDef2Offset
            2, 2,                 // class1Count, class2Count
            (-9i16) as u16, 0, 0, 0, // matrix: [0][0] = -9, the rest 0
            1, 20, 1, 1,          // classDef1: glyph 20 -> class 1
            1, 21, 1, 1,          // classDef2: glyph 21 -> class 1
            1, 2, 20, 30,         // coverage: glyphs 20 and 30
        ];
        let mut gpos = GPos::default();
        let pairs = parse_pair_adjustment(&words_to_bytes(&words), &mut gpos).expect("parses");

        // Glyph 30 is covered but in neither class definition: classes (0, 0), cell -9.
        assert_eq!(pairs.lookup(30, 31).map(|(a, _)| a.x_advance), Some(-9));
        // Glyph 40 is not covered by the subtable, so nothing applies to it.
        assert!(pairs.lookup(40, 31).is_none());

        // And a subtable that leaves both class definitions out: every glyph is class 0.
        let no_class_defs: [u16; 12] = [
            2, 18,                // format 2, coverageOffset
            4, 0,                 // valueFormat1 = X_ADVANCE, valueFormat2
            0, 0,                 // classDef1Offset, classDef2Offset: both absent
            1, 1,                 // class1Count, class2Count
            (-5i16) as u16,       // matrix: [0][0] = -5
            1, 1, 40,             // coverage: glyph 40
        ];
        let mut gpos = GPos::default();
        let pairs = parse_pair_adjustment(&words_to_bytes(&no_class_defs), &mut gpos).expect("parses");
        assert_eq!(pairs.lookup(40, 41).map(|(a, _)| a.x_advance), Some(-5));
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
        assert_eq!(
            gpos.get_cursive(GlyphId(40)),
            Some(CursiveAnchors { entry: Some((5, 6)), exit: Some((7, 8)) })
        );
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
    fn kerning_uses_only_the_kern_feature_and_ignores_the_merged_flat_maps() {
        // Audit R2: a pair that only a non-`kern` lookup (here lookup 1, feature `dist`)
        // adjusts is not kerning, and the flat `KernTable` (which merges the classes of
        // unrelated subtables) is not a fallback for a real GPOS table.
        let words: [u16; 14] = [
            1, 22,
            4, 0,
            1, 12,
            1, 21,
            (-40i16) as u16, 0,
            0,
            1, 1, 20,
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        gpos.lookups = vec![Vec::new(), Vec::new()];
        gpos.flags = vec![LookupFlags::default(); 2];
        // Put the same pair subtable in lookup 1 only.
        let pair = parse_pair_adjustment(&data, &mut gpos).expect("pair adjustment parses");
        gpos.lookups[1].push(PosSubtable::Pair(pair));
        gpos.scripts = vec![Script {
            tag: Tag(*b"latn"),
            default_language: Some(crate::opentype::gsub::LanguageSystem::with_features(vec![0, 1])),
            languages: Vec::new(),
        }];
        gpos.features = vec![Feature::new(b"kern", vec![0]), Feature::new(b"dist", vec![1])];
        // The flat map was filled by the parse, but it must not be consulted.
        assert_eq!(gpos.kern.get(20, 21), Some(-40));
        assert_eq!(gpos.kerning(GlyphId(20), GlyphId(21)), None);
        // Once the `kern` feature names the lookup, the pair kerns.
        gpos.features[0] = Feature::new(b"kern", vec![0, 1]);
        assert_eq!(gpos.kerning(GlyphId(20), GlyphId(21)), Some(-40));
    }

    #[test]
    fn cursive_attachment_accumulates_along_the_chain() {
        // Audit R5: each glyph's entry anchor meets the previous glyph's exit anchor, and the
        // previous glyph has itself been raised, so a chain climbs a step per glyph.
        let mut anchors = HashMap::new();
        for glyph in 1u16 ..= 3 {
            anchors.insert(glyph, CursiveAnchors { entry: Some((0, 0)), exit: Some((0, 10)) });
        }
        let mut gpos = GPos::default();
        gpos.lookups = vec![vec![PosSubtable::Cursive(anchors)]];
        gpos.flags = vec![LookupFlags::default()];
        let gpos = gpos_with(gpos, vec![0]);
        let positions = gpos.position(&[GlyphId(1), GlyphId(2), GlyphId(3)], &kern_enabled);
        assert_eq!(positions[0].y_offset, 0);
        assert_eq!(positions[1].y_offset, 10);
        assert_eq!(positions[2].y_offset, 20);
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
        gpos.flags.push(LookupFlags::default());
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
        gpos.flags = vec![LookupFlags { flag: 0x0008, mark_filtering_set: None }];
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

    #[test]
    fn a_mark_attaches_to_the_nearest_base_only() {
        // Audit C3: base 200 carries the class-0 anchor (30,40), mark 100 its own at (10,10),
        // so the mark moves by (20,30) when it follows 200. After the unclassed glyph 201 it
        // must not fall back over that glyph to 200: the nearest preceding glyph is where a
        // mark looks, not every earlier one that happens to have an anchor for it.
        let words: [u16; 23] = [
            1, 34, 40, 1, 12, 24, // format, markCov, baseCov, markClassCount, markArray, baseArray
            1, 0, 6,              // MarkArray: one mark, class 0, anchor at +6
            1, 10, 10,            // mark anchor (10,10)
            1, 4,                 // BaseArray: one base, anchor at +4
            1, 30, 40,            // base anchor (30,40)
            1, 1, 100,            // mark coverage
            1, 1, 200,            // base coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 4, &mut gpos, 100).expect("mark-to-base parses");
        let gpos = gpos_with(gpos, vec![0]);

        let positions = gpos.position(&[GlyphId(200), GlyphId(100)], &kern_enabled);
        assert_eq!(positions[1].x_offset, 20);
        assert_eq!(positions[1].y_offset, 30);

        let positions = gpos.position(&[GlyphId(200), GlyphId(100), GlyphId(201), GlyphId(100)], &kern_enabled);
        assert_eq!(positions[1].x_offset, 20);
        // The second mark's base is 201, which has no anchor for it.
        assert_eq!(positions[3], GlyphPosition::default());
    }

    #[test]
    fn a_mark_attaches_to_the_nearest_mark_not_a_base() {
        // Audit C3: mark-to-mark (6) looks for the nearest preceding mark, skipping bases.
        // Mark 202 sits between the anchored mark 201 and mark 100, so 100 follows 202 — which
        // has no anchor for it — and must not jump back to 201.
        let words: [u16; 23] = [
            1, 34, 40, 1, 12, 24, // format, mark1Cov, mark2Cov, markClassCount, mark1Array, mark2Array
            1, 0, 6,              // MarkArray: one mark, class 0, anchor at +6
            1, 10, 10,            // mark anchor (10,10)
            1, 4,                 // Mark2Array: one mark, anchor at +4
            1, 30, 40,            // mark2 anchor (30,40)
            1, 1, 100,            // mark1 coverage
            1, 1, 201,            // mark2 coverage
        ];
        let gdef_words: [u16; 14] = [
            1, 0, 12, 0, 0, 0,    // GDEF 1.0, glyphClassDefOffset 12
            2, 2, 100, 100, 3,    // classDef format 2: 100 -> class 3 (mark)
            201, 202, 3,          // 201..202 -> class 3 (mark)
        ];
        let gdef = crate::opentype::gdef::parse_gdef(&words_to_bytes(&gdef_words)).expect("GDEF parses");
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 6, &mut gpos, 100).expect("mark-to-mark parses");
        let gpos = gpos_with(gpos, vec![0]);

        // Directly after 201 the mark moves.
        let positions = gpos.position_with(&[GlyphId(201), GlyphId(100)], &kern_enabled, Some(&gdef));
        assert_eq!(positions[1].x_offset, 20);

        // With mark 202 in between, the nearest mark has no anchor for 100.
        let positions = gpos.position_with(
            &[GlyphId(201), GlyphId(202), GlyphId(100)],
            &kern_enabled,
            Some(&gdef),
        );
        assert_eq!(positions[2], GlyphPosition::default());
    }

    #[test]
    fn cursive_attachment_subtracts_the_advance_in_between() {
        // Audit C3: glyph 40 exits at (100,20), glyph 41 enters at (10,4). The origins of the
        // two glyphs are 500 units apart, so laying the entry anchor onto the exit anchor moves
        // glyph 41 by 100 - 10 - 500 = -410 horizontally and 20 - 4 = 16 vertically.
        let words: [u16; 20] = [
            1, 32, 2,       // format, coverageOffset, entryExitCount
            14, 20, 26, 0,  // entry40, exit40, entry41, (no exit for 41)
            1, 5, 6,        // entry anchor of 40
            1, 100, 20,     // exit anchor of 40
            1, 10, 4,       // entry anchor of 41
            1, 2, 40, 41,   // coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 3, &mut gpos, 100).expect("cursive attachment parses");
        let gpos = gpos_with(gpos, vec![0]);

        let positions = gpos.position_with_advances(
            &[GlyphId(40), GlyphId(41)],
            &kern_enabled,
            None,
            &|_| 500,
        );
        assert_eq!(positions[1].x_offset, -410);
        assert_eq!(positions[1].y_offset, 16);
    }

    #[test]
    fn a_pair_with_a_second_value_record_consumes_both_glyphs() {
        // Audit C8d: with a value record for the second glyph the lookup resumes after it, so
        // the pair (21,20) must not be applied on top of the pair (20,21) it overlaps.
        let words: [u16; 19] = [
            1, 30,          // format 1, coverageOffset
            4, 4,           // valueFormat1 = X_ADVANCE, valueFormat2 = X_ADVANCE
            2,              // pairSetCount
            14, 22,         // pairSetOffsets
            1, 21, (-40i16) as u16, 5,   // pairSet for 20: (21 -> -40, 5)
            1, 20, (-7i16) as u16, 0,    // pairSet for 21: (20 -> -7, 0)
            1, 2, 20, 21,   // coverage
        ];
        let data = words_to_bytes(&words);
        let mut gpos = GPos::default();
        parse_positioning(&data, 2, &mut gpos, 100).expect("pair adjustment parses");
        let gpos = gpos_with(gpos, vec![0]);

        let positions = gpos.position(&[GlyphId(20), GlyphId(21), GlyphId(20)], &kern_enabled);
        assert_eq!(positions[0].x_advance, -40);
        assert_eq!(positions[1].x_advance, 5);
        // The lookup moved past both glyphs, so (21,20) never applies.
        assert_eq!(positions[2], GlyphPosition::default());
    }
}
