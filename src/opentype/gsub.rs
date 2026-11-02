use std::collections::HashMap;
use nom::{
    number::complete::{be_u16, be_u32},
    multi::count,
    bytes::complete::take,
    sequence::tuple,
};
use crate::{R, GlyphId, FontError};
use crate::parsers::{iterator_n};
use crate::opentype::{parse_extension, parse_lookup_list, coverage_table, parse_class_def, tag, Tag};

#[derive(Debug, Clone)]
pub struct GlyphList(Vec<u16>);
impl GlyphList {
    pub fn matches(&self, mut glyphs: impl Iterator<Item=GlyphId>) -> Option<usize> {
        for &a in &self.0 {
            match glyphs.next() {
                Some(b) if a as u32 == b.0 => continue,
                _ => return None
            }
        }
        Some(self.0.len())
    }
}

#[derive(Debug, Clone)]
pub struct GSub {
    scripts: Vec<Script>,
    features: Vec<Feature>,
    lookup: Vec<Vec<LookupSubtable>>,
}
impl GSub {
    pub fn default_language(&self) -> Option<&LanguageSystem> {
        self.scripts.get(0).and_then(|script| script.default_language.as_ref())
    }
    pub fn language(&self, lang_tag: Tag) -> Option<&LanguageSystem> {
        self.scripts.iter().flat_map(|s| s.languages.iter().filter(|&&(tag, _)| tag == lang_tag).map(|(_, lang)| lang)).next()
    }
    pub fn scripts(&self) -> &[Script] {
        &self.scripts
    }
    /// The substitutions of `lang` whose feature tag `enabled` accepts, in feature and lookup
    /// order. Only `Single` and `Ligatures` are yielded — the kinds a caller can apply itself
    /// with `Substitution::apply`; `Multiple` and `Alternate` are applied by `substitute`.
    pub fn subs<'a, 'b: 'a>(&'b self, lang: &'a LanguageSystem, enabled: impl Fn(Tag) -> bool + 'a) -> impl Iterator<Item=&'b Substitution> + 'a {
        lang.feature_list.iter()
        // Feature and lookup indices come from the file: one past the end of a list is a
        // malformed font, not something to panic on.
        .filter_map(move |&FeatureIdx(idx)| self.features.get(idx as usize))
        .filter(move |feature| enabled(feature.tag))
        .flat_map(move |feature| feature.lookup_indices.iter()
            .filter_map(move |&lookup_idx| self.lookup.get(lookup_idx as usize))
            .flatten())
        .filter_map(|subtable| match subtable {
            LookupSubtable::Substitution(substitution) => Some(substitution),
            // The remaining kinds need the rest of the table to be applied.
            _ => None,
        })
    }
    pub fn lang_features<'a>(&'a self, lang: &'a LanguageSystem) -> impl Iterator<Item=Tag> + 'a {
        lang.feature_list.iter()
        .filter_map(move |&FeatureIdx(idx)| self.features.get(idx as usize).map(|feature| feature.tag))
    }
}

#[derive(Debug, Clone)]
pub enum Substitution {
    Single(HashMap<u16, u16>),
    Ligatures(HashMap<u16, Vec<(GlyphList, u16)>>),
}

impl Substitution {
    /// If this substitution applies at the start of `glyphs`: how many glyphs it consumes
    /// and the glyph that replaces them.
    pub fn apply(&self, glyphs: &[GlyphId]) -> Option<(usize, GlyphId)> {
        let (&first, rest) = glyphs.split_first()?;
        let first = first_glyph_of(first)?;
        match self {
            Substitution::Single(replacements) =>
                replacements.get(&first).map(|&replacement| (1, GlyphId(replacement as u32))),
            Substitution::Ligatures(ligatures) => {
                let ligatures = ligatures.get(&first)?;
                // OT §GSUB "Ligature Substitution": the *longest* matching ligature wins. Fonts
                // usually list them longest first, but that is a recommendation, not a rule.
                let mut best: Option<(usize, u16)> = None;
                for (components, ligature) in ligatures {
                    if let Some(extra) = components.matches(rest.iter().cloned()) {
                        let consumed = extra + 1;
                        if best.map_or(true, |(longest, _)| consumed > longest) {
                            best = Some((consumed, *ligature));
                        }
                    }
                }
                best.map(|(consumed, ligature)| (consumed, GlyphId(ligature as u32)))
            }
        }
    }
}

/// One subtable of a `GSUB` lookup.
///
/// `Substitution` holds the kinds a caller can apply by hand (`Single`, `Ligatures`) and is
/// what `GSub::subs` yields; the other kinds are applied by `GSub::substitute`, which also
/// needed the type left alone for the consumers that match `Substitution` exactly.
#[derive(Debug, Clone)]
pub enum LookupSubtable {
    /// OT §GSUB lookup type 1 or 4 (`Substitution`).
    Substitution(Substitution),
    /// OT §GSUB "Multiple Substitution": one glyph becomes a sequence. An empty sequence is
    /// legal and deletes the glyph.
    Multiple(HashMap<u16, Vec<u16>>),
    /// OT §GSUB "Alternate Substitution": one glyph becomes one of several.
    Alternate(HashMap<u16, Vec<u16>>),
    /// OT §GSUB "Context Substitution" (5) and "Chaining Context Substitution" (6): a rule
    /// that matches a glyph run and names the lookups to apply inside it.
    Context(Context),
    /// OT §GSUB "Reverse Chaining Contextual Single Substitution" (8).
    ReverseChain(ReverseChain),
}

impl LookupSubtable {
    /// If this subtable applies at the start of `glyphs`: how many glyphs it consumes and the
    /// glyphs that replace them. An `Alternate` subtable yields its first alternate;
    /// `apply_with` picks another. `Context` and `ReverseChain` need the whole `GSUB` table to
    /// be applied and always yield `None` here.
    pub fn apply(&self, glyphs: &[GlyphId]) -> Option<(usize, Vec<GlyphId>)> {
        self.apply_with(glyphs, 0)
    }

    /// `apply`, but `alternate` selects which glyph of an `Alternate` subtable is used
    /// (OT §GSUB leaves the choice to the application). An index past the end of the set falls
    /// back to the first alternate; the other kinds ignore it.
    pub fn apply_with(&self, glyphs: &[GlyphId], alternate: usize) -> Option<(usize, Vec<GlyphId>)> {
        match self {
            LookupSubtable::Substitution(substitution) =>
                substitution.apply(glyphs).map(|(consumed, glyph)| (consumed, vec![glyph])),
            LookupSubtable::Multiple(sequences) => {
                let first = first_glyph(glyphs)?;
                sequences.get(&first).map(|sequence| (1, to_glyph_ids(sequence)))
            }
            LookupSubtable::Alternate(alternates) => {
                let first = first_glyph(glyphs)?;
                alternates.get(&first)
                    .and_then(|set| set.get(alternate).or_else(|| set.first()))
                    .map(|&replacement| (1, vec![GlyphId(replacement as u32)]))
            }
            // These name other lookups to run, so they cannot be applied from the subtable
            // alone; `GSub::substitute` runs them.
            LookupSubtable::Context(_) | LookupSubtable::ReverseChain(_) => None,
        }
    }
}

/// What one glyph of a rule's backtrack, input or lookahead sequence has to be.
#[derive(Debug, Clone)]
enum GlyphMatch {
    /// Exactly this glyph id (context and chaining context format 1).
    Glyph(u16),
    /// A glyph of this class (format 2), looked up in the class definition that belongs to the
    /// part of the rule the matcher sits in. Class 0 is every glyph the definition does not
    /// list — OT §"Class Definition Table".
    Class(u16),
    /// A glyph in this coverage table (format 3).
    Coverage(Vec<u16>),
}

impl GlyphMatch {
    fn matches(&self, classes: &HashMap<u16, u16>, glyph: u16) -> bool {
        match self {
            GlyphMatch::Glyph(expected) => *expected == glyph,
            GlyphMatch::Class(class) => class_of(classes, glyph) == *class,
            // OT §"Coverage Tables" want the list sorted; a font that gets it wrong simply
            // does not match here.
            GlyphMatch::Coverage(coverage) => coverage.binary_search(&glyph).is_ok(),
        }
    }
}

fn class_of(classes: &HashMap<u16, u16>, glyph: u16) -> u16 {
    classes.get(&glyph).copied().unwrap_or(0)
}

/// One rule of a context (5) or chaining context (6) subtable. All six formats are normalised
/// to this shape: what the run starts with, what surrounds it, and which lookups to apply.
#[derive(Debug, Clone)]
struct SequenceRule {
    /// The glyph the run starts with (format 1); `None` when the subtable's coverage
    /// (format 3) or `first_class` (format 2) decides.
    first_glyph: Option<u16>,
    /// The class the first glyph has to be in (format 2), through `Context::input_classes`.
    first_class: Option<u16>,
    /// Glyphs before the run, nearest first (the tables store them in that order).
    backtrack: Vec<GlyphMatch>,
    /// Glyphs after the first one; that one is matched separately.
    input: Vec<GlyphMatch>,
    /// Glyphs after the run.
    lookahead: Vec<GlyphMatch>,
    /// `(sequenceIndex, lookupListIndex)`: which lookup to apply at which glyph of the run, in
    /// table order.
    records: Vec<(u16, u16)>,
}

/// OT §GSUB "Context Substitution" (lookup type 5) and "Chaining Context Substitution"
/// (lookup type 6), every format, flattened into rules.
#[derive(Debug, Clone)]
pub struct Context {
    /// Glyphs a run may start with; format 3 keeps its first input coverage here.
    coverage: Vec<u16>,
    /// Class definitions, one per role: single context format 2 has only the input one,
    /// chaining context format 2 has all three. Formats 1 and 3 leave them empty.
    input_classes: HashMap<u16, u16>,
    backtrack_classes: HashMap<u16, u16>,
    lookahead_classes: HashMap<u16, u16>,
    rules: Vec<SequenceRule>,
}

fn empty_context() -> Context {
    Context {
        coverage: Vec::new(),
        input_classes: HashMap::new(),
        backtrack_classes: HashMap::new(),
        lookahead_classes: HashMap::new(),
        rules: Vec::new(),
    }
}

impl Context {
    /// Whether `rule` matches the glyph run starting at `position`, with its backtrack and
    /// lookahead around it.
    fn matches(&self, rule: &SequenceRule, glyphs: &[GlyphId], position: usize) -> bool {
        let first = match glyphs.get(position).and_then(|&glyph| first_glyph_of(glyph)) {
            Some(first) => first,
            None => return false,
        };
        if self.coverage.binary_search(&first).is_err() {
            return false;
        }
        if let Some(expected) = rule.first_glyph {
            if expected != first {
                return false;
            }
        }
        if let Some(class) = rule.first_class {
            if class_of(&self.input_classes, first) != class {
                return false;
            }
        }
        // Input sequence: the glyphs after the first one.
        for (n, matcher) in rule.input.iter().enumerate() {
            match glyphs.get(position + 1 + n).and_then(|&glyph| first_glyph_of(glyph)) {
                Some(glyph) if matcher.matches(&self.input_classes, glyph) => {}
                _ => return false,
            }
        }
        // Backtrack: the glyph immediately before the run first.
        for (n, matcher) in rule.backtrack.iter().enumerate() {
            match position.checked_sub(n + 1)
                .and_then(|before| glyphs.get(before))
                .and_then(|&glyph| first_glyph_of(glyph))
            {
                Some(glyph) if matcher.matches(&self.backtrack_classes, glyph) => {}
                _ => return false,
            }
        }
        // Lookahead: right after the run.
        let after = position + 1 + rule.input.len();
        for (n, matcher) in rule.lookahead.iter().enumerate() {
            match glyphs.get(after + n).and_then(|&glyph| first_glyph_of(glyph)) {
                Some(glyph) if matcher.matches(&self.lookahead_classes, glyph) => {}
                _ => return false,
            }
        }
        true
    }
}

/// OT §GSUB "Reverse Chaining Contextual Single Substitution" (lookup type 8), format 1.
#[derive(Debug, Clone)]
pub struct ReverseChain {
    /// Covered glyph -> the glyph that replaces it.
    substitutes: HashMap<u16, u16>,
    /// One coverage per glyph before the substituted one, nearest first.
    backtrack: Vec<Vec<u16>>,
    /// One coverage per glyph after it.
    lookahead: Vec<Vec<u16>>,
}

impl ReverseChain {
    /// Substitutes in `glyphs` right to left: OT §GSUB applies this lookup type in reverse
    /// order over the whole run, so that a glyph it replaced is not looked at again.
    pub fn apply(&self, glyphs: &mut [GlyphId]) {
        for position in (0 .. glyphs.len()).rev() {
            let glyph = match first_glyph_of(glyphs[position]) {
                Some(glyph) => glyph,
                None => continue,
            };
            let replacement = match self.substitutes.get(&glyph) {
                Some(&replacement) => replacement,
                None => continue,
            };
            if !matches_coverage_before(&self.backtrack, glyphs, position) {
                continue;
            }
            if !matches_coverage_after(&self.lookahead, glyphs, position + 1) {
                continue;
            }
            glyphs[position] = GlyphId(replacement as u32);
        }
    }
}

fn matches_coverage_before(coverages: &[Vec<u16>], glyphs: &[GlyphId], position: usize) -> bool {
    coverages.iter().enumerate().all(|(n, coverage)| {
        position
            .checked_sub(n + 1)
            .and_then(|before| glyphs.get(before))
            .and_then(|&glyph| first_glyph_of(glyph))
            .map_or(false, |glyph| coverage.binary_search(&glyph).is_ok())
    })
}

fn matches_coverage_after(coverages: &[Vec<u16>], glyphs: &[GlyphId], start: usize) -> bool {
    coverages.iter().enumerate().all(|(n, coverage)| {
        glyphs
            .get(start + n)
            .and_then(|&glyph| first_glyph_of(glyph))
            .map_or(false, |glyph| coverage.binary_search(&glyph).is_ok())
    })
}

/// Substitution tables are keyed by u16 glyph ids; a larger id cannot be in one, and `as u16`
/// would silently look up an unrelated glyph.
fn first_glyph_of(glyph: GlyphId) -> Option<u16> {
    if glyph.0 > u16::MAX as u32 { None } else { Some(glyph.0 as u16) }
}

fn first_glyph(glyphs: &[GlyphId]) -> Option<u16> {
    first_glyph_of(*glyphs.first()?)
}

fn to_glyph_ids(ids: &[u16]) -> Vec<GlyphId> {
    ids.iter().map(|&g| GlyphId(g as u32)).collect()
}

/// Context lookups may name other context lookups. OT has no depth limit, but a font that
/// points a lookup at itself would recurse until the stack ran out; no real font nests more
/// than a handful deep.
const MAX_NESTING: u8 = 8;

impl GSub {
    /// Applies the lookups of the default language system whose feature tag `enabled`
    /// accepts, once each, left to right — the simple subset of OT §"Lookups" this crate
    /// implements. Single (1), Multiple (2), Alternate (3, first alternate), Ligature (4),
    /// Context (5) and Chaining Context (6) lookups are applied, the last two by running the
    /// lookups their rules name; Reverse Chaining (8) runs in its own right-to-left pass at
    /// the end; unrecognised glyphs pass through.
    pub fn substitute(&self, glyphs: &[GlyphId], enabled: &dyn Fn(Tag) -> bool) -> Vec<GlyphId> {
        let language = match self.default_language() {
            Some(language) => language,
            None => return glyphs.to_vec(),
        };

        // OT §"Lookups": lookups are applied in lookup-list order.
        let mut lookup_indices: Vec<u16> = language.feature_list.iter()
            .filter_map(|&FeatureIdx(idx)| self.features.get(idx as usize))
            .filter(|feature| enabled(feature.tag))
            .flat_map(|feature| feature.lookup_indices.iter().copied())
            .collect();
        lookup_indices.sort_unstable();
        lookup_indices.dedup();

        let mut out: Vec<GlyphId> = glyphs.to_vec();
        let mut position = 0;
        while position < out.len() {
            let mut applied = false;
            for &lookup_idx in &lookup_indices {
                if let Some(grown) = self.apply_lookup_at(lookup_idx, &mut out, position, 0) {
                    // OT §"Lookups": what a lookup produces is not fed back into it. A lookup
                    // that deletes the glyphs at `position` still has to move on, or the same
                    // rule would delete them again forever.
                    position += if grown > 0 { grown as usize } else { 1 };
                    applied = true;
                    break;
                }
            }
            if !applied {
                position += 1;
            }
        }

        // OT §GSUB "Reverse Chaining Contextual Single Substitution": that lookup type is
        // applied right to left over the whole run. Fonts use it on its own, so running these
        // passes after all forward lookups stays within the spec for them.
        for &lookup_idx in &lookup_indices {
            let subtables = match self.lookup.get(lookup_idx as usize) {
                Some(subtables) => subtables,
                None => continue,
            };
            for subtable in subtables {
                if let LookupSubtable::ReverseChain(reverse) = subtable {
                    reverse.apply(&mut out);
                }
            }
        }
        out
    }

    /// Applies one lookup at `position`. `Some(grown)` is how much the glyph run at and after
    /// `position` grew by (zero, or negative when the rule deleted glyphs); `None` if no
    /// subtable of the lookup applies there.
    fn apply_lookup_at(&self, lookup_idx: u16, glyphs: &mut Vec<GlyphId>, position: usize, depth: u8) -> Option<isize> {
        let subtables = self.lookup.get(lookup_idx as usize)?;
        for subtable in subtables {
            let applied = match subtable {
                // The rule names other lookups to run, so these need the whole table.
                LookupSubtable::Context(context) => self.apply_context(context, glyphs, position, depth),
                // Applied in the right-to-left pass of `substitute`.
                LookupSubtable::ReverseChain(_) => None,
                substitution => {
                    let step = substitution.apply(&glyphs[position ..]);
                    match step {
                        Some((consumed, replacement)) => {
                            let grown = replacement.len() as isize - consumed as isize;
                            glyphs.splice(position .. position + consumed, replacement);
                            Some(grown)
                        }
                        None => None,
                    }
                }
            };
            if applied.is_some() {
                return applied;
            }
        }
        None
    }

    /// Applies the first rule of `context` that matches at `position`, which means running the
    /// lookups its records name at the glyphs of the run.
    fn apply_context(&self, context: &Context, glyphs: &mut Vec<GlyphId>, position: usize, depth: u8) -> Option<isize> {
        if depth >= MAX_NESTING {
            debug!("GSUB: not following context lookups nested deeper than {}", MAX_NESTING);
            return None;
        }
        let rule = match context.rules.iter().find(|rule| context.matches(rule, glyphs, position)) {
            Some(rule) => rule,
            None => return None,
        };
        let before = glyphs.len();
        // A record that inserted or deleted glyphs shifts the glyphs after it, so the next
        // record's sequenceIndex has to move by the same amount.
        let mut shift = 0isize;
        for &(sequence_index, lookup_index) in &rule.records {
            let target = position as isize + sequence_index as isize + shift;
            if target < 0 || target as usize >= glyphs.len() {
                // OT §"Sequence Lookup Records": the index has to be inside the run; a font
                // that points outside it has nothing to apply to.
                continue;
            }
            if let Some(grown) = self.apply_lookup_at(lookup_index, glyphs, target as usize, depth + 1) {
                shift += grown;
            }
        }
        Some(glyphs.len() as isize - before as isize)
    }
}

pub fn parse_gsub(data: &[u8]) -> Result<GSub, FontError> {
    debug!("parse GSUB");
    let (i, major_version) = be_u16(data)?;
    require_eq!(major_version, 1);
    let (i, minor_version) = be_u16(i)?;
    
    let (i, script_list_off) = be_u16(i)?;
    let (i, feature_list_off) = be_u16(i)?;
    let (i, lookup_list_off) = be_u16(i)?;
    
    let script_list = parse_script_list(slice!(data, script_list_off as usize ..))?;
    let feature_list = parse_feature_list(slice!(data, feature_list_off as usize ..))?;
/*
    let print_lang = |lang: &LanguageSystem| {
        if let Some(required) = lang.required_feature {
            println!("    required {:?}", feature_list[required.0 as usize].tag);
        }
        for feature in &lang.feature_list {
            println!("    other {:?}", feature_list[feature.0 as usize].tag);
        }
    };
    for (script_nr, script) in script_list.iter().enumerate() {
        println!("script {}:", script_nr);
        if let Some(ref default) = script.default_language {
            println!("default language:");
            print_lang(default);
        }
        for (tag, lang) in &script.languages {
            println!("  {:?}", tag);
            print_lang(lang);
        }
    }
*/
    let (i, _feature_variations_offset) = match minor_version {
        0 => (i, 0),
        1 => be_u32(i)?,
        // See parse_gpos: unknown minor versions are skipped, not fatal.
        v => {
            warn!("unsupported GSUB version 1.{}, ignoring the rest of the header", v);
            (i, 0)
        }
    };
    
    let mut lookup = Vec::new();
    
    parse_lookup_list(slice!(data, lookup_list_off as usize ..), |lookup_idx, data, lookup_type, _lookup_flag| {
        while lookup_idx >= lookup.len() {
            lookup.push(Vec::new());
        }
        let mut push = |sub| lookup[lookup_idx].push(sub);

        if let Some(sub) = parse_substitution(data, lookup_type)? {
            push(sub);
        }
        Ok(())
    })?;
    
    Ok(GSub {
        lookup,
        scripts: script_list,
        features: feature_list,
    })
}

/// One subtable of a `GSUB` lookup, as the substitutions this crate can apply. Only lookup
/// types 1-8 are known; anything else yields `None`.
fn parse_substitution(data: &[u8], lookup_type: u16) -> Result<Option<LookupSubtable>, FontError> {
    Ok(Some(match lookup_type {
        // Single · Replace one glyph with one glyph
        1 => LookupSubtable::Substitution(parse_single_subst(data)?),
        // Ligature · Replace multiple glyphs with one glyph
        4 => LookupSubtable::Substitution(parse_ligatures(data)?),
        // Extension · wraps another lookup type (what every large font actually uses)
        7 => {
            let (inner_type, inner) = parse_extension(data)?;
            if inner_type == 7 {
                // OT §GSUB "Extension Substitution": the extension types are for wrapping
                // other lookups, not each other.
                warn!("GSUB: an Extension lookup may not point at another Extension lookup");
                return Ok(None);
            }
            match parse_substitution(inner, inner_type)? {
                Some(sub) => sub,
                None => return Ok(None),
            }
        }
        // Multiple · Replace one glyph with more than one glyph
        2 => parse_multiple_subst(data)?,
        // Alternate · Replace one glyph with one of many glyphs
        3 => parse_alternate_subst(data)?,
        // Context · Replace one or more glyphs in context
        5 => LookupSubtable::Context(parse_context_subst(data)?),
        // Chaining Context · Replace one or more glyphs in chained context
        6 => LookupSubtable::Context(parse_chain_context_subst(data)?),
        // Reverse chaining context single · applied in reverse order
        8 => LookupSubtable::ReverseChain(parse_reverse_chain(data)?),
        _ => return Ok(None),
    }))
}

fn parse_single_subst(data: &[u8]) -> Result<Substitution, FontError> {
    let (i, format) = be_u16(data)?;
    let mut subs = HashMap::new();
    match format {
        1 => {
            let (i, coverage_offset) = be_u16(i)?;
            let (i, delta_gid) = be_u16(i)?;
            let coverage = coverage_table(slice!(data, coverage_offset as usize ..))?;
            for gid in coverage {
                subs.insert(gid, gid.wrapping_add(delta_gid));
            }
        },
        2 => {
            let (i, coverage_offset) = be_u16(i)?;
            let (i, glyph_count) = be_u16(i)?;
            let coverage = coverage_table(slice!(data, coverage_offset as usize ..))?;
            let replacements = iterator_n(i, be_u16, glyph_count);
            for (gid, replacement_gid) in coverage.zip(replacements) {
                subs.insert(gid, replacement_gid);
            }
        }
        _ => error!("unsupported single substitution format {}", format)
    }
    Ok(Substitution::Single(subs))
}


/// OT §GSUB "Multiple Substitution" (lookup type 2), format 1: one glyph becomes a sequence.
/// `glyphCount == 0` is legal and deletes the glyph.
fn parse_multiple_subst(data: &[u8]) -> Result<LookupSubtable, FontError> {
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);
    let (i, coverage_offset) = be_u16(i)?;
    let (i, sequence_count) = be_u16(i)?;
    let coverage = coverage_table(slice!(data, coverage_offset as usize ..))?;
    let mut sequences = HashMap::with_capacity(sequence_count.min(1024) as usize);
    for (gid, offset) in coverage.zip(iterator_n(i, be_u16, sequence_count)) {
        let sequence_data = slice!(data, offset as usize ..);
        let (i, glyph_count) = be_u16(sequence_data)?;
        let sequence: Vec<u16> = iterator_n(i, be_u16, glyph_count).collect();
        sequences.insert(gid, sequence);
    }
    Ok(LookupSubtable::Multiple(sequences))
}

/// OT §GSUB "Alternate Substitution" (lookup type 3), format 1: one glyph becomes one of a set.
fn parse_alternate_subst(data: &[u8]) -> Result<LookupSubtable, FontError> {
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);
    let (i, coverage_offset) = be_u16(i)?;
    let (i, alternate_set_count) = be_u16(i)?;
    let coverage = coverage_table(slice!(data, coverage_offset as usize ..))?;
    let mut alternates = HashMap::with_capacity(alternate_set_count.min(1024) as usize);
    for (gid, offset) in coverage.zip(iterator_n(i, be_u16, alternate_set_count)) {
        let set_data = slice!(data, offset as usize ..);
        let (i, glyph_count) = be_u16(set_data)?;
        let set: Vec<u16> = iterator_n(i, be_u16, glyph_count).collect();
        // An AlternateSet with no alternates would have nothing to choose from; it cannot
        // match and is dropped rather than stored as an empty list.
        if !set.is_empty() {
            alternates.insert(gid, set);
        }
    }
    Ok(LookupSubtable::Alternate(alternates))
}

/// `count` numbers, or an error if the table is too short. The callers below treat a short
/// table as "this rule is broken, skip it".
fn numbers<'a>(i: &'a [u8], n: u16) -> Result<(&'a [u8], Vec<u16>), FontError> {
    Ok(count(be_u16, usize::from(n))(i)?)
}

/// `count` `(sequenceIndex, lookupListIndex)` records.
fn lookup_records<'a>(i: &'a [u8], n: u16) -> Result<(&'a [u8], Vec<(u16, u16)>), FontError> {
    Ok(count(tuple((be_u16, be_u16)), usize::from(n))(i)?)
}

/// The coverage tables an array of offsets (relative to the subtable start) points at.
fn coverage_lists(data: &[u8], offsets: Vec<u16>) -> Result<Vec<Vec<u16>>, FontError> {
    offsets
        .into_iter()
        .map(|offset| Ok(coverage_table(slice!(data, offset as usize ..))?.collect()))
        .collect()
}

/// OT §GSUB "Context Substitution" (lookup type 5), formats 1, 2 and 3.
fn parse_context_subst(data: &[u8]) -> Result<Context, FontError> {
    let (i, format) = be_u16(data)?;
    match format {
        1 => parse_context_format1(data, i),
        2 => parse_context_format2(data, i),
        3 => parse_context_format3(data, i),
        n => error!("unsupported context substitution format {}", n),
    }
}

/// OT §GSUB "ContextSubstFormat1": rule sets indexed by the position of the first glyph in the
/// coverage table, the rest of the run given as glyph ids.
fn parse_context_format1(data: &[u8], i: &[u8]) -> Result<Context, FontError> {
    let (i, coverage_offset) = be_u16(i)?;
    let (i, rule_set_count) = be_u16(i)?;
    let coverage: Vec<u16> = coverage_table(slice!(data, coverage_offset as usize ..))?.collect();
    let mut rules = Vec::new();
    for (first, set_offset) in coverage.iter().zip(iterator_n(i, be_u16, rule_set_count)) {
        if set_offset == 0 {
            // OT §: a NULL offset means there are no rules for this glyph.
            continue;
        }
        let set = slice!(data, set_offset as usize ..);
        let (i, rule_count) = be_u16(set)?;
        for rule_offset in iterator_n(i, be_u16, rule_count) {
            let rule = slice!(set, rule_offset as usize ..);
            match parse_simple_rule(rule, *first) {
                Ok(Some(rule)) => rules.push(rule),
                Ok(None) => {}
                Err(_) => warn!("GSUB: truncated context rule, skipping it"),
            }
        }
    }
    let mut context = empty_context();
    context.coverage = coverage;
    context.rules = rules;
    Ok(context)
}

/// OT §GSUB "ContextSubstFormat2": rule sets indexed by the class of the first glyph, the rest
/// of the run given as class numbers.
fn parse_context_format2(data: &[u8], i: &[u8]) -> Result<Context, FontError> {
    let (i, coverage_offset) = be_u16(i)?;
    let (i, class_def_offset) = be_u16(i)?;
    let (i, rule_set_count) = be_u16(i)?;
    let coverage: Vec<u16> = coverage_table(slice!(data, coverage_offset as usize ..))?.collect();
    let mut input_classes = HashMap::new();
    if class_def_offset != 0 {
        parse_class_def(slice!(data, class_def_offset as usize ..), &mut input_classes)?;
    }
    let mut rules = Vec::new();
    for (class, set_offset) in (0u16 ..).zip(iterator_n(i, be_u16, rule_set_count)) {
        if set_offset == 0 {
            continue;
        }
        let set = slice!(data, set_offset as usize ..);
        let (i, rule_count) = be_u16(set)?;
        for rule_offset in iterator_n(i, be_u16, rule_count) {
            let rule = slice!(set, rule_offset as usize ..);
            match parse_class_rule(rule, class) {
                Ok(Some(rule)) => rules.push(rule),
                Ok(None) => {}
                Err(_) => warn!("GSUB: truncated context rule, skipping it"),
            }
        }
    }
    let mut context = empty_context();
    context.coverage = coverage;
    context.input_classes = input_classes;
    context.rules = rules;
    Ok(context)
}

/// OT §GSUB "ContextSubstFormat3": one rule, every glyph of the run with its own coverage
/// table.
fn parse_context_format3(data: &[u8], i: &[u8]) -> Result<Context, FontError> {
    let (i, glyph_count) = be_u16(i)?;
    let (i, subst_count) = be_u16(i)?;
    let (i, coverage_offsets) = numbers(i, glyph_count)?;
    let (_, records) = lookup_records(i, subst_count)?;
    if glyph_count == 0 {
        warn!("GSUB: context format 3 has glyphCount 0, it cannot match anything");
        return Ok(empty_context());
    }
    let mut coverage = coverage_lists(data, coverage_offsets)?;
    // The first input glyph has a coverage table of its own; that one decides where the rule
    // may start.
    let first = coverage.remove(0);
    let mut context = empty_context();
    context.coverage = first;
    context.rules.push(SequenceRule {
        first_glyph: None,
        first_class: None,
        backtrack: Vec::new(),
        input: coverage.into_iter().map(GlyphMatch::Coverage).collect(),
        lookahead: Vec::new(),
        records,
    });
    Ok(context)
}

/// A context rule of format 1: `glyphCount`, `substCount`, the glyphs after the first one and
/// the lookup records. `None` if the rule is malformed.
fn parse_simple_rule(rule: &[u8], first: u16) -> Result<Option<SequenceRule>, FontError> {
    let (i, glyph_count) = be_u16(rule)?;
    let (i, subst_count) = be_u16(i)?;
    let following = match glyph_count.checked_sub(1) {
        Some(following) => following,
        None => {
            warn!("GSUB: context rule for glyph {} has glyphCount 0, skipping it", first);
            return Ok(None);
        }
    };
    let (i, input) = match numbers(i, following) {
        Ok(parsed) => parsed,
        Err(_) => {
            warn!("GSUB: context rule for glyph {} is truncated, skipping it", first);
            return Ok(None);
        }
    };
    let records = match lookup_records(i, subst_count) {
        Ok((_, records)) => records,
        Err(_) => return Ok(None),
    };
    Ok(Some(SequenceRule {
        first_glyph: Some(first),
        first_class: None,
        backtrack: Vec::new(),
        input: input.into_iter().map(GlyphMatch::Glyph).collect(),
        lookahead: Vec::new(),
        records,
    }))
}

/// A context rule of format 2: the same shape, with class numbers instead of glyph ids.
fn parse_class_rule(rule: &[u8], class: u16) -> Result<Option<SequenceRule>, FontError> {
    let (i, glyph_count) = be_u16(rule)?;
    let (i, subst_count) = be_u16(i)?;
    let following = match glyph_count.checked_sub(1) {
        Some(following) => following,
        None => {
            warn!("GSUB: context rule of class {} has glyphCount 0, skipping it", class);
            return Ok(None);
        }
    };
    let (i, input) = match numbers(i, following) {
        Ok(parsed) => parsed,
        Err(_) => {
            warn!("GSUB: context rule of class {} is truncated, skipping it", class);
            return Ok(None);
        }
    };
    let records = match lookup_records(i, subst_count) {
        Ok((_, records)) => records,
        Err(_) => return Ok(None),
    };
    Ok(Some(SequenceRule {
        first_glyph: None,
        first_class: Some(class),
        backtrack: Vec::new(),
        input: input.into_iter().map(GlyphMatch::Class).collect(),
        lookahead: Vec::new(),
        records,
    }))
}

/// The part chaining context formats 1, 2 and 3 share: the glyph counts with their sequences
/// (raw numbers — the caller knows whether they are glyph ids, classes or coverage offsets)
/// and the lookup records.
struct ChainSequence {
    backtrack: Vec<u16>,
    input: Vec<u16>,
    lookahead: Vec<u16>,
    records: Vec<(u16, u16)>,
}

fn parse_chain_sequence(i: &[u8]) -> Result<Option<ChainSequence>, FontError> {
    let (i, backtrack_count) = be_u16(i)?;
    let (i, backtrack) = numbers(i, backtrack_count)?;
    let (i, input_count) = be_u16(i)?;
    let input_count = match input_count.checked_sub(1) {
        Some(count) => count,
        None => {
            warn!("GSUB: chaining context rule has inputGlyphCount 0, skipping it");
            return Ok(None);
        }
    };
    let (i, input) = numbers(i, input_count)?;
    let (i, lookahead_count) = be_u16(i)?;
    let (i, lookahead) = numbers(i, lookahead_count)?;
    let (i, subst_count) = be_u16(i)?;
    let (_, records) = lookup_records(i, subst_count)?;
    Ok(Some(ChainSequence { backtrack, input, lookahead, records }))
}

/// OT §GSUB "Chaining Context Substitution" (lookup type 6), formats 1, 2 and 3.
fn parse_chain_context_subst(data: &[u8]) -> Result<Context, FontError> {
    let (i, format) = be_u16(data)?;
    match format {
        1 => parse_chain_format1(data, i),
        2 => parse_chain_format2(data, i),
        3 => parse_chain_format3(data, i),
        n => error!("unsupported chaining context substitution format {}", n),
    }
}

/// OT §GSUB "ChainContextSubstFormat1": rule sets indexed by the position of the first glyph in
/// the coverage table, everything else given as glyph ids.
fn parse_chain_format1(data: &[u8], i: &[u8]) -> Result<Context, FontError> {
    let (i, coverage_offset) = be_u16(i)?;
    let (i, rule_set_count) = be_u16(i)?;
    let coverage: Vec<u16> = coverage_table(slice!(data, coverage_offset as usize ..))?.collect();
    let mut rules = Vec::new();
    for (first, set_offset) in coverage.iter().zip(iterator_n(i, be_u16, rule_set_count)) {
        if set_offset == 0 {
            continue;
        }
        let set = slice!(data, set_offset as usize ..);
        let (i, rule_count) = be_u16(set)?;
        for rule_offset in iterator_n(i, be_u16, rule_count) {
            let rule = slice!(set, rule_offset as usize ..);
            let sequence = match parse_chain_sequence(rule) {
                Ok(Some(sequence)) => sequence,
                Ok(None) => continue,
                Err(_) => {
                    warn!("GSUB: truncated chaining context rule, skipping it");
                    continue;
                }
            };
            rules.push(SequenceRule {
                first_glyph: Some(*first),
                first_class: None,
                backtrack: sequence.backtrack.into_iter().map(GlyphMatch::Glyph).collect(),
                input: sequence.input.into_iter().map(GlyphMatch::Glyph).collect(),
                lookahead: sequence.lookahead.into_iter().map(GlyphMatch::Glyph).collect(),
                records: sequence.records,
            });
        }
    }
    let mut context = empty_context();
    context.coverage = coverage;
    context.rules = rules;
    Ok(context)
}

/// OT §GSUB "ChainContextSubstFormat2": rule sets indexed by the class of the first glyph,
/// with one class definition per role (backtrack, input, lookahead).
fn parse_chain_format2(data: &[u8], i: &[u8]) -> Result<Context, FontError> {
    let (i, coverage_offset) = be_u16(i)?;
    let (i, backtrack_class_offset) = be_u16(i)?;
    let (i, input_class_offset) = be_u16(i)?;
    let (i, lookahead_class_offset) = be_u16(i)?;
    let (i, rule_set_count) = be_u16(i)?;
    let coverage: Vec<u16> = coverage_table(slice!(data, coverage_offset as usize ..))?.collect();
    let mut backtrack_classes = HashMap::new();
    let mut input_classes = HashMap::new();
    let mut lookahead_classes = HashMap::new();
    if backtrack_class_offset != 0 {
        parse_class_def(slice!(data, backtrack_class_offset as usize ..), &mut backtrack_classes)?;
    }
    if input_class_offset != 0 {
        parse_class_def(slice!(data, input_class_offset as usize ..), &mut input_classes)?;
    }
    if lookahead_class_offset != 0 {
        parse_class_def(slice!(data, lookahead_class_offset as usize ..), &mut lookahead_classes)?;
    }
    let mut rules = Vec::new();
    for (class, set_offset) in (0u16 ..).zip(iterator_n(i, be_u16, rule_set_count)) {
        if set_offset == 0 {
            continue;
        }
        let set = slice!(data, set_offset as usize ..);
        let (i, rule_count) = be_u16(set)?;
        for rule_offset in iterator_n(i, be_u16, rule_count) {
            let rule = slice!(set, rule_offset as usize ..);
            let sequence = match parse_chain_sequence(rule) {
                Ok(Some(sequence)) => sequence,
                Ok(None) => continue,
                Err(_) => {
                    warn!("GSUB: truncated chaining context rule, skipping it");
                    continue;
                }
            };
            rules.push(SequenceRule {
                first_glyph: None,
                first_class: Some(class),
                backtrack: sequence.backtrack.into_iter().map(GlyphMatch::Class).collect(),
                input: sequence.input.into_iter().map(GlyphMatch::Class).collect(),
                lookahead: sequence.lookahead.into_iter().map(GlyphMatch::Class).collect(),
                records: sequence.records,
            });
        }
    }
    let mut context = empty_context();
    context.coverage = coverage;
    context.input_classes = input_classes;
    context.backtrack_classes = backtrack_classes;
    context.lookahead_classes = lookahead_classes;
    context.rules = rules;
    Ok(context)
}

/// OT §GSUB "ChainContextSubstFormat3": one rule, every glyph of the run with its own coverage
/// table.
fn parse_chain_format3(data: &[u8], i: &[u8]) -> Result<Context, FontError> {
    let (i, backtrack_count) = be_u16(i)?;
    let (i, backtrack_offsets) = numbers(i, backtrack_count)?;
    let (i, input_count) = be_u16(i)?;
    let (i, input_offsets) = numbers(i, input_count)?;
    let (i, lookahead_count) = be_u16(i)?;
    let (i, lookahead_offsets) = numbers(i, lookahead_count)?;
    let (i, subst_count) = be_u16(i)?;
    let (_, records) = lookup_records(i, subst_count)?;
    if input_count == 0 {
        warn!("GSUB: chaining context format 3 has inputGlyphCount 0, it cannot match anything");
        return Ok(empty_context());
    }
    let backtrack = coverage_lists(data, backtrack_offsets)?;
    let lookahead = coverage_lists(data, lookahead_offsets)?;
    let mut input = coverage_lists(data, input_offsets)?;
    // The first input glyph has a coverage table of its own; that one decides where a rule may
    // start.
    let first = input.remove(0);
    let mut context = empty_context();
    context.coverage = first;
    context.rules.push(SequenceRule {
        first_glyph: None,
        first_class: None,
        backtrack: backtrack.into_iter().map(GlyphMatch::Coverage).collect(),
        input: input.into_iter().map(GlyphMatch::Coverage).collect(),
        lookahead: lookahead.into_iter().map(GlyphMatch::Coverage).collect(),
        records,
    });
    Ok(context)
}

/// OT §GSUB "Reverse Chaining Contextual Single Substitution" (lookup type 8), format 1.
fn parse_reverse_chain(data: &[u8]) -> Result<ReverseChain, FontError> {
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);
    let (i, coverage_offset) = be_u16(i)?;
    let coverage: Vec<u16> = coverage_table(slice!(data, coverage_offset as usize ..))?.collect();
    let (i, backtrack_count) = be_u16(i)?;
    let (i, backtrack_offsets) = numbers(i, backtrack_count)?;
    let (i, lookahead_count) = be_u16(i)?;
    let (i, lookahead_offsets) = numbers(i, lookahead_count)?;
    let (i, glyph_count) = be_u16(i)?;
    let (_, substitutes) = numbers(i, glyph_count)?;
    Ok(ReverseChain {
        // OT §: substituteGlyphIDs runs parallel to the coverage table.
        substitutes: coverage.into_iter().zip(substitutes).collect(),
        backtrack: coverage_lists(data, backtrack_offsets)?,
        lookahead: coverage_lists(data, lookahead_offsets)?,
    })
}

fn parse_ligatures(data: &[u8]) -> Result<Substitution, FontError> {
    let (i, format) = be_u16(data)?;
    require_eq!(format, 1);

    let (i, coverage_offset) = be_u16(i)?;
    let (i, ligature_set_count) = be_u16(i)?;
    let coverage = coverage_table(slice!(data, coverage_offset as usize ..))?;
    let mut ligatures = HashMap::with_capacity(ligature_set_count as usize);

    for (first, offset) in coverage.zip(iterator_n(i, be_u16, ligature_set_count)) {
        let set_data = slice!(data, offset as usize ..);
        let (i, ligature_count) = be_u16(set_data)?;
        let entry = ligatures.entry(first).or_insert_with(|| Vec::with_capacity(ligature_count as usize));

        for set_offest in iterator_n(i, be_u16, ligature_count) {
            let data = slice!(set_data, set_offest as usize ..);
            let (i, ligature_glyph) = be_u16(data)?;
            let (i, component_count) = be_u16(i)?;
            // OT §GSUB "Ligature Substitution": componentCount counts the first glyph too, so
            // a value below 2 is malformed. It used to make `component_count - 1` underflow and
            // abort; skip the ligature instead.
            let following = match component_count.checked_sub(1) {
                Some(n) if n >= 1 => n,
                _ => {
                    warn!("GSUB: ligature for glyph {} has componentCount {}, skipping it",
                          ligature_glyph, component_count);
                    continue;
                }
            };
            let (_, components) = count(be_u16, following as usize)(i)?;

            entry.push((GlyphList(components), ligature_glyph));
        }
    }
    Ok(Substitution::Ligatures(ligatures))
}
/*
enum Action {
    GoTo(u16),
    Glyph(u16)
}

struct StateMachine {
    transitions: HashMap<(u16, u16), Action>, // current state, input gid
    num_states: usize
}
impl StateMachine {
*/

fn parse_script_list(data: &[u8]) -> Result<Vec<Script>, FontError> {
    let (i, script_count) = be_u16(data)?;
    let mut scripts = Vec::with_capacity(script_count as usize);
    for (tag, offset) in iterator_n(i, tuple((take(4usize), be_u16)), script_count) {
        scripts.push(parse_script_table(slice!(data, offset as usize .. ))?);
    }
    Ok(scripts)
}
fn parse_script_table(data: &[u8]) -> Result<Script, FontError> {
    let (i, default_lang_offset) = be_u16(data)?;
    let default_language = if default_lang_offset != 0 {
        Some(parse_language_system_table(slice!(data, default_lang_offset as usize ..))?)
    } else {
        None
    };

    let (i, lang_sys_count) = be_u16(i)?;
    let mut languages = Vec::with_capacity(lang_sys_count as usize);
    for (tag, lang_sys_offset) in iterator_n(i, tuple((tag, be_u16)), lang_sys_count) {
        let table = parse_language_system_table(slice!(data, lang_sys_offset as usize ..))?;
        languages.push((tag, table));
    }
    Ok(Script {
        default_language,
        languages,
    })
}

#[derive(Copy, Clone, Debug)]
struct FeatureIdx(u16);

#[derive(Debug, Clone)]
pub struct Script {
    default_language: Option<LanguageSystem>,
    languages: Vec<(Tag, LanguageSystem)>
}

#[derive(Debug, Clone)]
pub struct LanguageSystem {
    feature_list: Vec<FeatureIdx>,
    required_feature: Option<FeatureIdx>,
}

#[derive(Debug, Clone)]
struct Feature {
    tag: Tag,
    lookup_indices: Vec<u16>
}
// returns (requiredFeatureIndex, FeatureList)
fn parse_language_system_table(i: &[u8]) -> Result<LanguageSystem, FontError> {
    let (i, _lookup_order) = be_u16(i)?;
    let (i, required_feature_idx) = be_u16(i)?;
    let (i, feature_index_count) = be_u16(i)?;
    let feature_list = iterator_n(i, be_u16, feature_index_count).map(FeatureIdx).collect();
    let required_feature = if required_feature_idx == 0xFFFF { None } else { Some(FeatureIdx(required_feature_idx)) };
    Ok(LanguageSystem {
        feature_list,
        required_feature
    })
}

fn parse_feature_list(data: &[u8]) -> Result<Vec<Feature>, FontError> {
    let (i, feature_count) = be_u16(data)?;
    let mut features = Vec::with_capacity(feature_count as usize);
    for (tag, feature_offset) in iterator_n(i, tuple((tag, be_u16)), feature_count) {
        let lookup_indices = parse_feature_table(slice!(data, feature_offset as usize ..))?.collect();
        features.push(Feature {
            tag,
            lookup_indices
        });
    }
    Ok(features)
}

fn parse_feature_table(i: &[u8]) -> Result<impl Iterator<Item=u16> + '_, FontError> {
    let (i, _feature_params) = be_u16(i)?;
    let (i, lookup_index_count) = be_u16(i)?;
    Ok(iterator_n(i, be_u16, lookup_index_count))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn feature(tag: &[u8; 4], lookup_indices: Vec<u16>) -> Feature {
        Feature { tag: Tag(*tag), lookup_indices }
    }

    fn words_to_bytes(words: &[u16]) -> Vec<u8> {
        words.iter().flat_map(|w| w.to_be_bytes().to_vec()).collect()
    }

    fn gsub_with(features: Vec<Feature>, lookup: Vec<Vec<LookupSubtable>>) -> GSub {
        GSub {
            scripts: vec![Script {
                default_language: Some(LanguageSystem {
                    feature_list: (0 .. features.len() as u16).map(FeatureIdx).collect(),
                    required_feature: None,
                }),
                languages: Vec::new(),
            }],
            features,
            lookup,
        }
    }

    #[test]
    fn ligature_substitution_picks_the_longest_match() {
        // OT §GSUB "Ligature Substitution": f f i must become ffi, not ff + i.
        let mut ligatures: HashMap<u16, Vec<(GlyphList, u16)>> = HashMap::new();
        ligatures.insert(10, vec![
            (GlyphList(vec![20]), 30),     // f + i   -> fi  (30)
            (GlyphList(vec![10, 20]), 40), // f + f + i -> ffi (40)
        ]);
        let gsub = gsub_with(
            vec![feature(b"liga", vec![0])],
            vec![vec![LookupSubtable::Substitution(Substitution::Ligatures(ligatures))]],
        );
        let liga = |tag: Tag| tag.0 == *b"liga";

        assert_eq!(
            gsub.substitute(&[GlyphId(10), GlyphId(10), GlyphId(20)], &liga),
            vec![GlyphId(40)]
        );
        assert_eq!(
            gsub.substitute(&[GlyphId(10), GlyphId(20)], &liga),
            vec![GlyphId(30)]
        );
        // A disabled feature substitutes nothing.
        let glyphs = [GlyphId(10), GlyphId(20)];
        assert_eq!(gsub.substitute(&glyphs, &|_| false), glyphs.to_vec());
    }

    #[test]
    fn single_substitution_replaces_one_glyph() {
        let mut replacements = HashMap::new();
        replacements.insert(20u16, 21u16);
        let gsub = gsub_with(
            vec![feature(b"smcp", vec![0])],
            vec![vec![LookupSubtable::Substitution(Substitution::Single(replacements))]],
        );
        assert_eq!(
            gsub.substitute(&[GlyphId(19), GlyphId(20)], &|tag| tag.0 == *b"smcp"),
            vec![GlyphId(19), GlyphId(21)]
        );
    }

    #[test]
    fn extension_lookup_is_unwrapped() {
        // OT §GSUB "Extension Substitution" (type 7) around a SingleSubstFormat2 (type 1):
        // glyph 20 -> 50.
        let mut data = Vec::new();
        data.extend_from_slice(&[0, 1]);       // extension format 1
        data.extend_from_slice(&[0, 1]);       // extensionLookupType = Single
        data.extend_from_slice(&[0, 0, 0, 8]); // extensionOffset
        data.extend_from_slice(&[0, 2]);       // SingleSubst format 2
        data.extend_from_slice(&[0, 8]);       // coverageOffset
        data.extend_from_slice(&[0, 1]);       // glyphCount
        data.extend_from_slice(&[0, 50]);      // substituteGlyphID
        data.extend_from_slice(&[0, 1]);       // Coverage format 1
        data.extend_from_slice(&[0, 1]);       // glyphCount
        data.extend_from_slice(&[0, 20]);      // glyph

        match parse_substitution(&data, 7).expect("extension parses") {
            Some(LookupSubtable::Substitution(Substitution::Single(map))) =>
                assert_eq!(map.get(&20), Some(&50)),
            other => panic!("expected a single substitution, got {:?}", other),
        }
    }

    #[test]
    fn ligature_with_component_count_zero_is_skipped() {
        // OT §GSUB "Ligature Substitution" table with two ligatures for glyph 10: a valid
        // 10 + 20 -> 30, and one with componentCount 0, which used to underflow
        // `component_count - 1` and abort.
        let words: [u16; 14] = [
            1, 24, 1, 8,          // format, coverage offset, ligatureSetCount, set offset
            2, 6, 12,             // ligatureCount, ligature[0] offset, ligature[1] offset (from set start)
            30, 2, 20,            // 10 + 20 -> 30
            40, 0,                // malformed: no components
            1, 1,                 // coverage format 1, glyphCount 1
        ];
        let mut data: Vec<u8> = words.iter().flat_map(|w| w.to_be_bytes().to_vec()).collect();
        data.extend_from_slice(&[0, 10]); // covered glyph 10

        let substitution = parse_ligatures(&data).expect("ligature table parses");
        // The valid ligature is still there...
        assert_eq!(substitution.apply(&[GlyphId(10), GlyphId(20)]), Some((2, GlyphId(30))));
        // ...and the broken one does not match anything.
        assert_eq!(substitution.apply(&[GlyphId(10)]), None);
    }

    #[test]
    fn out_of_range_feature_and_lookup_indices_are_ignored() {
        let gsub = GSub {
            scripts: vec![Script {
                default_language: Some(LanguageSystem {
                    // Feature index 9 does not exist; feature 0 points at lookup 7, which does not.
                    feature_list: vec![FeatureIdx(0), FeatureIdx(9)],
                    required_feature: None,
                }),
                languages: Vec::new(),
            }],
            features: vec![feature(b"liga", vec![7])],
            lookup: vec![Vec::new()],
        };
        let lang = gsub.default_language().expect("default language");
        assert_eq!(gsub.lang_features(lang).collect::<Vec<_>>(), vec![Tag(*b"liga")]);
        assert_eq!(gsub.subs(lang, |_| true).count(), 0);
        // Nor may the applying path panic.
        assert_eq!(gsub.substitute(&[GlyphId(1)], &|_| true), vec![GlyphId(1)]);
    }

    #[test]
    fn apply_ignores_glyph_ids_outside_u16() {
        let mut replacements = HashMap::new();
        replacements.insert(0u16, 1u16);
        let single = Substitution::Single(replacements);
        // `as u16` would turn 0x10000 into 0 and wrongly substitute.
        assert_eq!(single.apply(&[GlyphId(0x1_0000)]), None);
        assert_eq!(single.apply(&[GlyphId(0)]), Some((1, GlyphId(1))));
        assert_eq!(single.apply(&[]), None);
    }

    #[test]
    fn multiple_substitution_expands_and_deletes() {
        // MultipleSubstFormat1: glyph 10 -> 20 21, glyph 11 -> (empty, i.e. deletion).
        let words: [u16; 13] = [
            1, 18, 2, 10, 16, // format, coverage offset, sequenceCount, sequence offsets
            2, 20, 21,        // sequence for 10
            0,                // sequence for 11
            1, 2, 10, 11,     // coverage
        ];
        let data = words_to_bytes(&words);
        let substitution = parse_multiple_subst(&data).expect("multiple substitution parses");
        assert_eq!(substitution.apply(&[GlyphId(10)]), Some((1, vec![GlyphId(20), GlyphId(21)])));
        assert_eq!(substitution.apply(&[GlyphId(11)]), Some((1, vec![])));
        assert_eq!(substitution.apply(&[GlyphId(12)]), None);

        // Through the lookup machinery: 9 10 becomes 9 20 21.
        let gsub = gsub_with(
            vec![feature(b"ccmp", vec![0])],
            vec![vec![substitution]],
        );
        assert_eq!(
            gsub.substitute(&[GlyphId(9), GlyphId(10)], &|tag| tag.0 == *b"ccmp"),
            vec![GlyphId(9), GlyphId(20), GlyphId(21)]
        );
    }

    #[test]
    fn alternate_substitution_picks_the_requested_alternate() {
        // AlternateSubstFormat1: glyph 10 -> 30 or 31.
        let words: [u16; 10] = [
            1, 14, 1, 8,      // format, coverage offset, alternateSetCount, set offset
            2, 30, 31,        // the alternate set
            1, 1, 10,         // coverage
        ];
        let data = words_to_bytes(&words);
        let substitution = parse_alternate_subst(&data).expect("alternate substitution parses");
        // OT §GSUB leaves the choice to the application; the first one is the default.
        assert_eq!(substitution.apply(&[GlyphId(10)]), Some((1, vec![GlyphId(30)])));
        assert_eq!(substitution.apply_with(&[GlyphId(10)], 1), Some((1, vec![GlyphId(31)])));
        // An index past the end falls back to the first alternate.
        assert_eq!(substitution.apply_with(&[GlyphId(10)], 9), Some((1, vec![GlyphId(30)])));
    }

    #[test]
    fn context_substitution_runs_the_lookup_its_rule_names() {
        // OT §GSUB "Context Substitution" (lookup type 5), format 1: glyph 10 followed by 20
        // runs lookup 0 at the second glyph of the run (20 -> 99).
        let words: [u16; 14] = [
            1, 22, 1, 8,          // format, coverageOffset, subRuleSetCount, set offset
            1, 4,                 // SubRuleSet: one rule, at +4
            2, 1, 20,             // SubRule: glyphCount 2, substCount 1, input[1] = glyph 20
            1, 0,                 // record: sequenceIndex 1, lookupListIndex 0
            1, 1, 10,             // coverage: format 1, one glyph, 10
        ];
        let data = words_to_bytes(&words);
        let context = parse_substitution(&data, 5).expect("context parses").expect("a subtable");

        let mut replacements = HashMap::new();
        replacements.insert(20u16, 99u16);
        let gsub = gsub_with(
            vec![feature(b"calt", vec![1])],
            vec![
                vec![LookupSubtable::Substitution(Substitution::Single(replacements))],
                vec![context],
            ],
        );
        let calt = |tag: Tag| tag.0 == *b"calt";
        assert_eq!(
            gsub.substitute(&[GlyphId(10), GlyphId(20)], &calt),
            vec![GlyphId(10), GlyphId(99)]
        );
        // Without the leading glyph the rule does not match and nothing is substituted.
        assert_eq!(gsub.substitute(&[GlyphId(20)], &calt), vec![GlyphId(20)]);
    }

    #[test]
    fn chaining_context_format3_matches_backtrack_and_lookahead() {
        // OT §GSUB "Chaining Context Substitution" (6), format 3: 30 | 10 20 | 40 runs lookup
        // 0 at the first glyph of the input (10 -> 77).
        let words: [u16; 23] = [
            3, 1, 22,             // format, backtrackGlyphCount, backtrack coverage offset
            2, 28, 34,            // inputGlyphCount, input coverage offsets
            1, 40,                // lookaheadGlyphCount, lookahead coverage offset
            1, 0, 0,              // substCount 1; record: sequenceIndex 0, lookupListIndex 0
            1, 1, 30,             // backtrack coverage: glyph 30
            1, 1, 10,             // input coverage 0: glyph 10
            1, 1, 20,             // input coverage 1: glyph 20
            1, 1, 40,             // lookahead coverage: glyph 40
        ];
        let data = words_to_bytes(&words);
        let context = parse_substitution(&data, 6).expect("chaining context parses").expect("a subtable");

        let mut replacements = HashMap::new();
        replacements.insert(10u16, 77u16);
        let gsub = gsub_with(
            vec![feature(b"calt", vec![1])],
            vec![
                vec![LookupSubtable::Substitution(Substitution::Single(replacements))],
                vec![context],
            ],
        );
        let calt = |tag: Tag| tag.0 == *b"calt";
        assert_eq!(
            gsub.substitute(&[GlyphId(30), GlyphId(10), GlyphId(20), GlyphId(40)], &calt),
            vec![GlyphId(30), GlyphId(77), GlyphId(20), GlyphId(40)]
        );
        // A wrong glyph on either side of the run means no match.
        assert_eq!(
            gsub.substitute(&[GlyphId(31), GlyphId(10), GlyphId(20), GlyphId(40)], &calt),
            vec![GlyphId(31), GlyphId(10), GlyphId(20), GlyphId(40)]
        );
        assert_eq!(
            gsub.substitute(&[GlyphId(30), GlyphId(10), GlyphId(20), GlyphId(41)], &calt),
            vec![GlyphId(30), GlyphId(10), GlyphId(20), GlyphId(41)]
        );
    }

    #[test]
    fn reverse_chaining_substitutes_in_its_own_pass() {
        // OT §GSUB "Reverse Chaining Contextual Single Substitution" (8), format 1:
        // 10 | 50 | 20 turns glyph 50 into 60.
        let words: [u16; 17] = [
            1, 16,                // format, coverageOffset
            1, 22,                // backtrackGlyphCount, backtrack coverage offset
            1, 28,                // lookaheadGlyphCount, lookahead coverage offset
            1, 60,                // glyphCount, substituteGlyphID
            1, 1, 50,             // coverage: glyph 50
            1, 1, 10,             // backtrack coverage: glyph 10
            1, 1, 20,             // lookahead coverage: glyph 20
        ];
        let data = words_to_bytes(&words);
        let reverse = parse_substitution(&data, 8).expect("reverse chaining parses").expect("a subtable");
        let gsub = gsub_with(vec![feature(b"calt", vec![0])], vec![vec![reverse]]);
        let calt = |tag: Tag| tag.0 == *b"calt";
        assert_eq!(
            gsub.substitute(&[GlyphId(10), GlyphId(50), GlyphId(20)], &calt),
            vec![GlyphId(10), GlyphId(60), GlyphId(20)]
        );
        // Without the backtrack glyph the lookup does not apply.
        assert_eq!(
            gsub.substitute(&[GlyphId(50), GlyphId(20)], &calt),
            vec![GlyphId(50), GlyphId(20)]
        );
    }

    #[test]
    fn class_matchers_treat_unlisted_glyphs_as_class_zero() {
        let mut classes = HashMap::new();
        classes.insert(10u16, 3u16);
        assert!(GlyphMatch::Class(3).matches(&classes, 10));
        // OT §"Class Definition Table": everything the definition does not list is class 0.
        assert!(GlyphMatch::Class(0).matches(&classes, 11));
        assert!(!GlyphMatch::Class(3).matches(&classes, 11));
        assert!(GlyphMatch::Coverage(vec![1, 5, 9]).matches(&classes, 5));
        assert!(!GlyphMatch::Coverage(vec![1, 5, 9]).matches(&classes, 6));
    }
}
