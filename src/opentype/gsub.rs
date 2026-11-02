use std::collections::HashMap;
use nom::{
    number::complete::{be_u16, be_u32},
    multi::count,
    bytes::complete::take,
    sequence::tuple,
};
use crate::{R, GlyphId, FontError};
use crate::parsers::{iterator_n};
use crate::opentype::{parse_extension, parse_lookup_list, coverage_table, tag, Tag};

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
            LookupSubtable::Multiple(_) | LookupSubtable::Alternate(_) => None,
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
/// what `GSub::subs` yields; `Multiple` and `Alternate` are applied by `GSub::substitute`,
/// which also needed the type left alone for the consumers that match `Substitution` exactly.
#[derive(Debug, Clone)]
pub enum LookupSubtable {
    /// OT §GSUB lookup type 1 or 4 (`Substitution`).
    Substitution(Substitution),
    /// OT §GSUB "Multiple Substitution": one glyph becomes a sequence. An empty sequence is
    /// legal and deletes the glyph.
    Multiple(HashMap<u16, Vec<u16>>),
    /// OT §GSUB "Alternate Substitution": one glyph becomes one of several.
    Alternate(HashMap<u16, Vec<u16>>),
}

impl LookupSubtable {
    /// If this subtable applies at the start of `glyphs`: how many glyphs it consumes and the
    /// glyphs that replace them. An `Alternate` subtable yields its first alternate;
    /// `apply_with` picks another.
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
        }
    }
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

impl GSub {
    /// Applies the lookups of the default language system whose feature tag `enabled`
    /// accepts, once each, left to right — the simple subset of OT §"Lookups" this crate
    /// implements. Single (1), Multiple (2), Alternate (3, first alternate) and Ligature (4)
    /// lookups are applied; Context (5), Chaining Context (6) and Reverse Chaining (8) carry
    /// no subtable yet and are skipped; unrecognised glyphs pass through.
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
            'lookups: for &lookup_idx in &lookup_indices {
                let lookups = match self.lookup.get(lookup_idx as usize) {
                    Some(lookups) => lookups,
                    None => continue,
                };
                for subtable in lookups {
                    if let Some((consumed, replacement)) = subtable.apply(&out[position ..]) {
                        let inserted = replacement.len();
                        out.splice(position .. position + consumed, replacement);
                        // OT §"Lookups": what a lookup produces is not fed back into it.
                        position += inserted;
                        applied = true;
                        break 'lookups;
                    }
                }
            }
            if !applied {
                position += 1;
            }
        }
        out
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

/// One subtable of a `GSUB` lookup, as the substitutions this crate can apply. Lookup types
/// 5, 6 and 8 are recognised but not implemented, and yield `None`.
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
        5 => { debug!("font has Context substitutions"); return Ok(None); }
        // Chaining Context · Replace one or more glyphs in chained context
        6 => { debug!("font has Chaining Context substitutions"); return Ok(None); }
        // Reverse chaining context single · applied in reverse order
        8 => { debug!("font has Reverse chaining context single substitutions"); return Ok(None); }
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
}
