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
    lookup: Vec<Vec<Substitution>>,
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
    pub fn subs<'a, 'b: 'a>(&'b self, lang: &'a LanguageSystem, enabled: impl Fn(Tag) -> bool + 'a) -> impl Iterator<Item=&'b Substitution> + 'a {
        lang.feature_list.iter()
        .map(move |&FeatureIdx(idx)| &self.features[idx as usize])
        .filter(move |feature| enabled(feature.tag))
        .flat_map(move |feature| feature.lookup_indices.iter().flat_map(move |&lookup_idx| self.lookup[lookup_idx as usize].iter()))
    }
    pub fn lang_features<'a>(&'a self, lang: &'a LanguageSystem) -> impl Iterator<Item=Tag> + 'a {
        lang.feature_list.iter()
        .map(move |&FeatureIdx(idx)| self.features[idx as usize].tag)
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
        let (&first, rest) = match glyphs.split_first() {
            Some(split) => split,
            None => return None,
        };
        match self {
            Substitution::Single(replacements) =>
                replacements.get(&(first.0 as u16)).map(|&replacement| (1, GlyphId(replacement as u32))),
            Substitution::Ligatures(ligatures) => {
                let ligatures = ligatures.get(&(first.0 as u16))?;
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

impl GSub {
    /// Applies the lookups of the default language system whose feature tag `enabled`
    /// accepts, once each, left to right — the simple subset of OT §"Lookups" this crate
    /// implements. Lookup types that are not implemented (`Context`, `Alternate`, …) carry
    /// no `Substitution` and are skipped; unrecognised glyphs pass through.
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
                for substitution in lookups {
                    if let Some((consumed, glyph)) = substitution.apply(&out[position ..]) {
                        out.splice(position .. position + consumed, Some(glyph));
                        position += 1;
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
/// 2, 3, 5, 6 and 8 are recognised but not implemented, and yield `None`.
fn parse_substitution(data: &[u8], lookup_type: u16) -> Result<Option<Substitution>, FontError> {
    Ok(Some(match lookup_type {
        // Single · Replace one glyph with one glyph
        1 => parse_single_subst(data)?,
        // Ligature · Replace multiple glyphs with one glyph
        4 => parse_ligatures(data)?,
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
        2 => { debug!("font has Multiple substitutions"); return Ok(None); }
        // Alternate · Replace one glyph with one of many glyphs
        3 => { debug!("font has Alternate substitutions"); return Ok(None); }
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
            let (_, components) = count(be_u16, component_count as usize - 1)(i)?;
            
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

    fn gsub_with(features: Vec<Feature>, lookup: Vec<Vec<Substitution>>) -> GSub {
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
            vec![vec![Substitution::Ligatures(ligatures)]],
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
            vec![vec![Substitution::Single(replacements)]],
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
            Some(Substitution::Single(map)) => assert_eq!(map.get(&20), Some(&50)),
            other => panic!("expected a single substitution, got {:?}", other),
        }
    }
}
