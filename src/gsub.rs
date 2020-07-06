use std::collections::HashMap;
use nom::{
    number::complete::{be_u16, be_u32},
    multi::count,
    bytes::complete::take,
    sequence::tuple,
};
use crate::{R, GlyphId};
use crate::parsers::{iterator_n};
use crate::opentype::{parse_lookup_list, coverage_table};

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
pub struct CompiledSubs {
    // first glyph -> (remaining glyphs, replacement)
    ligatures: HashMap<u16, Vec<(GlyphList, u16)>>,

    // input -> output
    single_substitutions: HashMap<u16, u16>,
}
impl CompiledSubs {
    fn new() -> CompiledSubs {
        CompiledSubs {
            ligatures: HashMap::new(),
            single_substitutions: HashMap::new()
        }
    }
    fn add(&mut self, sub: &Substitution) {
        match *sub {
            Substitution::Single(ref list) => {
                self.single_substitutions.extend(list.iter().cloned());
            }
            Substitution::Ligatures(ref list) => {
                self.ligatures.extend(list.iter().cloned());
            }
        }
    }
    pub fn substitutions<'a>(&'a self, first: GlyphId) -> Option<impl Iterator<Item=(&'a GlyphList, GlyphId)> + 'a> {
        self.ligatures.get(&(first.0 as u16)).map(|subs| subs.iter().map(|&(ref others, substutute)| {
            (others, GlyphId(substutute as u32))
        }))
    }
}
#[derive(Debug, Clone)]
pub struct Gsub {
    scripts: Vec<Script>,
    features: Vec<Feature>,
    lookup: Vec<Substitution>,
    all: Substitution,
}
impl Gsub {
    pub fn substitutions<'a>(&'a self, first: GlyphId) -> Option<impl Iterator<Item=(&'a GlyphList, GlyphId)> + 'a> {


    pub fn scripts(&self) -> &[Script] {
        &self.scripts
    }
    fn compile_lang(&self, lang: &LanguageSystem) -> CompiledSubs {
        let mut compiled = CompiledSubs::new();
        for &feature_idx in &lang.feature_list {
            for &lookup_idx in &self.features[feature_idx].lookup_indices {
                compiled.add(&self.lookup[lookup_idx]);
            }
        }
        compiled
    }
}

#[derive(Debug, Clone)]
enum Substitution {
    Single(Vec<(u16, u16)>),
    Ligatures(Vec<(u16, Vec<(GlyphList, u16)> )>),
}

pub fn parse_gsub(data: &[u8]) -> R<Gsub> {
    debug!("parse GSUB");
    let (i, major_version) = be_u16(data)?;
    assert_eq!(major_version, 1);
    let (i, minor_version) = be_u16(i)?;
    
    let (i, script_list_off) = be_u16(i)?;
    let (i, feature_list_off) = be_u16(i)?;
    let (i, lookup_list_off) = be_u16(i)?;
    
    let (_, script_list) = parse_script_list(&data[script_list_off as usize ..])?;
    let (_, feature_list) = parse_feature_list(&data[feature_list_off as usize ..])?;

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

    let (i, _feature_variations_offset) = match minor_version {
        0 => (i, 0),
        1 => be_u32(i)?,
        v => panic!("unsupported GPOS version 1.{}", v)
    };
    
    let mut lookup = Vec::new();
    
    parse_lookup_list(&data[lookup_list_off as usize ..], |data, lookup_type, _lookup_flag| {
        let sub = match lookup_type { 
            // Single · Replace one glyph with one glyph
            1 => lookup.push(parse_single_subst(data)?.1), 

            2 => {}, // Multiple · Replace one glyph with more than one glyph
            3 => {}, // Alternate · Replace one glyph with one of many glyphs

            // Ligature · Replace multiple glyphs with one glyph
            4 => lookup.push(parse_ligatures(data)?.1),
            
            5 => {}, // Context · Replace one or more glyphs in context
            6 => {}, // Chaining Context · Replace one or more glyphs in chained context
            7 => {}, // Extension Substitution · Extension mechanism for other substitutions (i.e. this excludes the Extension type substitution itself)
            8 => {}, // Reverse chaining context single · Applied in reverse order, replace single glyph in chaining context
            _ => {},
        };
        Ok((data, ()))
    })?;
    
    Ok((i, Gsub {
        lookup,
        scripts: script_list,
        features: feature_list,
    }))
}

fn parse_single_subst(data: &[u8]) -> R<Substitution> {
    let (i, format) = be_u16(data)?;
    let mut subs = Vec::new();
    match format {
        1 => {
            let (i, coverage_offset) = be_u16(i)?;
            let (i, delta_gid) = be_u16(i)?;
            let (_, coverage) = coverage_table(&data[coverage_offset as usize ..])?;
            for gid in coverage {
                subs.push((gid, gid.wrapping_add(delta_gid)));
            }
        },
        2 => {
            let (i, coverage_offset) = be_u16(i)?;
            let (i, glyph_count) = be_u16(i)?;
            let (_, coverage) = coverage_table(&data[coverage_offset as usize ..])?;
            let replacements = iterator_n(i, be_u16, glyph_count);
            for (gid, replacement_gid) in coverage.zip(replacements) {
                subs.push((gid, replacement_gid));
            }
        }
        _ => panic!("unsupported single substitution format {}", format)
    }
    Ok((i, Substitution::Single(subs)))
}


fn parse_ligatures(data: &[u8]) -> R<Substitution> {
    let (i, format) = be_u16(data)?;
    assert_eq!(format, 1);

    let (i, coverage_offset) = be_u16(i)?;
    let (i, ligature_set_count) = be_u16(i)?;
    let (_, coverage) = coverage_table(&data[coverage_offset as usize ..])?;
    let mut ligatures = Vec::with_capacity(ligature_set_count as usize);

    for (first, offset) in coverage.zip(iterator_n(i, be_u16, ligature_set_count)) {
        let set_data = &data[offset as usize ..];
        let (i, ligature_count) = be_u16(set_data)?;
        let mut entry = Vec::with_capacity(ligature_count as usize);

        for set_offest in iterator_n(i, be_u16, ligature_count) {
            let data = &set_data[set_offest as usize ..];
            let (i, ligature_glyph) = be_u16(data)?;
            let (i, component_count) = be_u16(i)?;
            let (_, components) = count(be_u16, component_count as usize - 1)(i)?;
            
            entry.push((GlyphList(components), ligature_glyph));
        }
        ligatures.push((first, entry));
    }
    Ok((i, Substitution::Ligatures(ligatures)))
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

fn parse_script_list(data: &[u8]) -> R<Vec<Script>> {
    let (i, script_count) = be_u16(data)?;
    let mut scripts = Vec::with_capacity(script_count as usize);
    for (tag, offset) in iterator_n(i, tuple((take(4usize), be_u16)), script_count) {
        scripts.push(parse_script_table(&data[offset as usize .. ])?.1);
    }
    Ok((i, scripts))
}
fn parse_script_table(data: &[u8]) -> R<Script> {
    let (i, default_lang_offset) = be_u16(data)?;
    let default_language = if default_lang_offset != 0 {
        Some(parse_language_system_table(&data[default_lang_offset as usize ..])?.1)
    } else {
        None
    };

    let (i, lang_sys_count) = be_u16(i)?;
    let mut languages = Vec::with_capacity(lang_sys_count as usize);
    for (tag, lang_sys_offset) in iterator_n(i, tuple((tag, be_u16)), lang_sys_count) {
        languages.push((tag, parse_language_system_table(&data[lang_sys_offset as usize ..])?.1));
    }
    Ok((i, Script {
        default_language,
        languages,
    }))
}

#[derive(Copy, Clone, Debug)]
struct FeatureIdx(u16);

#[derive(Debug, Clone)]
struct Script {
    default_language: Option<LanguageSystem>,
    languages: Vec<(Tag, LanguageSystem)>
}

#[derive(Debug, Clone)]
struct LanguageSystem {
    feature_list: Vec<FeatureIdx>,
    required_feature: Option<FeatureIdx>,
}

#[derive(Debug, Clone)]
struct Feature {
    tag: Tag,
    lookup_indices: Vec<u16>
}
// returns (requiredFeatureIndex, FeatureList)
fn parse_language_system_table(i: &[u8]) -> R<LanguageSystem> {
    let (i, _lookup_order) = be_u16(i)?;
    let (i, required_feature_idx) = be_u16(i)?;
    let (i, feature_index_count) = be_u16(i)?;
    let feature_list = iterator_n(i, be_u16, feature_index_count).map(FeatureIdx).collect();
    let required_feature = if required_feature_idx == 0xFFFF { None } else { Some(FeatureIdx(required_feature_idx)) };
    Ok((i, LanguageSystem {
        feature_list,
        required_feature
    }))
}

fn parse_feature_list(data: &[u8]) -> R<Vec<Feature>> {
    let (i, feature_count) = be_u16(data)?;
    let mut features = Vec::with_capacity(feature_count as usize);
    for (tag, feature_offset) in iterator_n(i, tuple((tag, be_u16)), feature_count) {
        features.push(Feature {
            tag,
            lookup_indices: parse_feature_table(&data[feature_offset as usize ..])?.1.collect()
        });
    }
    Ok((i, features))
}

fn parse_feature_table(i: &[u8]) -> R<impl Iterator<Item=u16> + '_> {
    let (i, _feature_params) = be_u16(i)?;
    let (i, lookup_index_count) = be_u16(i)?;
    Ok((i, iterator_n(i, be_u16, lookup_index_count)))
}

use std::fmt;
#[derive(Copy, Clone)]
pub struct Tag(pub [u8; 4]);
impl fmt::Debug for Tag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match std::str::from_utf8(&self.0) {
            Ok(s) => f.write_str(s),
            Err(_) => write!(f, "{:?}", self.0)
        }
    }
}
fn tag(i: &[u8]) -> R<Tag> {
    let (i, s) = take(4usize)(i)?;
    let mut arr = [0; 4];
    arr.copy_from_slice(s);
    Ok((i, Tag(arr)))
}
