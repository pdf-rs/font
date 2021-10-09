use std::collections::HashMap;
use nom::{
    number::complete::{be_u16, be_u32},
    multi::count,
    bytes::complete::take,
    sequence::tuple,
};
use crate::{R, GlyphId, FontError};
use crate::parsers::{iterator_n};
use crate::opentype::{parse_lookup_list, coverage_table, tag, Tag};

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
        v => panic!("unsupported GPOS version 1.{}", v)
    };
    
    let mut lookup = Vec::new();
    
    parse_lookup_list(slice!(data, lookup_list_off as usize ..), |lookup_idx, data, lookup_type, _lookup_flag| {
        while lookup_idx >= lookup.len() {
            lookup.push(Vec::new());
        }
        let mut push = |sub| lookup[lookup_idx].push(sub);

        match lookup_type { 
            // Single · Replace one glyph with one glyph
            1 => push(parse_single_subst(data)?), 

            2 => debug!("font has Multiple substitutions"), // Multiple · Replace one glyph with more than one glyph
            3 => debug!("font has Altername substitutions"), // Alternate · Replace one glyph with one of many glyphs

            // Ligature · Replace multiple glyphs with one glyph
            4 => push(parse_ligatures(data)?),
            
            5 => debug!("font has Context substitutions"), // Context · Replace one or more glyphs in context
            6 => debug!("font has Chaining Context substitutions"), // Chaining Context · Replace one or more glyphs in chained context
            7 => debug!("font has Extension Substitution"), // Extension Substitution · Extension mechanism for other substitutions (i.e. this excludes the Extension type substitution itself)
            8 => debug!("font has Reverse chaining context single substitutions"), // Reverse chaining context single · Applied in reverse order, replace single glyph in chaining context
            _ => {},
        }
        Ok(())
    })?;
    
    Ok(GSub {
        lookup,
        scripts: script_list,
        features: feature_list,
    })
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
