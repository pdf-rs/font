use std::collections::HashMap;
use nom::{
    number::complete::{be_u16, be_u32},
    multi::count
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
pub struct Gsub {
    ligatures: HashMap<u16, Vec<(GlyphList, u16)>>
}
impl Gsub {
    pub fn substitutions<'a>(&'a self, first: GlyphId) -> Option<impl Iterator<Item=(&'a GlyphList, GlyphId)> + 'a> {
        self.ligatures.get(&(first.0 as u16)).map(|subs| subs.iter().map(|&(ref others, substutute)| {
            (others, GlyphId(substutute as u32))
        }))
    }
}

pub fn parse_gsub(data: &[u8]) -> R<Gsub> {
    debug!("parse GSUB");
    let (i, major_version) = be_u16(data)?;
    assert_eq!(major_version, 1);
    let (i, minor_version) = be_u16(i)?;
    
    let (i, _script_list_off) = be_u16(i)?;
    let (i, _feature_list_off) = be_u16(i)?;
    let (i, lookup_list_off) = be_u16(i)?;
    
    let (i, _feature_variations_offset) = match minor_version {
        0 => (i, 0),
        1 => be_u32(i)?,
        v => panic!("unsupported GPOS version 1.{}", v)
    };
    
    let mut gsub = Gsub {
        ligatures: HashMap::new()
    };
    
    parse_lookup_list(&data[lookup_list_off as usize ..], |data, lookup_type, _lookup_flag| {
        match lookup_type { 
            1 => {} // Single · Replace one glyph with one glyph
            2 => {} // Multiple · Replace one glyph with more than one glyph
            3 => {} // Alternate · Replace one glyph with one of many glyphs
            4 => {
                // Ligature · Replace multiple glyphs with one glyph
                parse_ligatures(data, &mut gsub)?;
            }
            5 => {} // Context · Replace one or more glyphs in context
            6 => {} // Chaining Context · Replace one or more glyphs in chained context
            7 => {} // Extension Substitution · Extension mechanism for other substitutions (i.e. this excludes the Extension type substitution itself)
            8 => {} // Reverse chaining context single · Applied in reverse order, replace single glyph in chaining context
            _ => {}
        }
        Ok((data, ()))
    })?;
    
    Ok((i, gsub))
}

fn parse_ligatures<'a>(data: &'a [u8], gsub: &mut Gsub) -> R<'a, ()> {
    let (i, format) = be_u16(data)?;
    assert_eq!(format, 1);
    
    let (i, coverage_offset) = be_u16(i)?;
    let (i, ligature_set_count) = be_u16(i)?;
    let (_, coverage) = coverage_table(&data[coverage_offset as usize ..])?;

    for (first, offset) in coverage.zip(iterator_n(i, be_u16, ligature_set_count)) {
        let set_data = &data[offset as usize ..];
        let (i, ligature_count) = be_u16(set_data)?;
        let entry = gsub.ligatures.entry(first).or_insert(vec![]);

        for set_offest in iterator_n(i, be_u16, ligature_count) {
            let data = &set_data[set_offest as usize ..];
            let (i, ligature_glyph) = be_u16(data)?;
            let (i, component_count) = be_u16(i)?;
            let (_, components) = count(be_u16, component_count as usize - 1)(i)?;
            
            entry.push((GlyphList(components), ligature_glyph));
        }
    }
    Ok((i, ()))
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
