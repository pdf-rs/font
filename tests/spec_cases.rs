//! Minimal in-memory fonts, one per specification mismatch reported in
//! `report/pdf-rs-font-vs-specs-2026-11-02.md`.
//!
//! Each case builds the smallest sfnt that still reaches the code under test, so a failure names
//! exactly one finding. Run everything: `cargo test`; one group: `cargo test c01`.

use font::opentype::{cmap::parse_cmap, gdef::{GDef, MarkClass}, OpenTypeFont};
use font::{Font, GlyphId};

// ---------------------------------------------------------------------------------------------
// sfnt assembly

pub fn be16(v: u16) -> [u8; 2] { v.to_be_bytes() }
pub fn be32(v: u32) -> [u8; 4] { v.to_be_bytes() }

fn push_u16(out: &mut Vec<u8>, v: u16) { out.extend_from_slice(&be16(v)); }
fn push_i16(out: &mut Vec<u8>, v: i16) { out.extend_from_slice(&v.to_be_bytes()); }
fn push_u32(out: &mut Vec<u8>, v: u32) { out.extend_from_slice(&be32(v)); }

/// `head` (OT §head), 54 bytes, `indexToLocFormat = 0`, `unitsPerEm = 1000`.
pub fn head_table() -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 1);            // majorVersion
    push_u16(&mut t, 0);            // minorVersion
    push_u32(&mut t, 0x0001_0000);  // fontRevision
    push_u32(&mut t, 0);            // checkSumAdjustment
    push_u32(&mut t, 0x5F0F_3CF5);  // magicNumber
    push_u16(&mut t, 0);            // flags
    push_u16(&mut t, 1000);         // unitsPerEm
    t.extend_from_slice(&[0; 8]);   // created
    t.extend_from_slice(&[0; 8]);   // modified
    push_i16(&mut t, 0);            // xMin
    push_i16(&mut t, 0);            // yMin
    push_i16(&mut t, 1000);         // xMax
    push_i16(&mut t, 1000);         // yMax
    push_u16(&mut t, 0);            // macStyle
    push_u16(&mut t, 8);            // lowestRecPPEM
    push_u16(&mut t, 2);            // fontDirectionHint
    push_i16(&mut t, 0);            // indexToLocFormat
    push_u16(&mut t, 0);            // glyphDataFormat
    assert_eq!(t.len(), 54);
    t
}

/// `hhea` (OT §hhea), 36 bytes.
pub fn hhea_table(num_metrics: u16) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 1);        // majorVersion
    push_u16(&mut t, 0);        // minorVersion
    push_i16(&mut t, 800);      // ascender
    push_i16(&mut t, -200);     // descender
    push_i16(&mut t, 0);        // lineGap
    push_u16(&mut t, 1000);     // advanceWidthMax
    push_i16(&mut t, 0);        // minLeftSideBearing
    push_i16(&mut t, 0);        // minRightSideBearing
    push_i16(&mut t, 1000);     // xMaxExtent
    push_i16(&mut t, 1);        // caretSlopeRise
    push_i16(&mut t, 0);        // caretSlopeRun
    push_i16(&mut t, 0);        // caretOffset
    for _ in 0 .. 4 { push_i16(&mut t, 0); } // reserved
    push_i16(&mut t, 0);        // metricDataFormat
    push_u16(&mut t, num_metrics);
    assert_eq!(t.len(), 36);
    t
}

/// `maxp` (OT §maxp), version 1.0 header; the crate reads only `numGlyphs`.
pub fn maxp_table(num_glyphs: u16) -> Vec<u8> {
    let mut t = Vec::new();
    push_u32(&mut t, 0x0001_0000);
    push_u16(&mut t, num_glyphs);
    t
}

/// `hmtx` with `n` full metrics, advance 500, lsb 0.
pub fn hmtx_table(n: u16) -> Vec<u8> {
    let mut t = Vec::new();
    for _ in 0 .. n {
        push_u16(&mut t, 500);
        push_i16(&mut t, 0);
    }
    t
}

/// Assemble an sfnt: header, directory, then the tables (4-byte aligned).
pub fn build_font(tables: Vec<([u8; 4], Vec<u8>)>) -> Vec<u8> {
    let num_tables = tables.len() as u16;
    let mut out = Vec::new();
    push_u32(&mut out, 0x0001_0000);     // sfntVersion
    push_u16(&mut out, num_tables);
    push_u16(&mut out, 0);               // searchRange (not validated by the crate)
    push_u16(&mut out, 0);               // entrySelector
    push_u16(&mut out, 0);               // rangeShift

    let mut offset = 12 + 16 * tables.len();
    let mut body = Vec::new();
    for (tag, data) in &tables {
        out.extend_from_slice(tag);
        push_u32(&mut out, 0);           // checkSum
        push_u32(&mut out, offset as u32);
        push_u32(&mut out, data.len() as u32);
        body.extend_from_slice(data);
        while body.len() % 4 != 0 { body.push(0); }
        offset = 12 + 16 * tables.len() + body.len();
    }
    out.extend_from_slice(&body);
    out
}

/// The tables every test font must carry to get through `OpenTypeFont::from_tables`.
/// `hmtx` carries one full metric per glyph (`numberOfHMetrics == numGlyphs`), which is
/// the only layout that is valid without trailing left side bearings.
pub fn base_tables(num_glyphs: u16) -> Vec<([u8; 4], Vec<u8>)> {
    vec![
        (*b"head", head_table()),
        (*b"hhea", hhea_table(num_glyphs)),
        (*b"maxp", maxp_table(num_glyphs)),
        (*b"hmtx", hmtx_table(num_glyphs)),
    ]
}

/// ClassDef format 1 (OT §"Class Definition Table"): classes for `start`, `start+1`, …
pub fn class_def_1(start: u16, classes: &[u16]) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 1);
    push_u16(&mut t, start);
    push_u16(&mut t, classes.len() as u16);
    for &c in classes { push_u16(&mut t, c); }
    t
}

/// `GDEF` 1.0 with optional `glyphClassDef` and `markAttachClassDef`.
pub fn gdef_table(glyph_class: Option<Vec<u8>>, mark_attach_class: Option<Vec<u8>>) -> Vec<u8> {
    let head_len = 12; // version + four offsets
    let glyph_off = glyph_class.as_ref().map(|_| head_len);
    let glyph_len = glyph_class.as_ref().map(|c| c.len()).unwrap_or(0);
    let attach_off = mark_attach_class.as_ref().map(|_| head_len + glyph_len);

    let mut t = Vec::new();
    push_u16(&mut t, 1); // majorVersion
    push_u16(&mut t, 0); // minorVersion
    push_u16(&mut t, glyph_off.unwrap_or(0) as u16);
    push_u16(&mut t, 0); // attachListOffset
    push_u16(&mut t, 0); // ligCaretListOffset
    push_u16(&mut t, attach_off.unwrap_or(0) as u16);
    if let Some(c) = glyph_class { t.extend_from_slice(&c); }
    if let Some(c) = mark_attach_class { t.extend_from_slice(&c); }
    t
}

fn parse_gdef_of(data: &[u8]) -> GDef {
    let font = OpenTypeFont::parse(data).expect("font parses");
    font.gdef.expect("GDEF present")
}

// ---------------------------------------------------------------------------------------------
// F1/F2 — GPOS PairPos: the format 1 header, and format 2 class definitions

/// Coverage format 1 (OT §"Coverage Table").
fn coverage_1(glyphs: &[u16]) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 1);
    push_u16(&mut t, glyphs.len() as u16);
    for &g in glyphs { push_u16(&mut t, g); }
    t
}

/// A GPOS table with one lookup of `lookup_type` carrying a single subtable: 10-byte header,
/// lookup list at 10, lookup at 14, subtable at 22.
fn gpos_table(lookup_type: u16, subtable: &[u8]) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 1);   // majorVersion
    push_u16(&mut t, 0);   // minorVersion
    push_u16(&mut t, 0);   // scriptListOffset (not read by the crate)
    push_u16(&mut t, 0);   // featureListOffset (not read by the crate)
    push_u16(&mut t, 10);  // lookupListOffset
    push_u16(&mut t, 1);   // lookupCount
    push_u16(&mut t, 4);   // lookup offset, from the lookup list
    push_u16(&mut t, lookup_type);
    push_u16(&mut t, 0);   // lookupFlag
    push_u16(&mut t, 1);   // subtableCount
    push_u16(&mut t, 8);   // subtable offset, from the lookup
    assert_eq!(t.len(), 22);
    t.extend_from_slice(subtable);
    t
}

/// PairPos format 1 (OT §"Pair Adjustment Lookup"), `valueFormat1 = 0x0004` (xAdvance),
/// `valueFormat2 = 0`.
fn pair_pos_1(first_glyph: u16, pairs: &[(u16, i16)]) -> Vec<u8> {
    let coverage = coverage_1(&[first_glyph]);
    let mut pair_set = Vec::new();
    push_u16(&mut pair_set, pairs.len() as u16);
    for &(second, x_advance) in pairs {
        push_u16(&mut pair_set, second);
        push_i16(&mut pair_set, x_advance);
    }

    let header_len = 12; // format, coverageOffset, valueFormat1, valueFormat2, pairSetCount, 1 offset
    let mut t = Vec::new();
    push_u16(&mut t, 1);                                // format
    push_u16(&mut t, header_len as u16);                // coverageOffset
    push_u16(&mut t, 0x0004);                           // valueFormat1
    push_u16(&mut t, 0);                                // valueFormat2
    push_u16(&mut t, 1);                                // pairSetCount
    push_u16(&mut t, (header_len + coverage.len()) as u16); // pairSetOffset[0]
    t.extend_from_slice(&coverage);
    t.extend_from_slice(&pair_set);
    t
}

/// PairPos format 2 (OT §"Pair Adjustment Lookup"), `valueFormat1 = 0x0004`, `valueFormat2 = 0`.
/// `x_advances` is the class matrix in row-major order, `class_1_count × class_2_count` entries;
/// the matrix follows the header directly, the coverage and ClassDefs after it.
fn pair_pos_2(covered: &[u16], class_def_1: &[u8], class_def_2: &[u8],
              class_1_count: u16, class_2_count: u16, x_advances: &[i16]) -> Vec<u8> {
    let coverage = coverage_1(covered);
    let header_len = 16;
    let class_1_records_off = header_len;
    let coverage_off = class_1_records_off + 2 * x_advances.len();
    let class_def_1_off = coverage_off + coverage.len();
    let class_def_2_off = class_def_1_off + class_def_1.len();

    let mut t = Vec::new();
    push_u16(&mut t, 2);                        // format
    push_u16(&mut t, coverage_off as u16);
    push_u16(&mut t, 0x0004);                   // valueFormat1
    push_u16(&mut t, 0);                        // valueFormat2
    push_u16(&mut t, class_def_1_off as u16);
    push_u16(&mut t, class_def_2_off as u16);
    push_u16(&mut t, class_1_count);
    push_u16(&mut t, class_2_count);
    assert_eq!(t.len(), class_1_records_off);
    for &x_advance in x_advances { push_i16(&mut t, x_advance); }
    t.extend_from_slice(&coverage);
    t.extend_from_slice(class_def_1);
    t.extend_from_slice(class_def_2);
    t
}

fn kerning_of(data: &[u8], left: u32, right: u32) -> f32 {
    let font = OpenTypeFont::parse(data).expect("font parses");
    font.kerning(GlyphId(left), GlyphId(right))
}

// ---------------------------------------------------------------------------------------------
// F3/F4/F5/F27 — GDEF: tag case, glyphClassDef vs markAttachClassDef, optional subtables

#[test]
fn c01_gdef_is_read_at_all() {
    // F3: the table tag is `GDEF`, not `gdef`.
    let mut tables = base_tables(8);
    tables.push((*b"GDEF", gdef_table(Some(class_def_1(1, &[1, 2, 3, 4])), None)));
    let data = build_font(tables);
    let gdef = parse_gdef_of(&data);
    assert_eq!(gdef.mark_class(1), Some(MarkClass::Base));
    assert_eq!(gdef.mark_class(2), Some(MarkClass::Ligature));
    assert_eq!(gdef.mark_class(3), Some(MarkClass::Mark));
    assert_eq!(gdef.mark_class(4), Some(MarkClass::Component));
}

#[test]
fn c02_gdef_without_optional_tables_parses() {
    // F5/F27: both class definitions are optional (offset 0), which used to panic.
    let mut tables = base_tables(8);
    tables.push((*b"GDEF", gdef_table(None, None)));
    let data = build_font(tables);
    let gdef = parse_gdef_of(&data);
    assert_eq!(gdef.mark_class(1), None);
}

#[test]
fn c03_gdef_glyph_class_is_not_the_mark_attach_class() {
    // F4: `mark_class()` must read glyphClassDef; markAttachClassDef is a different table.
    let mut tables = base_tables(8);
    let glyph = class_def_1(5, &[2]);       // gid 5 -> ligature
    let mark_attach = class_def_1(5, &[3]); // gid 5 -> mark class 3
    tables.push((*b"GDEF", gdef_table(Some(glyph), Some(mark_attach))));
    let data = build_font(tables);
    let gdef = parse_gdef_of(&data);
    assert_eq!(gdef.mark_class(5), Some(MarkClass::Ligature));
    assert_eq!(gdef.mark_attach_class(5), Some(3));
}

// ---------------------------------------------------------------------------------------------
// F1/F2 — GPOS PairPos

#[test]
fn c04_pair_pos_format_1_kerning() {
    // F1: the format 1 header is format, coverageOffset, valueFormat1, valueFormat2,
    // pairSetCount, pairSetOffsets[] — there is no extra field before the offsets.
    let mut tables = base_tables(8);
    tables.push((*b"GPOS", gpos_table(2, &pair_pos_1(1, &[(2, -50), (3, -20)]))));
    let data = build_font(tables);
    assert_eq!(kerning_of(&data, 1, 2), -50.0);
    assert_eq!(kerning_of(&data, 1, 3), -20.0);
    assert_eq!(kerning_of(&data, 1, 4), 0.0);
}

#[test]
fn c05_pair_pos_format_2_class_kerning() {
    // F2: format 2 indexes a class matrix through two ClassDefs; without parsing them the
    // matrix can never be read and class kerning is always 0.
    let mut tables = base_tables(8);
    let subtable = pair_pos_2(
        &[1],
        &class_def_1(1, &[1]),      // gid 1 -> left class 1
        &class_def_1(2, &[1]),      // gid 2 -> right class 1
        2, 2,
        &[0, 0, 0, -40],            // matrix[1][1]
    );
    tables.push((*b"GPOS", gpos_table(2, &subtable)));
    let data = build_font(tables);
    assert_eq!(kerning_of(&data, 1, 2), -40.0);
    assert_eq!(kerning_of(&data, 1, 3), 0.0);
}

// ---------------------------------------------------------------------------------------------
// F13 — CMap::codepoints(): the last range, the merge loop

/// A `cmap` (§cmap) with a single Unicode (platform 3, encoding 1) format 4 subtable.
/// `segments` are `(first_code, last_code, first_glyph)` with consecutive glyph ids.
pub fn cmap_format4(segments: &[(u16, u16, u16)]) -> Vec<u8> {
    cmap_multi(&[(3, 1, format4_subtable(segments))])
}

/// A format 4 subtable (§cmap format 4) for `(first_code, last_code, first_glyph)` segments,
/// mapped through `idDelta` with consecutive glyph ids.
fn format4_subtable(segments: &[(u16, u16, u16)]) -> Vec<u8> {
    let seg_count = segments.len() as u16 + 1; // + the 0xFFFF terminator
    let mut sub = Vec::new();
    push_u16(&mut sub, 4);              // format
    let len_pos = sub.len();
    push_u16(&mut sub, 0);              // length, patched below
    push_u16(&mut sub, 0);              // language
    push_u16(&mut sub, seg_count * 2);  // segCountX2
    push_u16(&mut sub, 4);              // searchRange
    push_u16(&mut sub, 0);              // entrySelector
    push_u16(&mut sub, 0);              // rangeShift (the crate recomputes nothing)
    for &(_, last, _) in segments { push_u16(&mut sub, last); }
    push_u16(&mut sub, 0xFFFF);         // endCode terminator
    push_u16(&mut sub, 0);              // reservedPad
    for &(first, _, _) in segments { push_u16(&mut sub, first); }
    push_u16(&mut sub, 0xFFFF);         // startCode terminator
    for &(first, _, start_gid) in segments { push_u16(&mut sub, start_gid.wrapping_sub(first)); }
    push_u16(&mut sub, 0);              // idDelta terminator
    for _ in 0 .. seg_count { push_u16(&mut sub, 0); } // idRangeOffset[]: use idDelta
    let len = sub.len() as u16;
    sub[len_pos .. len_pos + 2].copy_from_slice(&be16(len));
    sub
}

/// A `cmap` with one encoding record per `(platform, encoding, subtable)`.
fn cmap_multi(records: &[(u16, u16, Vec<u8>)]) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 0);                        // version
    push_u16(&mut t, records.len() as u16);
    let mut offset = 4 + 8 * records.len();
    for (platform, encoding, sub) in records {
        push_u16(&mut t, *platform);
        push_u16(&mut t, *encoding);
        push_u32(&mut t, offset as u32);
        offset += sub.len();
    }
    for (_, _, sub) in records { t.extend_from_slice(sub); }
    t
}

fn codepoints_of(data: &[u8], max_ranges: usize) -> Vec<std::ops::RangeInclusive<u32>> {
    parse_cmap(data).expect("cmap parses").0.codepoints(max_ranges)
}

#[test]
fn c10_codepoints_keeps_the_last_range() {
    // F13: the range being built at the end of the walk is never pushed, so a contiguous
    // cmap yields an empty list.
    let got = codepoints_of(&cmap_format4(&[(0x41, 0x43, 1)]), 10);
    assert_eq!(got, vec![0x41 ..= 0x43]);
}

#[test]
fn c10b_codepoints_merges_ranges_down_to_max_ranges() {
    // F13: the simplify loop dropped `prev` and computed gaps as `b.start - a.end`.
    let data = cmap_format4(&[(0x41, 0x42, 1), (0x61, 0x62, 3)]);
    assert_eq!(codepoints_of(&data, 2), vec![0x41 ..= 0x42, 0x61 ..= 0x62]);
    assert_eq!(codepoints_of(&data, 1), vec![0x41 ..= 0x62]);
}

#[test]
fn c10c_codepoints_with_zero_max_ranges_does_not_panic() {
    // F13: `tuple_windows().min().unwrap()` panicked when nothing was left to merge.
    let got = codepoints_of(&cmap_format4(&[(0x41, 0x43, 1)]), 0);
    assert_eq!(got, vec![0x41 ..= 0x43]);
}

// ---------------------------------------------------------------------------------------------
// F12 — an unsupported cmap subtable must not hide the supported ones

#[test]
fn c11_unsupported_format_does_not_hide_other_subtables() {
    // OT §cmap: subtables of different formats coexist; a reader skips the ones it does not
    // implement.  Format 2 (high-byte mapping, CJK legacy) comes first here.
    let format2 = be16(2).to_vec(); // header only: the crate never gets as far as the body
    let data = cmap_multi(&[
        (0, 3, format2),
        (3, 1, format4_subtable(&[(0x41, 0x43, 1)])),
    ]);
    assert_eq!(codepoints_of(&data, 10), vec![0x41 ..= 0x43]);
}

#[test]
fn c11b_unsupported_format_alone_yields_an_empty_map() {
    // Skipping, not aborting: a font whose only subtable is format 2 still parses, with no
    // codepoints, instead of failing to load.
    let data = cmap_multi(&[(3, 1, be16(2).to_vec())]);
    assert!(codepoints_of(&data, 10).is_empty());
}

// ---------------------------------------------------------------------------------------------
// F15 — `post` version 1.0 is the standard Macintosh order

/// `post` (OT §post) header, 32 bytes, up to and including version 3.0.
pub fn post_table(version: u32) -> Vec<u8> {
    let mut t = Vec::new();
    push_u32(&mut t, version);
    push_u32(&mut t, 0);        // italicAngle
    push_i16(&mut t, 0);        // underlinePosition
    push_i16(&mut t, 0);        // underlineThickness
    push_u32(&mut t, 0);        // isFixedPitch
    for _ in 0 .. 4 { push_u32(&mut t, 0); } // min/maxMemType42, min/maxMemType1
    assert_eq!(t.len(), 32);
    t
}

#[test]
fn c12_post_version_1_uses_the_standard_mac_order() {
    // F15: version 1.0 has no glyphNameIndex array; the names *are* the 258 standard
    // Macintosh names, in order.
    let table = post_table(0x0001_0000);
    let post = font::opentype::post::parse_post(&table).expect("post parses");
    assert_eq!(post.names.len(), 258);
    assert_eq!(post.names[0], ".notdef");
    assert_eq!(post.names[3], "space");
    assert_eq!(post.names[36], "A");
}

#[test]
fn c12b_post_version_3_has_no_names() {
    // Version 3.0 means "no glyph names"; an empty list is correct and must stay that way.
    let table = post_table(0x0003_0000);
    let post = font::opentype::post::parse_post(&table).expect("post parses");
    assert!(post.names.is_empty());
}

/// `kern` version 0 (OT/MS §kern) with one subtable; `data` is the body after the 6-byte header.
pub fn kern_table_ms(format: u8, coverage: u8, data: &[u8]) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 0);                        // version
    push_u16(&mut t, 1);                        // nTables
    push_u16(&mut t, 0);                        // subtable version
    push_u16(&mut t, (6 + data.len()) as u16);  // length, header included
    t.push(format);
    t.push(coverage);
    t.extend_from_slice(data);
    t
}

/// Format 0 body: `n_pairs`, the (unused) search fields, then `(left, right, value)`.
pub fn kern_pairs(pairs: &[(u16, u16, i16)]) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, pairs.len() as u16);
    push_u16(&mut t, 0); // searchRange
    push_u16(&mut t, 0); // entrySelector
    push_u16(&mut t, 0); // rangeShift
    for &(left, right, value) in pairs {
        push_u16(&mut t, left);
        push_u16(&mut t, right);
        push_i16(&mut t, value);
    }
    t
}

/// `loca` with `indexToLocFormat = 0` (OT §loca): every offset is stored halved.
pub fn loca_table_short(offsets: &[u32]) -> Vec<u8> {
    let mut t = Vec::new();
    for &off in offsets {
        assert_eq!(off % 2, 0, "short loca stores offsets halved");
        push_u16(&mut t, (off / 2) as u16);
    }
    t
}

#[test]
fn c13_hmtx_rejects_zero_number_of_hmetrics() {
    // F30: OT §hmtx requires `1 <= numberOfHMetrics <= numGlyphs`. With 0 the crate used to
    // build an empty metric list and answer every glyph with advance 0.
    let mut tables = base_tables(2);
    tables[1].1 = hhea_table(0);
    let err = OpenTypeFont::parse(&build_font(tables));
    assert!(err.is_err(), "numberOfHMetrics = 0 must be rejected");
}

#[test]
fn c13b_hmtx_rejects_missing_trailing_lsbs() {
    // F30: `numGlyphs - numberOfHMetrics` left side bearings must be there; `many0` used to
    // accept a short table and fill in 0.
    let mut tables = base_tables(3);
    tables[1].1 = hhea_table(2);
    tables[3].1 = hmtx_table(2);
    let err = OpenTypeFont::parse(&build_font(tables));
    assert!(err.is_err(), "a short left-side-bearing array must be rejected");
}

#[test]
fn c13c_hmtx_has_no_metric_for_gid_at_or_beyond_num_glyphs() {
    // F30: `metrics_for_gid` used to invent (last_advance, 0) for any gid.
    let tables = base_tables(2);
    let font = OpenTypeFont::parse(&build_font(tables)).expect("font parses");
    assert!(font.glyph_metrics(2).is_none());
    assert_eq!(font.glyph_metrics(1).map(|m| m.advance), Some(500.0));
}

#[test]
fn c14_head_rejects_units_per_em_zero() {
    // F31: OT §head allows 16..=16384; 0 turned `font_matrix()` into `1/0 = inf`.
    let mut head = head_table();
    head[18] = 0;
    head[19] = 0;
    let mut tables = base_tables(2);
    tables[0].1 = head;
    assert!(OpenTypeFont::parse(&build_font(tables)).is_err());
}

#[test]
fn c14b_maxp_rejects_unknown_version() {
    // F31: OT §maxp has exactly 0x00005000 and 0x00010000.
    let mut maxp = maxp_table(2);
    maxp[1] = 2; // 0x00020000
    let mut tables = base_tables(2);
    tables[2].1 = maxp;
    assert!(OpenTypeFont::parse(&build_font(tables)).is_err());
}

#[test]
fn c15_kern_horizontal_subtable_may_also_be_minimum() {
    // F32: coverage is a bit field. 0x03 = horizontal + minimum, which is legal and still
    // applies; the crate required exactly 0x01 and failed the whole font.
    let mut tables = base_tables(2);
    tables.push((*b"kern", kern_table_ms(0, 0x03, &kern_pairs(&[(0, 1, -50)]))));
    let font = OpenTypeFont::parse(&build_font(tables)).expect("kern coverage 0x03 must load");
    assert_eq!(font.kerning(GlyphId(0), GlyphId(1)), -50.0);
}

#[test]
fn c15b_vertical_kern_subtable_is_skipped_not_fatal() {
    // F32: coverage bit 0 clear means vertical; this crate has no vertical kerning, so the
    // subtable is skipped. It must not take the font down.
    let mut tables = base_tables(2);
    tables.push((*b"kern", kern_table_ms(0, 0x00, &kern_pairs(&[(0, 1, -50)]))));
    let font = OpenTypeFont::parse(&build_font(tables)).expect("vertical kern must load");
    assert_eq!(font.kerning(GlyphId(0), GlyphId(1)), 0.0);
}

#[test]
fn c16_one_corrupt_glyph_does_not_fail_the_font() {
    // F31: a bad `glyf` record made `parse_shapes` return `Err` for the whole font; OT §glyf
    // treats a glyph without contours as empty, and one broken glyph is just one empty glyph.
    let mut glyf = Vec::new();
    push_i16(&mut glyf, -5);        // numberOfContours: not a valid value
    push_i16(&mut glyf, 0);         // xMin
    push_i16(&mut glyf, 0);         // yMin
    push_i16(&mut glyf, 100);       // xMax
    push_i16(&mut glyf, 100);       // yMax
    let end = glyf.len() as u32;
    let loca = loca_table_short(&[0, 0, end]);
    let mut tables = base_tables(2);
    tables.push((*b"loca", loca));
    tables.push((*b"glyf", glyf));
    let font = OpenTypeFont::parse(&build_font(tables)).expect("one bad glyph must not fail the font");
    assert_eq!(font.num_glyphs(), 2);
    assert!(font.is_empty_glyph(GlyphId(1)));
}

#[test]
fn c16b_loca_must_be_monotonic() {
    // F31: OT §loca offsets are non-decreasing; a decreasing pair used to fail deep inside
    // `parse_shapes` with an out-of-bounds message.
    let mut tables = base_tables(2);
    tables.push((*b"loca", loca_table_short(&[0, 10, 4])));
    let mut glyf = vec![0u8; 10];
    glyf[1] = 0; // shape data is irrelevant, the offsets are wrong
    tables.push((*b"glyf", glyf));
    assert!(OpenTypeFont::parse(&build_font(tables)).is_err());
}

#[test]
fn c16c_loca_may_not_point_past_glyf() {
    // F31: the last offset is the length of `glyf`.
    let mut tables = base_tables(2);
    tables.push((*b"loca", loca_table_short(&[0, 0, 100])));
    tables.push((*b"glyf", vec![0u8; 10]));
    assert!(OpenTypeFont::parse(&build_font(tables)).is_err());
}
