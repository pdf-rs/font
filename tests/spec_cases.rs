//! Minimal in-memory fonts, one per specification mismatch.
//!
//! Each case builds the smallest sfnt that still reaches the code under test, so a failure names
//! exactly one rule. Run everything: `cargo test`; one group: `cargo test c01`.

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
// GPOS PairPos: the format 1 header, and format 2 class definitions

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
// GDEF: tag case, glyphClassDef vs markAttachClassDef, optional subtables

#[test]
fn c01_gdef_is_read_at_all() {
    // the table tag is `GDEF`, not `gdef`.
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
    // both class definitions are optional (offset 0), which used to panic.
    let mut tables = base_tables(8);
    tables.push((*b"GDEF", gdef_table(None, None)));
    let data = build_font(tables);
    let gdef = parse_gdef_of(&data);
    assert_eq!(gdef.mark_class(1), None);
}

#[test]
fn c03_gdef_glyph_class_is_not_the_mark_attach_class() {
    // `mark_class()` must read glyphClassDef; markAttachClassDef is a different table.
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
// GPOS PairPos

#[test]
fn c04_pair_pos_format_1_kerning() {
    // the format 1 header is format, coverageOffset, valueFormat1, valueFormat2,
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
    // format 2 indexes a class matrix through two ClassDefs; without parsing them the
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
// CMap::codepoints(): the last range, the merge loop

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

/// A format 4 subtable that maps through `idRangeOffset` (the glyph index array) instead of
/// `idDelta`. `segments` are `(first_code, last_code, first entry in `glyphs`)`; the offsets are
/// computed the way §cmap defines them — relative to `&idRangeOffset[i]`, whose index is `n`.
fn format4_subtable_with_glyph_array(segments: &[(u16, u16, usize)], glyphs: &[u16]) -> Vec<u8> {
    let seg_count = segments.len() as u16 + 1; // + the 0xFFFF terminator
    let mut sub = Vec::new();
    push_u16(&mut sub, 4);              // format
    let len_pos = sub.len();
    push_u16(&mut sub, 0);              // length, patched below
    push_u16(&mut sub, 0);              // language
    push_u16(&mut sub, seg_count * 2);  // segCountX2
    push_u16(&mut sub, 4);              // searchRange
    push_u16(&mut sub, 0);              // entrySelector
    push_u16(&mut sub, 0);              // rangeShift
    for &(_, last, _) in segments { push_u16(&mut sub, last); }
    push_u16(&mut sub, 0xFFFF);         // endCode terminator
    push_u16(&mut sub, 0);              // reservedPad
    for &(first, _, _) in segments { push_u16(&mut sub, first); }
    push_u16(&mut sub, 0xFFFF);         // startCode terminator
    for _ in 0 .. seg_count { push_u16(&mut sub, 0); } // idDelta[]: no delta, glyphs as stored
    for (n, &(_, _, glyph_start)) in segments.iter().enumerate() {
        // bytes from &idRangeOffset[n] to &glyphs[glyph_start]
        let offset = 2 * seg_count as i32 + 2 * glyph_start as i32 - 2 * n as i32;
        push_u16(&mut sub, offset as u16);
    }
    push_u16(&mut sub, 0);              // idRangeOffset terminator
    for &gid in glyphs { push_u16(&mut sub, gid); }
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

/// A `cmap` with one Unicode (platform 3, encoding 10) format 12 subtable mapping each of
/// `codes` to its own glyph id. Groups are one code wide, so every code is its own run.
fn cmap_format12(codes: &[u32]) -> Vec<u8> {
    let mut sub = Vec::new();
    push_u16(&mut sub, 12);                           // format
    push_u16(&mut sub, 0);                            // reserved
    push_u32(&mut sub, 16 + 12 * codes.len() as u32); // length
    push_u32(&mut sub, 0);                            // language
    push_u32(&mut sub, codes.len() as u32);           // numGroups
    for (ix, code) in codes.iter().enumerate() {
        push_u32(&mut sub, *code);            // startCharCode
        push_u32(&mut sub, *code);            // endCharCode
        push_u32(&mut sub, ix as u32 + 1);    // startGlyphID
    }
    cmap_multi(&[(3, 10, sub)])
}

#[test]
fn c10_codepoints_keeps_the_last_range() {
    // the range being built at the end of the walk is never pushed, so a contiguous
    // cmap yields an empty list.
    let got = codepoints_of(&cmap_format4(&[(0x41, 0x43, 1)]), 10);
    assert_eq!(got, vec![0x41 ..= 0x43]);
}

#[test]
fn c10b_codepoints_merges_ranges_down_to_max_ranges() {
    // the simplify loop dropped `prev` and computed gaps as `b.start - a.end`.
    let data = cmap_format4(&[(0x41, 0x42, 1), (0x61, 0x62, 3)]);
    assert_eq!(codepoints_of(&data, 2), vec![0x41 ..= 0x42, 0x61 ..= 0x62]);
    assert_eq!(codepoints_of(&data, 1), vec![0x41 ..= 0x62]);
}

#[test]
fn c10c_codepoints_with_zero_max_ranges_does_not_panic() {
    // `tuple_windows().min().unwrap()` panicked when nothing was left to merge.
    let got = codepoints_of(&cmap_format4(&[(0x41, 0x43, 1)]), 0);
    assert_eq!(got, vec![0x41 ..= 0x43]);
}

#[test]
fn c10d_codepoints_reduces_many_scattered_runs_quickly() {
    // the merge loop rescanned every window of the range list and spliced once per
    // merge, which is O(n²) on a font whose codepoints are scattered (large CJK fonts).
    // Closing the widest gaps last means one sort of the gaps, whatever their number.
    // Format 12: one group per code, twenty thousand codes, every gap the same width.
    let codes: Vec<u32> = (0 .. 20_000u32).map(|ix| 0x100 + ix * 2).collect();
    let data = cmap_format12(&codes);
    let started = std::time::Instant::now();
    let got = codepoints_of(&data, 10);
    let elapsed = started.elapsed();
    let covered: u32 = got.iter().map(|r| r.end() - r.start() + 1).sum();
    assert_eq!(got.len(), 10, "reduced to max_ranges");
    assert_eq!(got[0], 0x100 ..= 0x100, "the wide gaps are the ones kept open");
    assert_eq!(covered, 39_990, "every code is covered, and the merged tail spans the gaps");
    assert_eq!(*got.last().unwrap().end(), 0x100 + 19_999 * 2, "the tail is one range");
    assert!(elapsed < std::time::Duration::from_secs(2), "took {:?}", elapsed);
}


// ---------------------------------------------------------------------------------------------
// cmap format 4: the glyph index array is addressed relative to &idRangeOffset[i]

#[test]
fn c14c_cmap_format4_reads_the_glyph_index_array() {
    // the index was computed in u16, so a malformed offset silently wrapped (release) or
    // panicked (debug).  Segment 1 has n = 1, which pins both the `2 * n` term and the fact
    // that the offset is relative to &idRangeOffset[n], not to the glyph array.
    let sub = format4_subtable_with_glyph_array(
        &[(0x41, 0x41, 0), (0x50, 0x51, 1)],
        &[10, 20, 21],
    );
    let (cmap, _encoding) = parse_cmap(&cmap_multi(&[(3, 1, sub)])).expect("cmap parses");
    assert_eq!(cmap.get_codepoint(0x41), Some(10), "segment 0 reads glyphs[0]");
    assert_eq!(cmap.get_codepoint(0x50), Some(20), "segment 1 reads glyphs[1]");
    assert_eq!(cmap.get_codepoint(0x51), Some(21), "and advances by 2 per code");
    assert_eq!(cmap.get_codepoint(0x42), None, "code outside every segment");
}

#[test]
fn c14d_cmap_format4_glyph_index_past_the_array_is_dropped() {
    // an idRangeOffset pointing past the end of the subtable must stop the segment, not
    // panic in `glyph_data.len() - 2` or read a neighbouring byte as a glyph index.
    let sub = format4_subtable_with_glyph_array(&[(0x41, 0x41, 0)], &[]);
    let (cmap, _encoding) = parse_cmap(&cmap_multi(&[(3, 1, sub)])).expect("cmap parses");
    assert_eq!(cmap.get_codepoint(0x41), None);
}

#[test]
fn c14e_cmap_format4_offset_before_the_array_is_dropped() {
    // `... + offset - segCountX2` in u16 wrapped when offset < segCountX2 (a truncated
    // font), pointing the read at the header.  It must drop the segment instead.
    let mut sub = format4_subtable_with_glyph_array(&[(0x41, 0x41, 0)], &[10]);
    // header (7 u16) + endCode[] + reservedPad + startCode[] + idDelta[], 2 segments each
    let offsets = 2 * (7 + 2 + 1 + 2 + 2);
    sub[offsets .. offsets + 2].copy_from_slice(&be16(2)); // < segCountX2 = 4
    let (cmap, _encoding) = parse_cmap(&cmap_multi(&[(3, 1, sub)])).expect("cmap parses");
    assert_eq!(cmap.get_codepoint(0x41), None);
}


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
// `post` version 1.0 is the standard Macintosh order

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
    // version 1.0 has no glyphNameIndex array; the names *are* the 258 standard
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
fn c13_hmtx_clamps_zero_number_of_hmetrics() {
    // OT §hmtx requires `1 <= numberOfHMetrics <= numGlyphs`. A value outside that range
    // cannot make the metric lookups fail, so it is clamped with a warning rather than
    // costing the whole font — 0 used to reject.
    let mut tables = base_tables(2);
    tables[1].1 = hhea_table(0);
    let font = OpenTypeFont::parse(&build_font(tables)).expect("numberOfHMetrics = 0 loads");
    assert_eq!(font.glyph_metrics(0).map(|m| m.advance), Some(500.0));
}

#[test]
fn c13b_hmtx_accepts_missing_trailing_lsbs() {
    // `numGlyphs - numberOfHMetrics` left side bearings are required by the spec, but
    // truncated tables are common: the missing ones are left out, and the glyphs
    // whose bearing is missing report no metric.
    let mut tables = base_tables(3);
    tables[1].1 = hhea_table(2);
    tables[3].1 = hmtx_table(2);
    let font = OpenTypeFont::parse(&build_font(tables)).expect("a short bearing array loads");
    assert_eq!(font.glyph_metrics(0).map(|m| m.advance), Some(500.0));
    assert!(font.glyph_metrics(2).is_none(), "glyph 2 has no bearing of its own");
}

#[test]
fn c13c_hmtx_has_no_metric_for_gid_at_or_beyond_num_glyphs() {
    // `metrics_for_gid` used to invent (last_advance, 0) for any gid.
    let tables = base_tables(2);
    let font = OpenTypeFont::parse(&build_font(tables)).expect("font parses");
    assert!(font.glyph_metrics(2).is_none());
    assert_eq!(font.glyph_metrics(1).map(|m| m.advance), Some(500.0));
}

#[test]
fn c14_head_rejects_units_per_em_zero() {
    // OT §head allows 16..=16384; 0 turned `font_matrix()` into `1/0 = inf`, which is the
    // one value that stays fatal.
    let mut head = head_table();
    head[18] = 0;
    head[19] = 0;
    let mut tables = base_tables(2);
    tables[0].1 = head;
    assert!(OpenTypeFont::parse(&build_font(tables)).is_err());
}

#[test]
fn c14c_head_keeps_a_units_per_em_outside_the_spec_range() {
    // the rest of the range is a convention, not a reason to refuse the font —
    // clamping would move every coordinate.
    let mut head = head_table();
    head[18] = 0;
    head[19] = 8;
    let mut tables = base_tables(2);
    tables[0].1 = head;
    let font = OpenTypeFont::parse(&build_font(tables)).expect("unitsPerEm = 8 loads");
    assert_eq!(font.num_glyphs(), 2);
    assert_eq!(font.glyph_metrics(0).map(|m| m.advance), Some(500.0));
}

#[test]
fn c14b_maxp_reads_an_unknown_version() {
    // OT §maxp has exactly 0x00005000 and 0x00010000. The fields are read at the
    // same offsets in both, so an unknown version is a warning.
    let mut maxp = maxp_table(2);
    maxp[1] = 2; // 0x00020000
    let mut tables = base_tables(2);
    tables[2].1 = maxp;
    let font = OpenTypeFont::parse(&build_font(tables)).expect("maxp 2.0 loads");
    assert_eq!(font.num_glyphs(), 2);
}

#[test]
fn c15_kern_horizontal_subtable_may_also_be_minimum() {
    // coverage is a bit field. 0x03 = horizontal + minimum, which is legal and still
    // applies; the crate required exactly 0x01 and failed the whole font.
    let mut tables = base_tables(2);
    tables.push((*b"kern", kern_table_ms(0, 0x03, &kern_pairs(&[(0, 1, -50)]))));
    let font = OpenTypeFont::parse(&build_font(tables)).expect("kern coverage 0x03 must load");
    assert_eq!(font.kerning(GlyphId(0), GlyphId(1)), -50.0);
}

#[test]
fn c15b_vertical_kern_subtable_is_skipped_not_fatal() {
    // coverage bit 0 clear means vertical; this crate has no vertical kerning, so the
    // subtable is skipped. It must not take the font down.
    let mut tables = base_tables(2);
    tables.push((*b"kern", kern_table_ms(0, 0x00, &kern_pairs(&[(0, 1, -50)]))));
    let font = OpenTypeFont::parse(&build_font(tables)).expect("vertical kern must load");
    assert_eq!(font.kerning(GlyphId(0), GlyphId(1)), 0.0);
}

#[test]
fn c16_one_corrupt_glyph_does_not_fail_the_font() {
    // a bad `glyf` record made `parse_shapes` return `Err` for the whole font; OT §glyf
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
fn c16b_loca_that_runs_backwards_leaves_the_glyph_empty() {
    // OT §loca offsets are non-decreasing. A backwards pair can only affect
    // that glyph, which `parse_shapes` reads as empty, so the font still loads.
    let mut tables = base_tables(2);
    tables.push((*b"loca", loca_table_short(&[0, 10, 4])));
    let mut glyf = vec![0u8; 10];
    glyf[1] = 0; // shape data is irrelevant, the offsets are wrong
    tables.push((*b"glyf", glyf));
    let font = OpenTypeFont::parse(&build_font(tables)).expect("a backwards loca pair loads");
    assert_eq!(font.num_glyphs(), 2);
    assert!(font.is_empty_glyph(GlyphId(1)));
}

#[test]
fn c16c_loca_past_the_end_of_glyf_leaves_those_glyphs_empty() {
    // the last offset is the length of `glyf`. Only the glyphs past the end
    // are affected, so the font still loads.
    let mut tables = base_tables(2);
    tables.push((*b"loca", loca_table_short(&[0, 0, 100])));
    tables.push((*b"glyf", vec![0u8; 10]));
    let font = OpenTypeFont::parse(&build_font(tables)).expect("an oversized last loca loads");
    assert_eq!(font.num_glyphs(), 2);
    assert!(font.is_empty_glyph(GlyphId(0)));
    assert!(font.is_empty_glyph(GlyphId(1)));
}

// ---------------------------------------------------------------------------------------------
// glyf / loca: simple glyphs, composite glyphs and their placement

/// A simple glyph record (OT §glyf) with one contour through `points`, all points on-curve.
pub fn simple_glyph(points: &[(i16, i16)]) -> Vec<u8> {
    let mut t = Vec::new();
    push_i16(&mut t, 1);                        // numberOfContours
    push_i16(&mut t, 0);                        // xMin
    push_i16(&mut t, 0);                        // yMin
    push_i16(&mut t, 1000);                     // xMax
    push_i16(&mut t, 1000);                     // yMax
    push_u16(&mut t, points.len() as u16 - 1);  // endPtsOfContours[0]
    push_u16(&mut t, 0);                        // instructionLength
    for _ in points { t.push(0x01); }           // ON_CURVE, i16 deltas
    let (mut px, mut py) = (0i16, 0i16);
    for &(x, _) in points { push_i16(&mut t, x - px); px = x; }
    for &(_, y) in points { push_i16(&mut t, y - py); py = y; }
    t
}

/// How one component of a composite glyph is placed (OT §glyf).
#[derive(Copy, Clone, Debug)]
pub enum Placement {
    /// `ARGS_ARE_XY_VALUES`: an x/y offset.
    Offset(i16, i16),
    /// Point matching: `parent` in the composite so far, `child` in the component.
    Points(u16, u16),
}

/// A composite glyph record (OT §glyf), every argument stored as a word.
pub fn composite_glyph(components: &[(u16, Placement)]) -> Vec<u8> {
    let mut t = Vec::new();
    push_i16(&mut t, -1);                       // numberOfContours
    for _ in 0 .. 4 { push_i16(&mut t, 0); }    // xMin, yMin, xMax, yMax
    for (n, &(gid, placement)) in components.iter().enumerate() {
        let mut flags = 0x0001;                 // ARG_1_AND_2_ARE_WORDS
        if n + 1 < components.len() { flags |= 0x0020; }  // MORE_COMPONENTS
        if let Placement::Offset(..) = placement { flags |= 0x0002; }
        push_u16(&mut t, flags);
        push_u16(&mut t, gid);
        match placement {
            Placement::Offset(dx, dy) => { push_i16(&mut t, dx); push_i16(&mut t, dy); }
            Placement::Points(parent, child) => { push_u16(&mut t, parent); push_u16(&mut t, child); }
        }
    }
    t
}

/// A composite glyph record (OT §glyf) whose components all carry a uniform `WE_HAVE_A_SCALE`
/// transform (F2Dot14, 0x2000 = 0.5) plus the flag bits in `extra_flags`.
pub fn composite_glyph_scaled(components: &[(u16, Placement)], scale: i16, extra_flags: u16) -> Vec<u8> {
    let mut t = Vec::new();
    push_i16(&mut t, -1);                       // numberOfContours
    for _ in 0 .. 4 { push_i16(&mut t, 0); }    // xMin, yMin, xMax, yMax
    for (n, &(gid, placement)) in components.iter().enumerate() {
        let mut flags = 0x0001 | 0x0008 | extra_flags; // words, WE_HAVE_A_SCALE
        if n + 1 < components.len() { flags |= 0x0020; }  // MORE_COMPONENTS
        if let Placement::Offset(..) = placement { flags |= 0x0002; }
        push_u16(&mut t, flags);
        push_u16(&mut t, gid);
        match placement {
            Placement::Offset(dx, dy) => { push_i16(&mut t, dx); push_i16(&mut t, dy); }
            Placement::Points(parent, child) => { push_u16(&mut t, parent); push_u16(&mut t, child); }
        }
        push_i16(&mut t, scale);
    }
    t
}

/// Assemble `glyf` and a matching short `loca` from the glyph records, each padded to 2 bytes.
pub fn glyf_and_loca(glyphs: &[Vec<u8>]) -> (Vec<u8>, Vec<u8>) {
    let mut glyf = Vec::new();
    let mut offsets = vec![0u32];
    for glyph in glyphs {
        glyf.extend_from_slice(glyph);
        while glyf.len() % 2 != 0 { glyf.push(0); }
        offsets.push(glyf.len() as u32);
    }
    (glyf, loca_table_short(&offsets))
}

/// A TrueType font carrying exactly these glyph records.
pub fn font_with_glyphs(glyphs: &[Vec<u8>]) -> OpenTypeFont {
    let (glyf, loca) = glyf_and_loca(glyphs);
    let mut tables = base_tables(glyphs.len() as u16);
    tables.push((*b"loca", loca));
    tables.push((*b"glyf", glyf));
    OpenTypeFont::parse(&build_font(tables)).expect("font parses")
}

/// Every point of a glyph's outline, in contour order, without the closing point that
/// `contour()` appends (so the indices are the ones OT §glyf uses).
pub fn outline_points(font: &OpenTypeFont, gid: u16) -> Vec<(f32, f32)> {
    let glyph = font.glyph(GlyphId(gid as u32)).expect("glyph exists");
    let mut out = Vec::new();
    for contour in glyph.path.contours() {
        let points = contour.points();
        let points = if points.len() > 1 && points[0] == points[points.len() - 1] {
            &points[.. points.len() - 1]
        } else {
            points
        };
        for point in points {
            out.push((point.x(), point.y()));
        }
    }
    out
}

#[test]
fn c17a_composite_point_matching() {
    // without ARGS_ARE_XY_VALUES the arguments are point numbers, not offsets. The
    // second copy of gid 1 must move so that its point 0 lands on point 2 of the first copy.
    let font = font_with_glyphs(&[
        Vec::new(),
        simple_glyph(&[(0, 0), (100, 0), (0, 100)]),
        composite_glyph(&[(1, Placement::Offset(0, 0)), (1, Placement::Points(2, 0))]),
    ]);
    assert_eq!(
        outline_points(&font, 2),
        vec![(0.0, 0.0), (100.0, 0.0), (0.0, 100.0),
             (0.0, 100.0), (100.0, 100.0), (0.0, 200.0)]
    );
}

#[test]
fn c17b_nested_composite_glyphs_are_expanded() {
    // a composite may reference another composite.
    let font = font_with_glyphs(&[
        Vec::new(),
        simple_glyph(&[(0, 0), (100, 0), (0, 100)]),
        composite_glyph(&[(1, Placement::Offset(10, 0))]),
        composite_glyph(&[(2, Placement::Offset(0, 20))]),
    ]);
    assert_eq!(
        outline_points(&font, 3),
        vec![(10.0, 20.0), (110.0, 20.0), (10.0, 120.0)]
    );
}

#[test]
fn c17c_self_referencing_composite_terminates() {
    // OT §glyf forbids a glyph that contains itself; reading one must not recurse forever.
    let font = font_with_glyphs(&[
        Vec::new(),
        simple_glyph(&[(0, 0), (100, 0), (0, 100)]),
        composite_glyph(&[(2, Placement::Offset(0, 0))]),
    ]);
    assert!(outline_points(&font, 2).is_empty());
}

#[test]
fn c17d_component_offset_is_scaled_only_when_asked() {
    // `SCALED_COMPONENT_OFFSET` (0x0800) makes the placement offset part of the
    // component's own design space, so the component matrix scales it (Apple's reading). The
    // Microsoft default — and `UNSCALED_COMPONENT_OFFSET` (0x1000) explicitly — puts the
    // offset in the composite's coordinates: the matrix applies to the outline only.
    let glyphs = |flags: u16| {
        font_with_glyphs(&[
            Vec::new(),
            simple_glyph(&[(0, 0), (100, 0), (0, 100)]),
            composite_glyph_scaled(&[(1, Placement::Offset(100, 40))], 0x2000, flags),
        ])
    };
    // scale 0.5 then offset (50, 20): the offset went through the matrix too
    assert_eq!(
        outline_points(&glyphs(0x0800), 2),
        vec![(50.0, 20.0), (100.0, 20.0), (50.0, 70.0)]
    );
    // scale 0.5 then offset (100, 40): the offset was already in composite coordinates
    assert_eq!(
        outline_points(&glyphs(0x1000), 2),
        vec![(100.0, 40.0), (150.0, 40.0), (100.0, 90.0)]
    );
    // and that is also what a font with neither flag gets
    assert_eq!(outline_points(&glyphs(0), 2), outline_points(&glyphs(0x1000), 2));
}

#[test]
fn c17e_point_numbers_are_the_glyf_ones() {
    // point matching must count the glyph's own points. A contour that repeats its
    // first point as its last one, and the closing point `contour()` adds, must both stay out of
    // the numbering — point 4 of a four-point contour does not exist.
    let font = font_with_glyphs(&[
        Vec::new(),
        simple_glyph(&[(0, 0), (100, 0), (100, 100), (0, 0)]),
        composite_glyph(&[(1, Placement::Offset(0, 0)), (1, Placement::Points(4, 1))]),
    ]);
    assert_eq!(
        outline_points(&font, 2),
        // the second copy could not be placed, so it sits at the origin unchanged
        vec![(0.0, 0.0), (100.0, 0.0), (100.0, 100.0), (0.0, 0.0),
             (0.0, 0.0), (100.0, 0.0), (100.0, 100.0), (0.0, 0.0)]
    );

    // and point 2 is the third point of the glyph, not the fourth
    let font = font_with_glyphs(&[
        Vec::new(),
        simple_glyph(&[(0, 0), (100, 0), (100, 100), (0, 0)]),
        composite_glyph(&[(1, Placement::Offset(0, 0)), (1, Placement::Points(2, 0))]),
    ]);
    assert_eq!(
        outline_points(&font, 2),
        vec![(0.0, 0.0), (100.0, 0.0), (100.0, 100.0), (0.0, 0.0),
             (100.0, 100.0), (200.0, 100.0), (200.0, 200.0), (100.0, 100.0)]
    );
}

// ---------------------------------------------------------------------------------------------
// an unknown GPOS/GSUB minor version must not abort the font

/// `gpos_table` with an explicit `minorVersion` (OT §GPOS header).
fn gpos_table_version(minor_version: u16, lookup_type: u16, subtable: &[u8]) -> Vec<u8> {
    let mut t = gpos_table(lookup_type, subtable);
    t[2..4].copy_from_slice(&minor_version.to_be_bytes());
    t
}

/// A GSUB table with no scripts, no features and no lookups (OT §GSUB header).
fn gsub_table(minor_version: u16) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 1);            // majorVersion
    push_u16(&mut t, minor_version);
    push_u16(&mut t, 10);           // scriptListOffset
    push_u16(&mut t, 12);           // featureListOffset
    push_u16(&mut t, 14);           // lookupListOffset
    push_u16(&mut t, 0);            // ScriptList: scriptCount
    push_u16(&mut t, 0);            // FeatureList: featureCount
    push_u16(&mut t, 0);            // LookupList: lookupCount
    t
}

#[test]
fn c18_unknown_gpos_minor_version_is_ignored() {
    // `v => panic!("unsupported GPOS version 1.{}")` killed the whole font. The tables we
    // read sit at the same offsets in every 1.x version, so 1.2 must still yield its kerning.
    let subtable = pair_pos_1(1, &[(2, -40)]);
    let mut tables = base_tables(3);
    tables.push((*b"GPOS", gpos_table_version(2, 2, &subtable)));
    let data = build_font(tables);
    assert_eq!(kerning_of(&data, 1, 2), -40.0);
}

#[test]
fn c18b_unknown_gsub_minor_version_is_ignored() {
    // same panic in `parse_gsub`, where it is not even a GPOS table.
    let mut tables = base_tables(2);
    tables.push((*b"GSUB", gsub_table(2)));
    let data = build_font(tables);
    let font = OpenTypeFont::parse(&data).expect("GSUB 1.2 parses");
    assert!(font.gsub.is_some());
}

// ---------------------------------------------------------------------------------------------
// counts taken from the file must not wrap around or index past the end

#[test]
fn c24_woff2_hmtx_rejects_more_metrics_than_glyphs() {
    // `maxp.numGlyphs as usize - number_of_hmetrics` wrapped around to a huge number (a
    // panic in debug builds) when a WOFF2 font claims more metrics than it has glyphs. The
    // three streams are long enough that the count is the first thing that can go wrong.
    let head = font::opentype::parse_head(&head_table()).expect("head parses");
    let maxp = font::opentype::parse_maxp(&maxp_table(1)).expect("maxp parses");
    let hhea = font::opentype::parse_hhea(&hhea_table(2)).expect("hhea parses");
    // flags, 2 x (advanceWidth, lsb), then the trailing lsb array.
    let hmtx = [0u8; 13];
    assert!(font::opentype::parse_hmtx_woff2_format1(&hmtx, &head, &hhea, &maxp).is_err());

    // OT §hhea: `numberOfHMetrics` is at least 1.
    let hhea = font::opentype::parse_hhea(&hhea_table(0)).expect("hhea parses");
    assert!(font::opentype::parse_hmtx_woff2_format1(&hmtx, &head, &hhea, &maxp).is_err());
}

#[test]
fn c24b_woff2_hmtx_reads_a_consistent_table() {
    // Positive control: one glyph, one metric, no trailing lsbs (flags & 2), as WOFF2 §5.4
    // prescribes when the lsb stream is omitted.
    let head = font::opentype::parse_head(&head_table()).expect("head parses");
    let maxp = font::opentype::parse_maxp(&maxp_table(1)).expect("maxp parses");
    let hhea = font::opentype::parse_hhea(&hhea_table(1)).expect("hhea parses");
    // flags = 1 (bearings are all `head.xMin`), advanceWidth = 500, one glyph so no trailing
    // lsb array.
    let hmtx = [1u8, 0x01, 0xf4];
    let (rest, parsed) = font::opentype::parse_hmtx_woff2_format1(&hmtx, &head, &hhea, &maxp)
        .expect("one metric for one glyph");
    assert!(rest.is_empty());
    let m = parsed.metrics_for_gid(0).expect("metric for glyph 0");
    assert_eq!((m.advance, m.lsb), (500.0, head.x_min as f32));
    assert!(parsed.metrics_for_gid(1).is_none());
}

// ---------------------------- OS/2 metrics ----------------------------

/// `OS/2` version 4 (OT §OS/2), 96 bytes: fixed prefix, `ulCodePageRange1/2`, the three
/// version 2 fields the crate reads, and the rest of the tail.
pub fn os2_table(fs_selection: u16, typo: (i16, i16, i16), x_height: i16, cap_height: i16) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 4);        // version
    push_i16(&mut t, 500);      // xAvgCharWidth
    push_u16(&mut t, 400);      // usWeightClass
    push_u16(&mut t, 5);        // usWidthClass
    push_u16(&mut t, 0);        // fsType
    for _ in 0 .. 10 { push_i16(&mut t, 0); } // ySubscriptXSize … yStrikeoutPosition
    push_i16(&mut t, 0);        // sFamilyClass
    t.extend_from_slice(&[0; 10]); // panose
    for range in [1u32, 2, 4, 8] { push_u32(&mut t, range); } // ulUnicodeRange1..4
    t.extend_from_slice(b"TEST"); // achVendID
    push_u16(&mut t, fs_selection);
    push_u16(&mut t, 0x20);     // usFirstCharIndex
    push_u16(&mut t, 0x7e);     // usLastCharIndex
    push_i16(&mut t, typo.0);   // sTypoAscender
    push_i16(&mut t, typo.1);   // sTypoDescender
    push_i16(&mut t, typo.2);   // sTypoLineGap
    push_u16(&mut t, 1000);     // usWinAscent
    push_u16(&mut t, 200);      // usWinDescent
    push_u32(&mut t, 1);        // ulCodePageRange1
    push_u32(&mut t, 0);        // ulCodePageRange2
    push_i16(&mut t, x_height); // sxHeight
    push_i16(&mut t, cap_height); // sCapHeight
    push_u16(&mut t, 0);        // usDefaultChar
    push_u16(&mut t, 0x20);     // usBreakChar
    push_u16(&mut t, 2);        // usMaxContext
    t
}

/// Two empty glyphs plus the given `OS/2` table.
fn font_with_os2(fs_selection: u16) -> OpenTypeFont {
    let (glyf, loca) = glyf_and_loca(&[Vec::new(), Vec::new()]);
    let mut tables = base_tables(2);
    tables.push((*b"loca", loca));
    tables.push((*b"glyf", glyf));
    tables.push((*b"OS/2", os2_table(fs_selection, (900, -300, 120), 500, 700)));
    OpenTypeFont::parse(&build_font(tables)).expect("font parses")
}

#[test]
fn c25_os2_typo_metrics_win_when_the_font_asks_for_them() {
    // line metrics came from `hhea` only; `fsSelection` bit 7 (USE_TYPO_METRICS)
    // says the `sTypo*` values are the ones to use.
    let font = font_with_os2(0x0080);
    let vmetrics = font.vmetrics().expect("line metrics");
    assert_eq!(
        (vmetrics.ascent, vmetrics.descent, vmetrics.line_gap),
        (900.0, -300.0, 120.0)
    );
    // The rest of the table is exposed too, instead of just the weight.
    assert_eq!(font.info().x_height, Some(500.0));
    assert_eq!(font.info().cap_height, Some(700.0));
    assert_eq!(font.info().unicode_ranges, [1, 2, 4, 8]);
    assert_eq!(font.info().weight, Some(400));
}

#[test]
fn c25b_hhea_metrics_without_use_typo_metrics() {
    // Negative control: bit 7 clear means `hhea` is what applications use.
    let font = font_with_os2(0);
    let vmetrics = font.vmetrics().expect("line metrics");
    assert_eq!(
        (vmetrics.ascent, vmetrics.descent, vmetrics.line_gap),
        (800.0, -200.0, 0.0)
    );
}

#[test]
fn c25c_os2_truncated_inside_the_codepage_ranges_has_no_heights() {
    // the version 1 tail was only stepped over when all eight bytes were
    // present, so a table cut inside `ulCodePageRange1` had those four bytes read as
    // `sxHeight`/`sCapHeight` (0 and 256 from the range bits).
    let mut data = os2_table(0, (900, -300, 120), 500, 700);
    data.truncate(82); // 78-byte prefix + half of `ulCodePageRange1`
    let (glyf, loca) = glyf_and_loca(&[Vec::new(), Vec::new()]);
    let mut tables = base_tables(2);
    tables.push((*b"loca", loca));
    tables.push((*b"glyf", glyf));
    tables.push((*b"OS/2", data));
    let font = OpenTypeFont::parse(&build_font(tables)).expect("font parses");
    assert_eq!(font.info().x_height, None);
    assert_eq!(font.info().cap_height, None);
    // The fixed prefix is still read.
    assert_eq!(font.info().weight, Some(400));
}

// ---------------------------- vhea version fields ----------------------------

/// `vhea` (OT §vhea), 36 bytes: two `u16` versions, ascent/descent/lineGap, then the
/// vertical metrics, four reserved words, metricDataFormat and numOfLongVerMetrics.
pub fn vhea_table(ascender: i16, descender: i16, line_gap: i16) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 1);        // majorVersion
    push_u16(&mut t, 1);        // minorVersion
    push_i16(&mut t, ascender); // vertTypoAscender
    push_i16(&mut t, descender);// vertTypoDescender
    push_i16(&mut t, line_gap); // vertTypoLineGap
    push_u16(&mut t, 1000);     // advanceHeightMax
    push_i16(&mut t, 0);        // minTopSideBearing
    push_i16(&mut t, 0);        // minBottomSideBearing
    push_i16(&mut t, 1000);     // yMaxExtent
    push_i16(&mut t, 0);        // caretSlopeRise
    push_i16(&mut t, 1);        // caretSlopeRun
    push_i16(&mut t, 0);        // caretOffset
    for _ in 0 .. 4 { push_i16(&mut t, 0); } // reserved
    push_i16(&mut t, 0);        // metricDataFormat
    push_u16(&mut t, 1);        // numOfLongVerMetrics
    assert_eq!(t.len(), 36);
    t
}

#[test]
fn c16d_vhea_reads_both_version_fields() {
    // `version` was read as one `u16`, so `minorVersion` was taken as `vertTypoAscender`
    // and every following field came from two bytes too early.
    let table = vhea_table(880, -120, 40);
    let (rest, metrics) = font::opentype::parse_vhea(&table).expect("vhea parses");
    assert!(rest.is_empty(), "the whole 36-byte table is consumed");
    assert_eq!(
        (metrics.ascent, metrics.descent, metrics.line_gap),
        (880.0, -120.0, 40.0)
    );
}

// ---------------------------- `name` record preference ----------------------------

/// UTF-16BE bytes, as platform 0 and 3 records carry them.
pub fn utf16be(text: &str) -> Vec<u8> {
    text.encode_utf16().flat_map(|c| c.to_be_bytes()).collect()
}

/// `name` (OT §name), format 0: header, one 12-byte record per entry, then the string
/// storage every record's `offset` is relative to. Records are
/// `(platformID, encodingID, languageID, nameID, string bytes)`.
pub fn name_table(records: &[(u16, u16, u16, u16, &[u8])]) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 0);                              // format
    push_u16(&mut t, records.len() as u16);           // count
    push_u16(&mut t, (6 + 12 * records.len()) as u16);// stringOffset
    let mut storage = Vec::new();
    for &(platform, encoding, language, name_id, string) in records {
        assert!(storage.len() < 0x10000 && string.len() < 0x10000);
        push_u16(&mut t, platform);
        push_u16(&mut t, encoding);
        push_u16(&mut t, language);
        push_u16(&mut t, name_id);
        push_u16(&mut t, string.len() as u16);
        push_u16(&mut t, storage.len() as u16);
        storage.extend_from_slice(string);
    }
    t.extend_from_slice(&storage);
    t
}

#[test]
fn c33_name_picks_the_best_record_not_the_last() {
    // the *last* record that matched (platform 0, 3/0 or 3/1) won, so a font carrying
    // Mac Roman or Unicode-platform names after the Windows ones lost them. The Windows
    // Unicode English record is deliberately *first* here: only a preference order passes.
    let windows = utf16be("WinFam");
    let unicode = utf16be("UniFam");
    let table = name_table(&[
        (3, 1, 0x409, 1, &windows),
        (1, 0, 0, 1, b"MacFam"),
        (0, 3, 0, 1, &unicode),
    ]);
    let name = font::opentype::parse_name(&table).expect("name parses");
    assert_eq!(name.family.as_deref(), Some("WinFam"));
    assert_eq!(name.subfamily, None);
}

#[test]
fn c33_name_decodes_mac_roman() {
    // platform 1 (Macintosh) was ignored outright, so a Type 1-era name table yielded
    // nothing at all. 0x8E is "é" in Mac Roman, not a valid UTF-8 byte.
    let table = name_table(&[
        (1, 0, 0, 6, b"Caf\x8e"),
        (1, 0, 0, 1, b"Famille"),
    ]);
    let name = font::opentype::parse_name(&table).expect("name parses");
    assert_eq!(name.postscript_name.as_deref(), Some("Café"));
    assert_eq!(name.family.as_deref(), Some("Famille"));
}

// ---------------------------- gzip-compressed SVG documents ----------------------------

/// `SVG ` (OT §SVG): version, document list offset, reserved, then the document list
/// (one entry per document, offsets relative to the list start).
#[cfg(feature = "svg")]
pub fn svg_table(documents: &[&[u8]]) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 0);  // version
    push_u32(&mut t, 10); // svgDocumentListOffset
    push_u32(&mut t, 0);  // reserved
    push_u16(&mut t, documents.len() as u16);
    let mut offset = (2 + 12 * documents.len()) as u32;
    for document in documents {
        push_u16(&mut t, 65);   // startGlyphID
        push_u16(&mut t, 65);   // endGlyphID
        push_u32(&mut t, offset);
        push_u32(&mut t, document.len() as u32);
        offset += document.len() as u32;
    }
    for document in documents {
        t.extend_from_slice(document);
    }
    t
}

/// A gzip container around `data`, using a single *stored* (uncompressed) deflate block so
/// the test needs no compressor. The CRC32/ISIZE trailer is not checked by the reader.
#[cfg(feature = "svg")]
pub fn gzip_stored(data: &[u8]) -> Vec<u8> {
    assert!(data.len() <= u16::MAX as usize);
    let mut g = vec![0x1f, 0x8b, 8, 0, 0, 0, 0, 0, 0, 3]; // magic, CM=deflate, no flags
    g.push(0x01); // BFINAL=1, BTYPE=00 (stored)
    g.extend_from_slice(&(data.len() as u16).to_le_bytes());
    g.extend_from_slice(&(!(data.len() as u16)).to_le_bytes());
    g.extend_from_slice(data);
    g.extend_from_slice(&[0; 8]); // CRC32, ISIZE
    g
}

#[cfg(feature = "svg")]
fn font_with_svg(document: &[u8]) -> Box<dyn Font + Send + Sync + 'static> {
    let (glyf, loca) = glyf_and_loca(&[Vec::new(), Vec::new()]);
    let mut tables = base_tables(2);
    tables.push((*b"loca", loca));
    tables.push((*b"glyf", glyf));
    tables.push((*b"SVG ", svg_table(&[document])));
    font::parse(&build_font(tables)).expect("font with an SVG table parses")
}

#[cfg(feature = "svg")]
const SVG_DOC: &[u8] = br#"<svg xmlns="http://www.w3.org/2000/svg"><g id="glyph65"><rect width="10" height="10"/></g></svg>"#;

#[cfg(feature = "svg")]
#[test]
fn c26_gzip_svg_documents_are_decompressed() {
    // OT §SVG allows a gzip-compressed document; the raw stream went to the SVG parser
    // and every gzip font failed to load.
    let font = font_with_svg(&gzip_stored(SVG_DOC));
    assert!(font.svg_glyph(GlyphId(65)).is_some(), "the document's glyph65 is reachable");
}

#[cfg(feature = "svg")]
#[test]
fn c26b_plain_svg_documents_still_parse() {
    // Negative control: an uncompressed document is passed through untouched.
    let font = font_with_svg(SVG_DOC);
    assert!(font.svg_glyph(GlyphId(65)).is_some());
}

#[cfg(feature = "svg")]
#[test]
fn c26c_a_document_that_cannot_be_decompressed_costs_only_itself() {
    // a bad plain SVG document was only logged, a bad gzip one aborted the whole
    // table. Both are now skipped per document.
    let broken: &[u8] = &[0x1f, 0x8b, 8, 0]; // gzip magic, then nothing
    let good = gzip_stored(SVG_DOC);
    let mut t = Vec::new();
    push_u16(&mut t, 0);  // version
    push_u32(&mut t, 10); // svgDocumentListOffset
    push_u32(&mut t, 0);  // reserved
    push_u16(&mut t, 2);  // two documents
    let offset = (2 + 12 * 2) as u32;
    push_u16(&mut t, 64); push_u16(&mut t, 64);
    push_u32(&mut t, offset);
    push_u32(&mut t, broken.len() as u32);
    push_u16(&mut t, 65); push_u16(&mut t, 65);
    push_u32(&mut t, offset + broken.len() as u32);
    push_u32(&mut t, good.len() as u32);
    t.extend_from_slice(broken);
    t.extend_from_slice(&good);

    let (glyf, loca) = glyf_and_loca(&[Vec::new(), Vec::new()]);
    let mut tables = base_tables(2);
    tables.push((*b"loca", loca));
    tables.push((*b"glyf", glyf));
    tables.push((*b"SVG ", t));
    let font = font::parse(&build_font(tables)).expect("one broken document must not fail the table");
    assert!(font.svg_glyph(GlyphId(65)).is_some(), "the readable document still loads");
    assert!(font.svg_glyph(GlyphId(64)).is_none());
}

// ---------------------------------------------------------------------------------------------
// A broken layout table loses shaping data, not the whole font

/// A GPOS table with one lookup carrying two subtables (OT §"Lookup List Table").
fn gpos_two_subtables(lookup_type: u16, first: &[u8], second: &[u8]) -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 1);            // majorVersion
    push_u16(&mut t, 0);            // minorVersion
    push_u16(&mut t, 0);            // scriptListOffset
    push_u16(&mut t, 0);            // featureListOffset
    push_u16(&mut t, 10);           // lookupListOffset
    push_u16(&mut t, 1);            // lookupCount
    push_u16(&mut t, 4);            // lookup offset, from the lookup list
    push_u16(&mut t, lookup_type);
    push_u16(&mut t, 0);            // lookupFlag
    push_u16(&mut t, 2);            // subtableCount
    push_u16(&mut t, 10);           // subtable offset 0, from the lookup
    push_u16(&mut t, (10 + first.len()) as u16); // subtable offset 1
    assert_eq!(t.len(), 24);
    t.extend_from_slice(first);
    t.extend_from_slice(second);
    t
}

#[test]
fn c48_a_broken_gpos_subtable_does_not_cost_the_other_subtables() {
    // one unreadable subtable used to fail `parse`, so a font whose other
    // subtable is perfectly good became unloadable.
    let tables = {
        let mut t = base_tables(8);
        t.push((*b"GPOS", gpos_two_subtables(2, &[0, 1], &pair_pos_1(1, &[(2, -30)]))));
        t
    };
    let font = OpenTypeFont::parse(&build_font(tables)).expect("font with a broken GPOS subtable parses");
    assert_eq!(font.kerning(GlyphId(1), GlyphId(2)), -30.0,
               "the readable subtable of the same lookup still kerns");
}

#[test]
fn c48b_a_broken_gsub_drops_the_table_not_the_font() {
    // `lookupListOffset` pointing past the end used to be fatal.
    let mut gsub = gsub_table(0);
    gsub[8..10].copy_from_slice(&0xffffu16.to_be_bytes()); // lookupListOffset, past the end
    let mut tables = base_tables(8);
    tables.push((*b"GSUB", gsub));
    let font = OpenTypeFont::parse(&build_font(tables)).expect("font with an unreadable GSUB parses");
    assert!(font.gsub.is_none(), "the GSUB that cannot be read is dropped");
}

#[test]
fn c48c_a_truncated_gdef_drops_the_table_not_the_font() {
    // GDEF is optional; half a header is not a reason to refuse the font.
    let mut gdef = gdef_table(Some(class_def_1(1, &[1, 2, 3, 4])), None);
    gdef.truncate(8);
    let mut tables = base_tables(8);
    tables.push((*b"GDEF", gdef));
    let font = OpenTypeFont::parse(&build_font(tables)).expect("font with a truncated GDEF parses");
    assert!(font.gdef.is_none(), "the GDEF that cannot be read is dropped");
}
