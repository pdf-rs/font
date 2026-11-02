//! Minimal in-memory fonts, one per specification mismatch reported in
//! `report/pdf-rs-font-vs-specs-2026-11-02.md`.
//!
//! Each case builds the smallest sfnt that still reaches the code under test, so a failure names
//! exactly one finding. Run everything: `cargo test`; one group: `cargo test c01`.

use font::opentype::{gdef::{GDef, MarkClass}, OpenTypeFont};

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
pub fn base_tables(num_glyphs: u16) -> Vec<([u8; 4], Vec<u8>)> {
    vec![
        (*b"head", head_table()),
        (*b"hhea", hhea_table(1)),
        (*b"maxp", maxp_table(num_glyphs)),
        (*b"hmtx", hmtx_table(1)),
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
