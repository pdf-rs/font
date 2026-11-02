//! Minimal in-memory WOFF files, one per panic reported in
//! `report/pdf-rs-font-vs-specs-2026-11-02.md` (F26).
#![cfg(feature = "woff")]

use font::Font;

fn push_u16(v: &mut Vec<u8>, x: u16) {
    v.extend_from_slice(&x.to_be_bytes());
}
fn push_u32(v: &mut Vec<u8>, x: u32) {
    v.extend_from_slice(&x.to_be_bytes());
}

/// WOFF 1.0 header (WOFF §"WOFF Header"), 44 bytes.
fn woff_header(flavor: &[u8; 4], num_tables: u16) -> Vec<u8> {
    let mut v = Vec::new();
    v.extend_from_slice(b"wOFF");
    v.extend_from_slice(flavor);
    push_u32(&mut v, 44); // length
    push_u16(&mut v, num_tables);
    push_u16(&mut v, 0); // reserved
    push_u32(&mut v, 0); // totalSfntSize
    push_u16(&mut v, 1); // majorVersion
    push_u16(&mut v, 0); // minorVersion
    push_u32(&mut v, 0); // metaOffset
    push_u32(&mut v, 0); // metaLength
    push_u32(&mut v, 0); // metaOrigLength
    push_u32(&mut v, 0); // privOffset
    push_u32(&mut v, 0); // privLength
    v
}

fn dir_entry(v: &mut Vec<u8>, tag: &[u8; 4], offset: u32, comp: u32, orig: u32) {
    v.extend_from_slice(tag);
    push_u32(v, offset);
    push_u32(v, comp);
    push_u32(v, orig);
    push_u32(v, 0); // origChecksum
}

#[test]
fn c23_woff_collection_is_an_error() {
    // F26: `unimplemented!()` for flavor `ttcf`.
    let data = woff_header(b"ttcf", 0);
    assert!(font::parse_woff(&data).is_err());
}

#[test]
fn c23b_corrupt_zlib_table_is_an_error() {
    // F26: `.expect("can't decompress data")`. comp_length < orig_length marks the table
    // as zlib-compressed (WOFF §"Table Directory").
    let mut data = woff_header(&[0, 1, 0, 0], 1);
    dir_entry(&mut data, b"glyf", 64, 4, 8);
    data.extend_from_slice(&[0xDE, 0xAD, 0xBE, 0xEF]); // not a zlib stream
    assert!(font::parse_woff(&data).is_err());
}

#[test]
fn c23c_truncated_table_directory_is_an_error() {
    // The directory claims one table but carries none.
    let data = woff_header(&[0, 1, 0, 0], 1);
    assert!(font::parse_woff(&data).is_err());
}

// ---------------------------- F46: tables are read at their offsets ----------------------------

/// `head` (OT §head), 54 bytes, `unitsPerEm = 1000`.
fn head_table() -> Vec<u8> {
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
    t.extend_from_slice(&[0; 8]);   // xMin, yMin, xMax, yMax
    push_u16(&mut t, 0);            // macStyle
    push_u16(&mut t, 8);            // lowestRecPPEM
    push_u16(&mut t, 2);            // fontDirectionHint
    push_u16(&mut t, 0);            // indexToLocFormat
    push_u16(&mut t, 0);            // glyphDataFormat
    assert_eq!(t.len(), 54);
    t
}

/// `hhea` (OT §hhea), 36 bytes, one horizontal metric.
fn hhea_table() -> Vec<u8> {
    let mut t = Vec::new();
    push_u16(&mut t, 1);        // majorVersion
    push_u16(&mut t, 0);        // minorVersion
    t.extend_from_slice(&[0; 6]); // ascender, descender, lineGap
    t.extend_from_slice(&[0; 6]); // advanceWidthMax, minLeftSideBearing, minRightSideBearing
    t.extend_from_slice(&[0; 8]); // xMaxExtent, caretSlopeRise/Run, caretOffset
    t.extend_from_slice(&[0; 8]); // reserved
    push_u16(&mut t, 0);        // metricDataFormat
    push_u16(&mut t, 1);        // numberOfHMetrics
    assert_eq!(t.len(), 36);
    t
}

/// `maxp` (OT §maxp) version 1.0 with `num_glyphs`.
fn maxp_table(num_glyphs: u16) -> Vec<u8> {
    let mut t = Vec::new();
    push_u32(&mut t, 0x0001_0000);
    push_u16(&mut t, num_glyphs);
    t
}

/// A WOFF 1.0 file. `tables` are stored in the order given, while the directory is sorted
/// by tag, as WOFF §"Table Directory" requires — so storage order and directory order
/// differ unless the caller sorts.
fn woff(tables: &[(&[u8; 4], Vec<u8>)]) -> Vec<u8> {
    let dir_len = 44 + 20 * tables.len();
    let mut placed: Vec<(&[u8; 4], u32, u32)> = Vec::new();
    let mut body = Vec::new();
    for (tag, data) in tables {
        placed.push((tag, (dir_len + body.len()) as u32, data.len() as u32));
        body.extend_from_slice(data);
        while body.len() % 4 != 0 { body.push(0); }
    }

    let mut out = Vec::new();
    out.extend_from_slice(b"wOFF");
    out.extend_from_slice(&[0, 1, 0, 0]);                    // flavor
    push_u32(&mut out, (dir_len + body.len()) as u32);        // length
    push_u16(&mut out, tables.len() as u16);
    push_u16(&mut out, 0);                                   // reserved
    push_u32(&mut out, 12 + 16 * tables.len() as u32);       // totalSfntSize
    push_u16(&mut out, 1);                                   // majorVersion
    push_u16(&mut out, 0);                                   // minorVersion
    for _ in 0 .. 5 { push_u32(&mut out, 0); }               // meta/priv offsets

    let mut order: Vec<usize> = (0 .. tables.len()).collect();
    order.sort_by_key(|&n| tables[n].0);
    for &n in &order {
        let (tag, offset, len) = placed[n];
        out.extend_from_slice(tag);
        push_u32(&mut out, offset);
        push_u32(&mut out, len); // compLength == origLength: stored uncompressed
        push_u32(&mut out, len);
        push_u32(&mut out, 0);   // origChecksum
    }
    out.extend_from_slice(&body);
    out
}

#[test]
fn c27_woff_tables_are_read_at_their_offsets() {
    // F46: tables were read sequentially after the directory, ignoring `offset`, so any
    // file whose storage order differs from the directory order was misread.
    let data = woff(&[
        (b"maxp", maxp_table(3)), // stored first …
        (b"hhea", hhea_table()),
        (b"head", head_table()),  // … head last, but the directory lists head first
    ]);
    let font = font::parse_woff(&data).expect("the tables are where the directory says");
    assert_eq!(font.num_glyphs(), 3);
    // The head table really was read: unitsPerEm 1000 -> a 1/1000 font matrix.
    assert_eq!(font.font_matrix().m11(), 0.001);
}

#[test]
fn c27b_table_offset_past_the_end_of_the_file_is_an_error() {
    // WOFF §"Table Directory": `offset`/`compLength` must describe a range inside the file.
    let mut data = woff(&[(b"head", head_table()), (b"maxp", maxp_table(1)), (b"hhea", hhea_table())]);
    // The first directory entry is `head`; point it far past the end.
    let offset_at = 44 + 4;
    data[offset_at .. offset_at + 4].copy_from_slice(&1_000_000u32.to_be_bytes());
    assert!(font::parse_woff(&data).is_err());
}
