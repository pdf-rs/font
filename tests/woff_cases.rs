//! Minimal in-memory WOFF files, one per panic reported in
//! `report/pdf-rs-font-vs-specs-2026-11-02.md` (F26).
#![cfg(feature = "woff")]

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
