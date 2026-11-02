//! Minimal in-memory Type 1 fonts (PFA and PFB), one per mismatch reported in
//! `report/pdf-rs-font-vs-specs-2026-11-02.md` (F24, F25).
//!
//! The fonts are plain PostScript text: no `eexec` wrapper, only the charstrings are
//! encrypted the way TN5015 §7 defines (`lenIV` random bytes, then the RC4-like
//! cipher of `eexec::Decoder::charstring`, r = 4330).
#![cfg(feature = "type1")]

use font::{Font, GlyphId};

// ---------------------------- charstring encryption

/// Inverse of `eexec::Decoder::charstring().decode(data, len_iv)`: prepend `len_iv`
/// arbitrary bytes, encrypt everything.
fn encrypt(plain: &[u8], len_iv: usize) -> Vec<u8> {
    const C1: u16 = 52845;
    const C2: u16 = 22719;
    let mut r: u16 = 4330;
    let mut out = Vec::with_capacity(plain.len() + len_iv);
    for &p in std::iter::repeat(&0x5Au8).take(len_iv).chain(plain.iter()) {
        let cipher = p ^ (r >> 8) as u8;
        r = (cipher as u16)
            .wrapping_add(r)
            .wrapping_mul(C1)
            .wrapping_add(C2);
        out.push(cipher);
    }
    out
}

// ---------------------------- PFA assembly

/// Append `data` as a PostScript literal string `(…)`, escaping what the tokenizer
/// would otherwise interpret. Bytes are kept raw, so any 8-bit value survives.
fn push_ps_string(out: &mut Vec<u8>, data: &[u8]) {
    out.push(b'(');
    for &b in data {
        match b {
            b'(' | b')' | b'\\' => {
                out.push(b'\\');
                out.push(b);
            }
            b'\n' => out.extend_from_slice(b"\\n"),
            b'\r' => out.extend_from_slice(b"\\r"),
            b'\t' => out.extend_from_slice(b"\\t"),
            _ => out.push(b),
        }
    }
    out.push(b')');
}

/// A Type 1 font in PFA form.
///
/// `charstrings` maps glyph name → *plain* (unencrypted) Type 1 charstring;
/// `encoding` is the raw text of the `/Encoding` definition (`None` = no `/Encoding`
/// entry at all).
fn pfa(charstrings: &[(&str, &[u8])], encoding: Option<&str>) -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(b"%!PS-AdobeFont-1.0: Test\n");
    // `/Test` first, the font dict on top: that is the order `definefont` expects.
    out.extend_from_slice(b"/Test 4 dict dup begin\n");
    out.extend_from_slice(b"/FontName /Test def\n");
    out.extend_from_slice(b"/FontMatrix {0.001 0 0 0.001 0 0} def\n");
    out.extend_from_slice(b"/FontBBox {0 0 1000 1000} def\n");

    out.extend_from_slice(format!("/CharStrings {} dict dup begin\n", charstrings.len()).as_bytes());
    for (name, plain) in charstrings {
        out.extend_from_slice(format!("/{} ", name).as_bytes());
        push_ps_string(&mut out, &encrypt(plain, 4));
        out.extend_from_slice(b" def\n");
    }
    out.extend_from_slice(b"end readonly def\n");

    out.extend_from_slice(b"/Private 1 dict dup begin\n/lenIV 4 def\nend readonly def\n");
    if let Some(encoding) = encoding {
        out.extend_from_slice(b"/Encoding ");
        out.extend_from_slice(encoding.as_bytes());
        out.extend_from_slice(b" def\n");
    }
    out.extend_from_slice(b"end\ndefinefont pop\n");
    out
}

// ---------------------------- charstrings

/// `0 500 hsbw` — Type 1 ints 0 and 500 (500 is the two-byte form `248 136`).
const HSBW: &[u8] = &[139, 248, 136, 13];

fn notdef() -> Vec<u8> {
    let mut cs = HSBW.to_vec();
    cs.push(14); // endchar
    cs
}

/// `hsbw 100 100 rlineto endchar`
fn glyph_a() -> Vec<u8> {
    let mut cs = HSBW.to_vec();
    cs.extend_from_slice(&[239, 239, 5]); // 100 100 rlineto
    cs.push(14);
    cs
}

fn parse(data: &[u8]) -> Box<dyn Font + Send + Sync + 'static> {
    font::parse(data).expect("Type 1 font parses")
}

// ---------------------------- tests

#[test]
fn c20_type1_without_encoding_parses() {
    // F24: `/Encoding` is optional (PLRM §5.6); `.expect("no /Encoding")` aborted the font.
    let data = pfa(&[(".notdef", &notdef()), ("A", &glyph_a())], None);
    let font = parse(&data);
    assert_eq!(font.num_glyphs(), 2);
    let a = font.gid_for_name("A").expect("glyph A");
    // No encoding: no codepoint reaches the glyph, the name still does.
    assert_eq!(font.gid_for_codepoint(65), None);
    // The width came from `hsbw`.
    assert_eq!(font.glyph(a).unwrap().metrics.advance, 500.0);
}

#[test]
fn c20b_encoding_as_a_name_parses() {
    // F24: `/Encoding` may be a name instead of an array (`as_array().expect(…)` panicked).
    let data = pfa(
        &[(".notdef", &notdef()), ("A", &glyph_a())],
        Some("/StandardEncoding"),
    );
    let font = parse(&data);
    assert_eq!(font.num_glyphs(), 2);
    assert!(font.gid_for_name("A").is_some());
}

#[test]
fn c20c_encoding_array_maps_codepoints() {
    // The normal case must keep working: codepoint N is the N-th entry of the array.
    let data = pfa(
        &[(".notdef", &notdef()), ("A", &glyph_a())],
        Some("{/.notdef /A}"),
    );
    let font = parse(&data);
    // Glyph ids follow the `/CharStrings` iteration order (a HashMap), so compare against
    // the name lookup rather than a literal id.
    assert_eq!(font.gid_for_codepoint(1), font.gid_for_name("A"));
    assert!(font.gid_for_codepoint(1).is_some());
    // `.notdef` is skipped: codepoint 0 does not reach a glyph.
    assert_eq!(font.gid_for_codepoint(0), None);
}

#[test]
fn c21_unknown_charstring_operator_skips_the_glyph() {
    // F25: an unknown escape (`12 n`) used to `panic!("invalid operator")`.
    let mut cs = HSBW.to_vec();
    cs.extend_from_slice(&[12, 99]); // 12 99: not a valid Type 1 escape
    cs.push(14);
    let data = pfa(
        &[(".notdef", &notdef()), ("A", &cs)],
        Some("{/.notdef /A}"),
    );
    let font = parse(&data);
    // The glyph is dropped with a warning, the font itself still parses.
    assert_eq!(font.num_glyphs(), 1);
    assert_eq!(font.gid_for_name("A"), None);
    assert_eq!(font.gid_for_name(".notdef"), Some(GlyphId(0)));
}

#[test]
fn c22_pfb_unknown_block_type_is_an_error() {
    // F25: `panic!("unknown block type")` in the PFB reader.
    let data = [
        0x80, 1, 0, 0, 0, 0, // an empty ASCII block
        0x80, 7, 0, 0, 0, 0, // block type 7 does not exist
    ];
    assert!(font::parse(&data).is_err());
}
