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
    pfa_len_iv(charstrings, encoding, 4)
}

/// Same as `pfa`, but with an explicit `/lenIV`.
///
/// A negative `len_iv` writes `/lenIV -1 def` and stores the charstrings *unencrypted*,
/// which is what a font with `lenIV = -1` really looks like (TN5015 §2).
fn pfa_len_iv(charstrings: &[(&str, &[u8])], encoding: Option<&str>, len_iv: i32) -> Vec<u8> {
    // A negative `len_iv` means the charstrings are stored unencrypted (TN5015 §2);
    // otherwise `len_iv` random bytes are prepended and everything is run through the
    // charstring cipher.
    let prepared: Vec<(&str, Vec<u8>)> = charstrings
        .iter()
        .map(|&(name, plain)| {
            let bytes = if len_iv >= 0 {
                encrypt(plain, len_iv as usize)
            } else {
                plain.to_vec()
            };
            (name, bytes)
        })
        .collect();
    let borrowed: Vec<(&str, &[u8])> = prepared
        .iter()
        .map(|(name, bytes)| (*name, bytes.as_slice()))
        .collect();
    pfa_with_raw_strings(&borrowed, encoding, len_iv)
}

/// Assemble a PFA font whose `/CharStrings` entries are written **verbatim** — the
/// caller has already encrypted them (or not, for `lenIV < 0`). Needed to build fonts
/// whose charstrings are shorter than `lenIV`.
fn pfa_with_raw_strings(
    charstrings: &[(&str, &[u8])],
    encoding: Option<&str>,
    len_iv: i32,
) -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(b"%!PS-AdobeFont-1.0: Test\n");
    // `/Test` first, the font dict on top: that is the order `definefont` expects.
    out.extend_from_slice(b"/Test 4 dict dup begin\n");
    out.extend_from_slice(b"/FontName /Test def\n");
    out.extend_from_slice(b"/FontMatrix {0.001 0 0 0.001 0 0} def\n");
    out.extend_from_slice(b"/FontBBox {0 0 1000 1000} def\n");

    out.extend_from_slice(format!("/CharStrings {} dict dup begin\n", charstrings.len()).as_bytes());
    for (name, bytes) in charstrings {
        out.extend_from_slice(format!("/{} ", name).as_bytes());
        push_ps_string(&mut out, bytes);
        out.extend_from_slice(b" def\n");
    }
    out.extend_from_slice(b"end readonly def\n");

    out.extend_from_slice(
        format!("/Private 1 dict dup begin\n/lenIV {} def\nend readonly def\n", len_iv).as_bytes(),
    );
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

// ---------------------------- seac (F10)

/// A Type 1 number: `139+n` for 0..=107, `247…248` for 108..=1130, and `255` plus a
/// four-byte integer for everything else (including negatives).
fn num(out: &mut Vec<u8>, n: i32) {
    if (0 ..= 107).contains(&n) {
        out.push((n + 139) as u8);
    } else if (108 ..= 1130).contains(&n) {
        out.push((247 + (n - 108) / 256) as u8);
        out.push(((n - 108) % 256) as u8);
    } else {
        out.push(255);
        out.extend_from_slice(&n.to_be_bytes());
    }
}

/// `hsbw` then a rectangle from `(x0, y0)` to `(x1, y1)`, `endchar`.
fn rect(x0: i32, y0: i32, x1: i32, y1: i32, width: i32) -> Vec<u8> {
    let mut cs = Vec::new();
    num(&mut cs, x0);
    num(&mut cs, width);
    cs.push(13); // hsbw
    let (w, h) = (x1 - x0, y1 - y0);
    for &(dx, dy) in &[(w, 0), (0, h), (-w, 0), (0, -h)] {
        num(&mut cs, dx);
        num(&mut cs, dy);
        cs.push(5); // rlineto
    }
    cs.push(14); // endchar
    cs
}

/// `hsbw 0 500` then `asb adx ady bchar achar seac` (12 6).
fn seac(adx: i32, ady: i32, base: i32, accent: i32) -> Vec<u8> {
    let mut cs = HSBW.to_vec();
    num(&mut cs, 0); // asb
    num(&mut cs, adx);
    num(&mut cs, ady);
    num(&mut cs, base);
    num(&mut cs, accent);
    cs.extend_from_slice(&[12, 6]);
    cs
}

// ---------------------------- flex (F40)

/// A charstring drawing a 100 unit wide flex: `rmoveto` to the reference point, two
/// `rrcurveto`, then `flexheight x y 3 0 callothersubr` and `pop pop setcurrentpoint`.
fn flex_glyph() -> Vec<u8> {
    let mut cs = HSBW.to_vec(); // hsbw 0 500
    num(&mut cs, 0); // n = 0
    num(&mut cs, 1); // otherSubr 1: flex start
    cs.extend_from_slice(&[12, 16]);
    num(&mut cs, 10); // rmoveto, reference point (10, 10)
    num(&mut cs, 10);
    cs.push(21);
    for &d in &[0, 50, 50, 50, 40, 0] { // first curve, ends (100, 110)
        num(&mut cs, d);
    }
    cs.push(8); // rrcurveto
    for &d in &[10, -50, 40, -50, 0, 0] { // second curve, ends (150, 10)
        num(&mut cs, d);
    }
    cs.push(8); // rrcurveto
    num(&mut cs, 10); // flex height
    num(&mut cs, 150); // final x
    num(&mut cs, 10); // final y
    num(&mut cs, 3); // n = 3
    num(&mut cs, 0); // otherSubr 0: flex end
    cs.extend_from_slice(&[12, 16]);
    cs.extend_from_slice(&[12, 17, 12, 17, 12, 33]); // pop pop setcurrentpoint
    cs.push(14); // endchar
    cs
}

#[test]
fn c24_type1_flex_draws_two_cubics() {
    // F40: only `rmoveto` fed the flex sequence, so the seven points were never
    // complete and the glyph was dropped (`TupleElements` failed).
    let data = pfa(
        &[(".notdef", &notdef()), ("flex", &flex_glyph())],
        Some("{/.notdef /flex}"),
    );
    let font = parse(&data);
    let gid = font.gid_for_name("flex").expect("glyph flex");
    let glyph = font.glyph(gid).unwrap();

    assert_eq!(glyph.metrics.advance, 500.0);
    let contours = glyph.path.contours();
    assert_eq!(contours.len(), 1);
    let points = contours[0].points();
    let expected = [
        (10., 10.),   // reference point, the start of the flex
        (10., 60.),   // first control point
        (60., 110.),  // second control point
        (100., 110.), // joining point
        (110., 60.),  // third control point
        (150., 10.),  // fourth control point
        (150., 10.),  // end point
    ];
    assert_eq!(points.len(), expected.len(), "{:?}", points);
    for (p, &(x, y)) in points.iter().zip(expected.iter()) {
        assert_eq!((p.x(), p.y()), (x, y));
    }
}

#[test]
fn c23_type1_seac_composes_the_accent() {
    // F10: `seac` was a no-op, so every accented Type 1 glyph came out empty.
    let data = pfa(
        &[
            (".notdef", &notdef()),
            ("A", &rect(0, 0, 300, 700, 500)),
            ("acute", &rect(0, 0, 50, 60, 300)),
            ("Aacute", &seac(100, 200, 65, 194)),
        ],
        Some("{/.notdef /A /acute /Aacute}"),
    );
    let font = parse(&data);
    let a = font.gid_for_name("A").expect("glyph A");
    let acute = font.gid_for_name("acute").expect("glyph acute");
    let aacute = font.gid_for_name("Aacute").expect("glyph Aacute");

    let base = font.glyph(a).unwrap().path;
    let accent = font.glyph(acute).unwrap().path;
    let composed = font.glyph(aacute).unwrap();

    // The composite keeps its own width …
    assert_eq!(composed.metrics.advance, 500.0);
    // … and carries both parts: bchar 65 = /A, achar 194 = /acute (StandardEncoding).
    assert_eq!(
        composed.path.contours().len(),
        base.contours().len() + accent.contours().len()
    );
    // The accent sits at (adx, ady) from the base origin.
    let shifted = &composed.path.contours()[base.contours().len()];
    let original = accent.contours()[0].points();
    assert_eq!(shifted.points().len(), original.len());
    for (s, o) in shifted.points().iter().zip(original) {
        assert_eq!(s.x(), o.x() + 100.0);
        assert_eq!(s.y(), o.y() + 200.0);
    }
    // The base is not moved.
    let base_after = composed.path.contours()[0].points();
    for (s, o) in base_after.iter().zip(base.contours()[0].points()) {
        assert_eq!(s, o);
    }
}

// ---------------------------- lenIV (F41)

#[test]
fn c25_type1_leniv_minus_one_is_plaintext() {
    // F41: `lenIV -1` means "charstrings are not encrypted" (TN5015 §2), but the value
    // was cast to `usize` and every charstring decrypted with a garbage key and skipped.
    let data = pfa_len_iv(
        &[(".notdef", &notdef()), ("A", &glyph_a())],
        Some("{/.notdef /A}"),
        -1,
    );
    let font = parse(&data);
    assert_eq!(font.num_glyphs(), 2);
    let a = font.gid_for_name("A").expect("glyph A");
    // `/Encoding` is positional: array entry 1 is `/A`, so code 1 reaches it.
    assert_eq!(font.gid_for_codepoint(1), Some(a));

    let glyph = font.glyph(a).unwrap();
    assert_eq!(glyph.metrics.advance, 500.0);
    assert_eq!(glyph.metrics.lsb, 0.0);
    // `100 100 rlineto` after `hsbw`: one contour whose only recorded point is the
    // rlineto endpoint (the starting point at the origin is implicit).
    let contours = glyph.path.contours();
    assert_eq!(contours.len(), 1);
    let points = contours[0].points();
    assert_eq!(points.len(), 1, "{:?}", points);
    assert_eq!((points[0].x(), points[0].y()), (100.0, 100.0));
}

#[test]
fn c26_type1_charstring_shorter_than_leniv_is_skipped() {
    // F41: a charstring shorter than `lenIV` cannot hold the random prefix. Decoding it
    // produced an empty string, which failed with "CharWidth not set" — the glyph is
    // skipped with a warning instead, and the rest of the font still loads.
    let short = b"ab"; // 2 bytes, lenIV is 4
    let notdef_cs = notdef_encrypted();
    let b_cs = glyph_a_encrypted();
    let data = pfa_with_raw_strings(
        &[
            (".notdef", notdef_cs.as_slice()),
            ("A", &short[..]),
            ("B", b_cs.as_slice()),
        ],
        Some("{/.notdef /A /B}"),
        4,
    );
    let font = parse(&data);
    // `.notdef` and `/B` survive, `/A` is gone.
    assert!(font.gid_for_name("A").is_none());
    assert_eq!(font.num_glyphs(), 2);
    let b = font.gid_for_name("B").expect("glyph B");
    assert_eq!(font.glyph(b).unwrap().metrics.advance, 500.0);
}

/// `notdef()` run through the charstring cipher, as `pfa` would store it.
fn notdef_encrypted() -> Vec<u8> {
    encrypt(&notdef(), 4)
}

/// `glyph_a()` run through the charstring cipher, as `pfa` would store it.
fn glyph_a_encrypted() -> Vec<u8> {
    encrypt(&glyph_a(), 4)
}
