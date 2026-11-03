//! Minimal in-memory CFF fonts, one per Type 2 charstring mismatch reported in
//! `report/pdf-rs-font-vs-specs-2026-11-02.md`.
//!
//! The Type 2 interpreter (`src/type2.rs`) is private, so every case is observed
//! through `CffFont::parse` -> `Font::glyph`: the advance width and the outline
//! points of a glyph built from a hand-written charstring.
#![cfg(feature = "cff")]

use font::{CffFont, Font, Glyph, GlyphId};

/// CFF DICT / Type 2 charstring integer encoding (TN5176 §"Operand Encoding",
/// TN5177 §"Charstring Number Encoding"); the two are identical for these values.
fn push_int(v: i32, out: &mut Vec<u8>) {
    if (-107..=107).contains(&v) {
        out.push((v + 139) as u8);
    } else if (108..=1131).contains(&v) {
        let v = v - 108;
        out.push((247 + v / 256) as u8);
        out.push((v % 256) as u8);
    } else if (-1131..=-108).contains(&v) {
        let v = -v - 108;
        out.push((251 - v / 256) as u8);
        out.push((v % 256) as u8);
    } else {
        out.push(28);
        out.extend_from_slice(&(v as i16).to_be_bytes());
    }
}

/// A CFF INDEX with a one-byte offSize (enough for these tiny fonts).
fn index(items: &[&[u8]]) -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(&(items.len() as u16).to_be_bytes());
    if items.is_empty() {
        return out;
    }
    out.push(1); // offSize
    let mut off = 1u8;
    out.push(off);
    for item in items {
        off += item.len() as u8;
        out.push(off);
    }
    for item in items {
        out.extend_from_slice(item);
    }
    out
}

/// header + Name INDEX + TopDICT INDEX + String INDEX + GlobalSubr INDEX +
/// CharStrings INDEX + `tail`.
///
/// `extra_top_dict` holds complete TopDICT entries placed before the ones this
/// builder writes; `private` adds `Private [0,0]`; `charset_tail` adds a `Charset`
/// entry pointing at `tail` and appends `tail` after the CharStrings INDEX.
fn build_cff_full(
    extra_top_dict: &[u8],
    private: bool,
    charset_tail: Option<&[u8]>,
    cs0: &[u8],
    cs1: &[u8],
) -> Vec<u8> {
    build_cff_glyphs(extra_top_dict, private, charset_tail, &[cs0, cs1])
}

/// `build_cff_full` with any number of charstrings (the charset in `charset_tail` names
/// them).
fn build_cff_glyphs(
    extra_top_dict: &[u8],
    private: bool,
    charset_tail: Option<&[u8]>,
    char_strings: &[&[u8]],
) -> Vec<u8> {
    build_cff_glyphs_with_gsubrs(extra_top_dict, private, charset_tail, &[], char_strings)
}

/// `build_cff_glyphs`, plus a GlobalSubr INDEX holding `global_subrs` (index 0 is
/// `-global_subr_bias callgsubr`).
fn build_cff_glyphs_with_gsubrs(
    extra_top_dict: &[u8],
    private: bool,
    charset_tail: Option<&[u8]>,
    global_subrs: &[&[u8]],
    char_strings: &[&[u8]],
) -> Vec<u8> {
    let name_index = index(&[b"Test".as_ref()]);
    let empty = index(&[]);
    let gsubr_index = index(global_subrs);
    let charstrings = index(char_strings);
    let tail = charset_tail.unwrap_or(&[]);

    // The CharStrings and Charset operands are absolute offsets into the CFF data, and
    // their own encoding length depends on their value: encode, measure, repeat.
    let mut charset_offset = 0i32;
    let mut cs_offset = 0i32;
    let mut top_dict = Vec::new();
    let mut top_dict_index = Vec::new();
    for _ in 0..8 {
        top_dict.clear();
        top_dict.extend_from_slice(extra_top_dict);
        if private {
            push_int(0, &mut top_dict);
            push_int(0, &mut top_dict);
            top_dict.push(18); // Operator::Private
        }
        if charset_tail.is_some() {
            push_int(charset_offset, &mut top_dict);
            top_dict.push(15); // Operator::Charset
        }
        push_int(cs_offset, &mut top_dict);
        top_dict.push(17); // Operator::CharStrings
        top_dict_index = index(&[top_dict.as_slice()]);
        let base = (4 + name_index.len()
            + top_dict_index.len()
            + empty.len()
            + gsubr_index.len()) as i32;
        let new_charset = if charset_tail.is_some() {
            base + charstrings.len() as i32
        } else {
            0
        };
        let stable = base == cs_offset && new_charset == charset_offset;
        cs_offset = base;
        charset_offset = new_charset;
        if stable {
            break;
        }
    }

    let mut data = vec![1, 0, 4, 1]; // major, minor, hdrSize, offSize
    data.extend_from_slice(&name_index);
    data.extend_from_slice(&top_dict_index);
    data.extend_from_slice(&empty); // String INDEX
    data.extend_from_slice(&gsubr_index); // GlobalSubr INDEX
    data.extend_from_slice(&charstrings);
    data.extend_from_slice(tail);
    data
}

/// header + Name INDEX + TopDICT INDEX + String INDEX + GlobalSubr INDEX +
/// CharStrings INDEX. The TopDICT holds only `Private [0,0]` and `CharStrings`,
/// so the private dict is empty and every glyph falls back to width 0.
fn build_cff(cs0: &[u8], cs1: &[u8]) -> Vec<u8> {
    build_cff_full(&[], true, None, cs0, cs1)
}

/// `build_cff`, but the String INDEX holds `strings` (SID `391 + i`, the first one after the
/// 391 standard strings), so `extra_top_dict` can name SIDs that only exist in this font.
fn build_cff_with_strings(extra_top_dict: &[u8], strings: &[&[u8]]) -> Vec<u8> {
    let name_index = index(&[b"Test".as_ref()]);
    let string_index = index(strings);
    let empty = index(&[]);
    let charstrings = index(&[&[14u8][..], &[14u8][..]]); // two `endchar`-only glyphs

    // The CharStrings operand is an absolute offset, its own encoding length depends on its
    // value: encode, measure, repeat.
    let mut cs_offset = 0i32;
    let mut top_dict = Vec::new();
    let mut top_dict_index = Vec::new();
    for _ in 0..8 {
        top_dict.clear();
        top_dict.extend_from_slice(extra_top_dict);
        push_int(cs_offset, &mut top_dict);
        top_dict.push(17); // Operator::CharStrings
        top_dict_index = index(&[top_dict.as_slice()]);
        let base = (4 + name_index.len()
            + top_dict_index.len()
            + string_index.len()
            + empty.len()) as i32;
        let stable = base == cs_offset;
        cs_offset = base;
        if stable {
            break;
        }
    }

    let mut data = vec![1, 0, 4, 1]; // major, minor, hdrSize, offSize
    data.extend_from_slice(&name_index);
    data.extend_from_slice(&top_dict_index);
    data.extend_from_slice(&string_index);
    data.extend_from_slice(&empty); // GlobalSubr INDEX
    data.extend_from_slice(&charstrings);
    data
}

fn glyph(data: &[u8], gid: u32) -> Glyph {
    let font = CffFont::parse(data, 0).expect("font parses");
    font.glyph(GlyphId(gid)).expect("glyph exists")
}

/// Every on-curve point of the outline, in order (TN5177 contours).
fn points(glyph: &Glyph) -> Vec<(f32, f32)> {
    let mut out = Vec::new();
    for contour in glyph.path.contours() {
        for point in contour.points() {
            out.push((point.x(), point.y()));
        }
    }
    out
}

#[test]
fn c06_hstem_takes_no_width_when_the_argument_count_is_even() {
    // F6: `hstem` must treat an even number of arguments as "no width", exactly
    // like `hstemhm` (TN5177 §4.3: the width, if present, makes the count odd).
    let mut cs = Vec::new();
    for v in [10, 20, 30, 40] {
        push_int(v, &mut cs);
    }
    cs.push(1); // hstem
    cs.push(14); // endchar
    let data = build_cff(&[14], &cs);

    assert_eq!(
        glyph(&data, 1).metrics.advance,
        0.0,
        "10 20 30 40 hstem is two stem hints and no width"
    );
}

#[test]
fn c06b_hstem_width_is_the_odd_argument() {
    // Positive control: an odd count does carry a width, so the first operand is
    // consumed as the delta width (nominal width 0 -> advance == 500).
    let mut cs = Vec::new();
    for v in [500, 10, 20, 30, 40] {
        push_int(v, &mut cs);
    }
    cs.push(1); // hstem
    cs.push(14); // endchar
    let data = build_cff(&[14], &cs);

    assert_eq!(glyph(&data, 1).metrics.advance, 500.0);
}

#[test]
fn c06c_vstem_takes_no_width_when_the_argument_count_is_even() {
    // Same bug as c06 on the `vstem` operator.
    let mut cs = Vec::new();
    for v in [10, 20, 30, 40] {
        push_int(v, &mut cs);
    }
    cs.push(3); // vstem
    cs.push(14); // endchar
    let data = build_cff(&[14], &cs);

    assert_eq!(glyph(&data, 1).metrics.advance, 0.0);
}

#[test]
fn c07_roll_rotates_the_top_n_operands() {
    // F9: `roll` must rotate the last N values left on the stack. Not the last N
    // minus the two operands it just consumed, and not the window one lower.
    // `10 20 30 40 50 60 4 2 roll` -> 10 20 50 60 30 40 (TN5177 §4.3, op 12 30).
    let mut cs = Vec::new();
    for v in [10, 20, 30, 40, 50, 60] {
        push_int(v, &mut cs);
    }
    push_int(4, &mut cs); // N
    push_int(2, &mut cs); // J
    cs.push(12);
    cs.push(30); // roll
    cs.push(5); // rlineto
    cs.push(14); // endchar
    let data = build_cff(&[14], &cs);

    let got = points(&glyph(&data, 1));
    assert_eq!(
        &got[..3],
        &[(10.0, 20.0), (60.0, 80.0), (90.0, 120.0)],
        "rlineto end points after 10 20 30 40 50 60 4 2 roll"
    );
}

#[test]
fn c07b_roll_may_cover_the_whole_stack() {
    // F9: `n == stack.len()` is legal; only `n > stack.len()` is an error, so the
    // window bound must be inclusive (everything on the stack is rolled).
    let mut cs = Vec::new();
    for v in [10, 20, 30, 40] {
        push_int(v, &mut cs);
    }
    push_int(4, &mut cs); // N == 4 == number of remaining operands
    push_int(2, &mut cs); // J
    cs.push(12);
    cs.push(30); // roll
    cs.push(5); // rlineto
    cs.push(14); // endchar
    let data = build_cff(&[14], &cs);

    let got = points(&glyph(&data, 1));
    assert_eq!(&got[..2], &[(30.0, 40.0), (40.0, 60.0)]);
}

/// `flex1`: dx1 dy1 dx2 dy2 dx3 dy3 dx4 dy4 dx5 dy5 d6 (TN5177 §4.1, op 12 37).
fn flex1_charstring(args: [i32; 11]) -> Vec<u8> {
    let mut cs = Vec::new();
    for v in args {
        push_int(v, &mut cs);
    }
    cs.push(12);
    cs.push(37); // flex1
    cs.push(14); // endchar
    cs
}

#[test]
fn c08_flex1_horizontal_end_point() {
    // F7 is a false positive: 11 arguments are exactly what the code consumes.
    // Sum of the first ten arguments is (50, 25), |dx| > |dy|, so the end point's
    // y-value is the starting y and d6 is a dx.
    let cs = flex1_charstring([10, 5, 10, 5, 10, 5, 10, 5, 10, 5, 10]);
    let got = points(&glyph(&build_cff(&[14], &cs), 1));
    assert_eq!(
        &got[..6],
        &[
            (10.0, 5.0),
            (20.0, 10.0),
            (30.0, 15.0),
            (40.0, 20.0),
            (50.0, 25.0),
            (60.0, 25.0)
        ],
        "flex1, horizontal: dx=50 dominates dy=25, end = (d5.x + d6, y)"
    );
}

#[test]
fn c08b_flex1_vertical_end_point() {
    // Same operator, dy dominant: d6 is a dy and the end point keeps the starting x.
    let cs = flex1_charstring([10, 5, 5, 10, 5, 10, 5, 10, 5, 10, 40]);
    let got = points(&glyph(&build_cff(&[14], &cs), 1));
    assert_eq!(
        &got[..6],
        &[
            (10.0, 5.0),
            (15.0, 15.0),
            (20.0, 25.0),
            (25.0, 35.0),
            (30.0, 45.0),
            (30.0, 85.0)
        ],
        "flex1, vertical: dy=45 dominates dx=35, end = (x, d5.y + d6)"
    );
}

/// `hflex1`: dx1 dy1 dx2 dy2 dx3 dx4 dx5 dy5 dx6 (TN5177 §4.1, op 12 36).
fn hflex1_charstring(args: [i32; 9]) -> Vec<u8> {
    let mut cs = Vec::new();
    for v in args {
        push_int(v, &mut cs);
    }
    cs.push(12);
    cs.push(36); // hflex1
    cs.push(14); // endchar
    cs
}

#[test]
fn c08c_hflex1_ends_on_the_starting_y() {
    // F8: `hflex1` is only legal when the starting and ending points share their
    // y-value, so the last delta is dy6 = -(dy1 + dy2 + dy5), not 0.
    let cs = hflex1_charstring([10, 5, 10, 5, 10, 10, 10, 5, 10]);
    let got = points(&glyph(&build_cff(&[14], &cs), 1));
    assert_eq!(
        &got[..6],
        &[
            (10.0, 5.0),
            (20.0, 10.0),
            (30.0, 10.0),
            (40.0, 10.0),
            (50.0, 15.0),
            (60.0, 0.0)
        ],
        "hflex1: dy1=dy2=dy5=5, so the end point is back at y = 0"
    );
}

#[test]
fn c08d_hflex1_with_no_y_excursion_is_unchanged() {
    // Control: with dy1 = dy2 = dy5 = 0 the correction is a no-op, which is the
    // only case the old `x xy x` reading got right.
    let cs = hflex1_charstring([10, 0, 10, 0, 10, 10, 10, 0, 10]);
    let got = points(&glyph(&build_cff(&[14], &cs), 1));
    assert_eq!(&got[..6], &[
        (10.0, 0.0),
        (20.0, 0.0),
        (30.0, 0.0),
        (40.0, 0.0),
        (50.0, 0.0),
        (60.0, 0.0)
    ]);
}

#[test]
fn c08e_hflex_still_ends_flat() {
    // `hflex` (12 34) is correct in the original code: every y-delta but dy2 is 0.
    let mut cs = Vec::new();
    for v in [10, 10, 5, 10, 10, 10, 10] {
        push_int(v, &mut cs);
    }
    cs.push(12);
    cs.push(34); // hflex
    cs.push(14); // endchar
    let got = points(&glyph(&build_cff(&[14], &cs), 1));
    assert_eq!(&got[..6], &[
        (10.0, 0.0),
        (20.0, 5.0),
        (30.0, 5.0),
        (40.0, 5.0),
        (50.0, 5.0),
        (60.0, 5.0)
    ]);
}

// ---------------------------- F22/F23: decide, don't panic ----------------------------

#[test]
fn c19_unknown_charstring_type_is_an_error() {
    // F23: `CharstringType` is 1 or 2 (CFF §18); anything else must reject the font,
    // not panic inside the interpreter setup.
    let mut extra = Vec::new();
    push_int(3, &mut extra);
    extra.extend_from_slice(&[12, 6]); // CharstringType
    let data = build_cff_full(&extra, true, None, &[14], &[14]);
    assert!(CffFont::parse(&data, 0).is_err(), "CharstringType 3 must be rejected");
}

#[test]
fn c19b_missing_private_dict_is_not_fatal() {
    // F22: `Private` is optional in the Top DICT (CFF §"Top DICT Data"); without it
    // the private dict is empty, so `default_width_x`/`nominal_width_x` are 0.
    let mut cs = Vec::new();
    push_int(500, &mut cs); // delta width
    cs.push(14); // endchar
    let data = build_cff_full(&[], false, None, &[14], &cs);
    let font = CffFont::parse(&data, 0).expect("a font without Private parses");
    assert_eq!(font.glyph(GlyphId(1)).expect("glyph 1").metrics.advance, 500.0);
}

#[test]
fn c19c_unknown_charset_format_is_an_error() {
    // F23: charset formats are 0, 1 and 2 (CFF §"Charset"); format 3 used to panic.
    let tail = [3u8];
    let data = build_cff_full(&[], true, Some(&tail), &[14], &[14]);
    assert!(CffFont::parse(&data, 0).is_err(), "charset format 3 must be rejected");
}

#[test]
fn c19d_index_offset_zero_is_an_error() {
    // F23: INDEX offsets are 1-based (CFF §"INDEX data"); 0 used to underflow in
    // `o - 1`. Corrupt the Name INDEX, which is the first one in the file.
    let mut data = vec![1, 0, 4, 1]; // header
    data.extend_from_slice(&[0, 1, 1, 0, 4]); // count 1, offSize 1, offsets [0, 4]
    data.extend_from_slice(b"Test");
    data.extend_from_slice(&index(&[])); // Top DICT INDEX
    data.extend_from_slice(&index(&[])); // String INDEX
    data.extend_from_slice(&index(&[])); // GlobalSubr INDEX
    assert!(CffFont::parse(&data, 0).is_err(), "INDEX offset 0 must be rejected");
}

// ---------------------------- F11: endchar seac ----------------------------

/// Type 2: `x0 y0 rmoveto` then a rectangle from `(x0, y0)` to `(x1, y1)`, `endchar`.
fn rect2(x0: i32, y0: i32, x1: i32, y1: i32) -> Vec<u8> {
    let mut cs = Vec::new();
    if x0 != 0 || y0 != 0 {
        push_int(x0, &mut cs);
        push_int(y0, &mut cs);
        cs.push(21); // rmoveto
    }
    let (w, h) = (x1 - x0, y1 - y0);
    for &(dx, dy) in &[(w, 0), (0, h), (-w, 0), (0, -h)] {
        push_int(dx, &mut cs);
        push_int(dy, &mut cs);
        cs.push(5); // rlineto
    }
    cs.push(14); // endchar
    cs
}

/// Type 2: `adx ady bchar achar endchar` — the deprecated `seac` form.
fn seac2(adx: i32, ady: i32, base: i32, accent: i32) -> Vec<u8> {
    let mut cs = Vec::new();
    for v in [adx, ady, base, accent] {
        push_int(v, &mut cs);
    }
    cs.push(14); // endchar
    cs
}

#[test]
fn c24_endchar_seac_composes_the_accent() {
    // F11: `endchar` with four arguments (TN5177 Appendix C) was read as a width and
    // dropped, so the glyph came out empty.
    let tail = [0u8, 0, 34, 0, 125, 0, 171]; // charset format 0: /A, /acute, /Aacute
    let charstrings: &[&[u8]] = &[
        &[14u8],
        &rect2(0, 0, 300, 700),
        &rect2(0, 0, 50, 60),
        &seac2(100, 200, 65, 194),
    ];
    let data = build_cff_glyphs(&[], true, Some(&tail), charstrings);
    let font = CffFont::parse(&data, 0).expect("font parses");

    let a = font.gid_for_name("A").expect("/A");
    let acute = font.gid_for_name("acute").expect("/acute");
    let aacute = font.gid_for_name("Aacute").expect("/Aacute");
    let glyph_a = font.glyph(a).expect("glyph A");
    let glyph_accent = font.glyph(acute).expect("glyph acute");
    let composed = font.glyph(aacute).expect("glyph Aacute");

    let base_pts = points(&glyph_a);
    let accent_pts = points(&glyph_accent);
    let composed_pts = points(&composed);
    assert_eq!(composed_pts.len(), base_pts.len() + accent_pts.len());
    // The base is untouched, the accent sits at (adx, ady).
    assert_eq!(&composed_pts[.. base_pts.len()], base_pts.as_slice());
    for (i, &(x, y)) in accent_pts.iter().enumerate() {
        assert_eq!(composed_pts[base_pts.len() + i], (x + 100.0, y + 200.0));
    }
}

/// Type 2 subr bias for a subroutine count (TN5177 §4.4): 107, 1131 or 32768.
fn subr_bias(count: usize) -> i32 {
    if count < 1240 {
        107
    } else if count < 33900 {
        1131
    } else {
        32768
    }
}

#[test]
fn c27_type2_recursive_gsubr_is_rejected() {
    // F43: `callsubr`/`callgsubr` recursed with no depth limit, so a one-entry global
    // subr that calls itself overflowed the native stack and killed the process. The
    // nesting is now capped (`MAX_SUBR_DEPTH`), the charstring fails and the glyph with
    // it — the font reports an error instead of crashing.
    let bias = subr_bias(1); // one global subr -> 0 is `-107 callgsubr`
    let mut subr = Vec::new();
    push_int(-bias, &mut subr); // operand: global subr 0
    subr.push(29); // callgsubr
    // No `return` on purpose: even with one, the recursion would be infinite.

    // Glyph 0 is an empty `.notdef`, glyph 1 calls the subr that calls itself.
    let data = build_cff_glyphs_with_gsubrs(
        &[],
        true,
        None,
        &[subr.as_slice()],
        &[&[14], subr.as_slice()],
    );

    // Parsing must return — the guard fires at depth MAX_SUBR_DEPTH.
    let result = CffFont::parse(&data, 0);
    assert!(
        result.is_err(),
        "a self-recursive global subr must fail the charstring, not hang"
    );
}

#[test]
fn c27b_type2_nested_gsubr_still_runs() {
    // Positive control for the depth guard: a subr calling a (different) subr is legal
    // and must still work.
    let bias = subr_bias(2); // subrs 0 and 1 -> operands -107 (`-bias`) and -106
    let mut subr0 = Vec::new();
    push_int(1 - bias, &mut subr0); // global subr 1
    subr0.push(29); // callgsubr
    subr0.push(11); // return

    let mut subr1 = Vec::new();
    push_int(0, &mut subr1);
    push_int(0, &mut subr1);
    subr1.push(21); // 0 0 rmoveto
    subr1.push(11); // return

    let mut cs1 = Vec::new();
    push_int(-bias, &mut cs1); // global subr 0
    cs1.push(29); // callgsubr
    cs1.push(14); // endchar

    let data = build_cff_glyphs_with_gsubrs(
        &[],
        true,
        None,
        &[subr0.as_slice(), subr1.as_slice()],
        &[&[14], cs1.as_slice()],
    );
    let font = CffFont::parse(&data, 0).expect("two-level subr nesting is legal");
    let glyph = font.glyph(GlyphId(1)).expect("glyph 1");
    assert_eq!(points(&glyph), vec![(0.0, 0.0)]);
}

#[test]
fn c28_type2_deprecated_logic_and_transient_operators() {
    // F42: `and`/`or`/`not`, `put`/`get` and `ifelse` were `error!("unimplemented")`, which
    // failed the whole font — they are deprecated but legal (TN5177 §4.4).
    //
    // The charstring stores the results of the logic operators in the transient array,
    // uses `ifelse` to pick one of two values, and then draws a path whose points are
    // read back with `get`: any wrong result moves a point.
    let mut cs = Vec::new();

    // and: 1 1 -> 1, 1 0 -> 0
    for (a, b, slot) in [(1, 1, 0), (1, 0, 1)] {
        push_int(a, &mut cs);
        push_int(b, &mut cs);
        cs.extend_from_slice(&[12, 3]);
        push_int(slot, &mut cs);
        cs.extend_from_slice(&[12, 20]); // put
    }
    // or: 0 0 -> 0, 0 1 -> 1
    for (a, b, slot) in [(0, 0, 2), (0, 1, 3)] {
        push_int(a, &mut cs);
        push_int(b, &mut cs);
        cs.extend_from_slice(&[12, 4]);
        push_int(slot, &mut cs);
        cs.extend_from_slice(&[12, 20]);
    }
    // not: 1 -> 0, 0 -> 1
    for (a, slot) in [(1, 4), (0, 5)] {
        push_int(a, &mut cs);
        cs.extend_from_slice(&[12, 5]);
        push_int(slot, &mut cs);
        cs.extend_from_slice(&[12, 20]);
    }
    // ifelse: `num1 num2 v1 v2 ifelse` -> num1 when v1 <= v2, else num2 (TN5177 §4.4).
    // Like every CFF operator the *operands* are the two values compared, so the
    // interesting part is that the comparison is on the top pair, not on the bottom one.
    for (num1, num2, v1, v2, slot) in [(0, 1, 3, 7, 6), (2, 1, 7, 3, 7), (4, 5, 5, 5, 8)] {
        for n in [num1, num2, v1, v2] {
            push_int(n, &mut cs);
        }
        cs.extend_from_slice(&[12, 22]);
        push_int(slot, &mut cs);
        cs.extend_from_slice(&[12, 20]); // put
    }

    // 0 0 rmoveto, then three rlineto segments built from the stored values.
    push_int(0, &mut cs);
    push_int(0, &mut cs);
    cs.push(21); // rmoveto
    push_int(0, &mut cs); // get t[0] -> 1
    cs.extend_from_slice(&[12, 21]);
    push_int(6, &mut cs); // get t[6] -> 0 (3 <= 7 -> num1)
    cs.extend_from_slice(&[12, 21]);
    cs.push(5); // rlineto -> (1, 0)
    push_int(4, &mut cs); // get t[4] -> 0 (not 1)
    cs.extend_from_slice(&[12, 21]);
    push_int(5, &mut cs); // get t[5] -> 1 (not 0)
    cs.extend_from_slice(&[12, 21]);
    cs.push(5); // rlineto -> (1, 1)
    push_int(7, &mut cs); // get t[7] -> 1 (7 > 3 -> num2)
    cs.extend_from_slice(&[12, 21]);
    push_int(8, &mut cs); // get t[8] -> 4 (5 <= 5 -> num1)
    cs.extend_from_slice(&[12, 21]);
    cs.push(5); // rlineto -> (2, 5)
    cs.push(14); // endchar

    let data = build_cff(&[14], &cs);
    let glyph = glyph(&data, 1);
    assert_eq!(
        points(&glyph),
        vec![(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (2.0, 5.0)],
        "and 1,1 = 1, and 1,0 = 0, or 0,0 = 0, or 0,1 = 1, not 1 = 0, not 0 = 1, \
         ifelse(0,1,3,7) = 0, ifelse(2,1,7,3) = 1, ifelse(4,5,5,5) = 4"
    );
}

#[test]
fn c28b_type2_transient_index_out_of_range_is_an_error() {
    // The transient array has a fixed size; a font indexing past it gets an error for
    // that glyph instead of an out-of-bounds write.
    let mut cs = Vec::new();
    push_int(1, &mut cs);
    push_int(32, &mut cs); // `put` index 32: the array holds 0 .. 31
    cs.extend_from_slice(&[12, 20]);
    cs.push(14); // endchar

    let data = build_cff(&[14], &cs);
    assert!(CffFont::parse(&data, 0).is_err());
}

#[test]
fn c29_type2_index_with_negative_i_copies_the_top() {
    // F44: TN5177 §4.4 — "if i is negative, the top element is copied"; the operand was
    // cast with `to_usize()`, which errors on a negative number and failed the glyph.
    //
    // `3 -1 index` leaves [3, 3] on the stack (the -1 is popped first, so the copy is
    // the 3), which the rlineto then uses as (dx, dy).
    let mut cs = Vec::new();
    push_int(0, &mut cs);
    push_int(0, &mut cs);
    cs.push(21); // rmoveto
    push_int(3, &mut cs);
    push_int(-1, &mut cs);
    cs.extend_from_slice(&[12, 29]); // index
    cs.push(5); // rlineto
    cs.push(14); // endchar

    let data = build_cff(&[14], &cs);
    let glyph = glyph(&data, 1);
    assert_eq!(points(&glyph), vec![(0.0, 0.0), (3.0, 3.0)]);
}

/// `build_cff_glyphs` plus an `Encoding` entry: `encoding` is appended after the
/// CharStrings INDEX and the TopDICT operand is iterated to its real offset. No
/// `Charset` entry is written, so the predefined ISOAdobe charset applies (SIDs
/// 1..=228, gid == SID).
fn build_cff_with_encoding(encoding: &[u8], char_strings: &[&[u8]]) -> Vec<u8> {
    let mut offset = 0i32;
    let mut data = Vec::new();
    for _ in 0..8 {
        let mut top = Vec::new();
        push_int(offset, &mut top);
        top.push(16); // Operator::Encoding
        data = build_cff_glyphs(&top, true, None, char_strings);
        if data.len() as i32 == offset {
            break;
        }
        offset = data.len() as i32;
    }
    data.extend_from_slice(encoding);
    data
}

#[test]
fn c18_encoding_supplement_may_name_a_missing_glyph() {
    // F18: the supplement was applied with `sid_map[&sid]`, so an SID the charset does
    // not contain aborted the font. TN5176 §"Encoding" allows it: a supplement names
    // glyphs, and a font need not have them. The entry is skipped, the rest applies.
    //
    // Encoding format 0 with the supplement flag: one encoded code (0 -> SID 0) and two
    // supplements — code 65 -> SID 34 ("A", present), code 66 -> SID 500 (absent, and
    // outside the font's empty String INDEX).
    let encoding = [0x80, 1, 0, 2, 65, 0x00, 34, 66, 0x01, 0xF4];
    let cs = vec![14]; // endchar
    let data = build_cff_with_encoding(&encoding, &[&cs, &cs]);

    let font = CffFont::parse(&data, 0).expect("a supplement naming a missing SID is not fatal");
    assert_eq!(font.gid_for_codepoint(65), Some(GlyphId(34)), "known SID still maps");
    assert_eq!(font.gid_for_codepoint(66), None, "the missing SID maps to no glyph");
}

#[test]
fn c17_last_charset_glyph_is_in_the_unicode_map() {
    // F17: the map must cover every charset entry, not stop one short. Three charstrings and
    // no `Charset` entry -> the predefined ISOAdobe charset: gid 1 = SID 1 ("space"),
    // gid 2 = SID 2 ("exclam"), the last glyph in the font.
    let cs = vec![14]; // endchar
    let data = build_cff_glyphs(&[], true, None, &[&cs, &cs, &cs]);
    let font = CffFont::parse(&data, 0).expect("font parses");

    assert_eq!(font.num_glyphs(), 3);
    assert_eq!(font.gid_for_unicode_codepoint(0x20), Some(GlyphId(1)), "\"space\" is gid 1");
    assert_eq!(font.gid_for_unicode_codepoint(0x21), Some(GlyphId(2)), "\"exclam\" is the last glyph");
}

#[test]
fn c17b_zero_char_strings_does_not_panic() {
    // F17: `glyphs.len()-1` underflows for a font with no charstrings. `Cff::slot` rejects
    // such a font outright (`require!(num_glyphs >= 1)`, there is always `.notdef`), so the
    // subtraction is unreachable today; the loop still uses `saturating_sub` and this asserts
    // the guarantee it relies on rather than a panic.
    let data = build_cff_glyphs(&[], true, None, &[]);
    assert!(CffFont::parse(&data, 0).is_err(), "a font without .notdef is rejected");
}

#[test]
fn c47_cff_name_and_weight_come_from_the_name_index_and_top_dict() {
    // F47: the Name INDEX and the Top DICT's FamilyName/FullName/Weight were parsed and
    // thrown away, so a bare CFF font reported no name and no weight at all.
    let mut top_dict = Vec::new();
    for (sid, op) in [(391, 3), (392, 4), (393, 2)] {
        // FamilyName, Weight, FullName
        push_int(sid, &mut top_dict);
        top_dict.push(op);
    }
    let strings: &[&[u8]] = &[b"Helvetia", b"SemiBold", b"Helvetia SemiBold"];
    let data = build_cff_with_strings(&top_dict, strings);
    let font = CffFont::parse(&data, 0).expect("font parses");

    let name = font.name();
    assert_eq!(name.family.as_deref(), Some("Helvetia"), "FamilyName SID 391");
    assert_eq!(name.full_name.as_deref(), Some("Helvetia SemiBold"), "FullName SID 393");
    assert_eq!(name.postscript_name.as_deref(), Some("Test"), "the Name INDEX entry");
    assert_eq!(font.info().weight, Some(600), "\"SemiBold\" is weight class 600");
}

#[test]
fn c47b_cff_without_a_family_name_falls_back_to_the_font_name() {
    // A bare CFF font need not carry FamilyName/Weight; the Name INDEX still gives the
    // PostScript name, and nothing is invented where the Top DICT has no entry.
    let data = build_cff(&[14], &[14]);
    let font = CffFont::parse(&data, 0).expect("font parses");

    let name = font.name();
    assert_eq!(name.postscript_name.as_deref(), Some("Test"));
    assert_eq!(name.family.as_deref(), Some("Test"), "font name before the '-' separator");
    assert_eq!(name.full_name, None);
    assert_eq!(font.info().weight, None);
}
