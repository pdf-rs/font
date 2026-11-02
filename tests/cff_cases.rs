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
/// CharStrings INDEX. The TopDICT holds only `Private [0,0]` and `CharStrings`,
/// so the private dict is empty and every glyph falls back to width 0.
fn build_cff(cs0: &[u8], cs1: &[u8]) -> Vec<u8> {
    let name_index = index(&[b"Test".as_ref()]);
    let empty = index(&[]);
    let charstrings = index(&[cs0, cs1]);

    // The CharStrings operand is an absolute offset into the CFF data, and its
    // own encoding length depends on its value: encode, measure, repeat.
    let mut offset_value = 0i32;
    let mut top_dict = Vec::new();
    let mut top_dict_index = Vec::new();
    for _ in 0..8 {
        top_dict.clear();
        push_int(0, &mut top_dict);
        push_int(0, &mut top_dict);
        top_dict.push(18); // Operator::Private
        push_int(offset_value, &mut top_dict);
        top_dict.push(17); // Operator::CharStrings
        top_dict_index = index(&[top_dict.as_slice()]);
        let actual = (4 + name_index.len() + top_dict_index.len() + empty.len() * 2) as i32;
        if actual == offset_value {
            break;
        }
        offset_value = actual;
    }

    let mut data = vec![1, 0, 4, 1]; // major, minor, hdrSize, offSize
    data.extend_from_slice(&name_index);
    data.extend_from_slice(&top_dict_index);
    data.extend_from_slice(&empty); // String INDEX
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
