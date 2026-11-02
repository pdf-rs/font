# `src/opentype/cmap.rs` — character to glyph mapping

## What it does
Parses the `cmap` table into a `CMap { single_codepoint: HashMap<u32,u32>, double_codepoint:
HashMap<(u32,u32),u32> }` (the second map holds Unicode Variation Sequences), picks an
`Encoding` (Unicode, or MacRoman when only platform 1 is present), and exposes
`get_codepoint`, `get_pair`, `items`, `codepoints`.

Formats implemented: 0, 4, 6, 12, 14. The platform/encoding filter
(`(0,_) | (3,0) | (3,10) | (3,1)` → Unicode, `(1,0)` → MacRoman if no Unicode subtable) matches
OT §cmap's "platform/encoding priority" intent.

## Differences from the specifications
* **F12 (fixed 2026-11-02):** an unsupported subtable format aborted the whole table with
  `error!("cmap format {}", n)` (the crate's own `error!` returns `Err`). Format 2 (high-byte
  mapping, all CJK legacy fonts), 8 (mixed 16/32-bit) and 13 (last resort) are legal and common,
  so a font carrying one of them *and* a good format-4 subtable became unloadable. The format is
  now read and checked before the encoding bookkeeping, an unsupported one is skipped
  (`continue`) with a `warn!`, and the catch-all arm is a `warn!` too.
  Tests: `c11`, `c11b`.
* **F13 (fixed 2026-11-02):** `CMap::codepoints()` never pushed the final run, so a contiguous
  cmap yielded an empty list; the merging loop dropped `prev` the same way, computed gaps as
  `b.start - a.end` instead of the number of codepoints missing between the runs, and
  `tuple_windows().min().unwrap()` panicked once nothing was left to merge. It now sorts/dedups,
  builds maximal runs (one `last_mut()` walk), and repeatedly merges the pair with the smallest
  real gap until `max_ranges` is met (`max_ranges` is clamped to at least 1).
  Tests: `c10`, `c10b`, `c10c`.
* **F14:** format 4's `idRangeOffset` addressing (`:177-186`) is done in `u16`:
  `2 * (n as u16 + (c - start)) + offset - segCountX2` wraps/overflows for a malformed subtable
  (panic in debug, wrong glyph in release). The formula itself is right — `glyphIdArray` starts
  `segCountX2` bytes after the `idRangeOffset` array — and the bound check
  `index > glyph_data.len() - 2` is correct but underflows when the array is shorter than 2
  bytes. Also missing: `segCountX2` even check, `searchRange`/`entrySelector`/`rangeShift`
  consistency, and the spec's requirement that the last segment is `0xFFFF..0xFFFF` (the code
  `break`s on it, which is fine, but does not reject a missing one).
* Format 0 (`:130-141`): `take(len - 4)` underflows for `len < 4`; the byte count is not checked
  against the 256-entry table.
* Format 12 (`:200-212`): groups are expanded one codepoint at a time with no check that groups
  are sorted and non-overlapping (OT §cmap: "groups must be sorted by increasing startCharCode"),
  and `len` is not compared with the bytes available. Format 13 (many codepoints → one glyph) is
  not implemented.
* Format 14 (`:213-237`): the `defaultUVS`/`nonDefaultUVS` offsets are correctly taken relative to
  the subtable start, and default UVS are resolved through the *base* cmap as the spec requires.
  Missing: `length` validation (`slice!(i, .. length - 6)` underflows) and the offsets are not
  bounded by the subtable length.
* `parse_cmap` returns `Encoding::Unicode` when no usable subtable was found
  (`cmap_encoding.unwrap_or(Encoding::Unicode)`), i.e. the font claims Unicode encoding with an
  empty map. `pdf`'s font code then maps nothing instead of falling back to a built-in encoding.
* Symbol fonts (platform 3, encoding 0) are treated as plain Unicode; the spec's "code 0xF000 +
  c" symbol convention is not applied. This is usually what you want for PDF, but it is a
  difference worth stating.

## Cross references
`opentype/mod.rs.md` (Coverage/ClassDef), `cff.rs.md` (CFF has its own 8-bit encoding),
`type1.rs.md` (Type 1 `/Encoding`).
