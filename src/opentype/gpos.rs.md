# `src/opentype/gpos.rs` — GPOS: pair kerning and mark-to-base attachment

## What it does
`parse_gpos` walks the lookup list and keeps two things: a `KernTable` (glyph pairs + class
pairs, `KernTable::get(left, right)`) filled from **PairPos** lookups (type 2), and a
`HashMap<(base_gid, mark_gid), (dx, dy)>` filled from **MarkBasePos** lookups (type 4).
`GPos::from_kern` lets a `kern` table stand in when GPOS is absent (`mod.rs:110-117`).

## Fixed
* **F36 (2026-11-02):** lookup type 9 (Extension) is followed: `parse_positioning` reads the
  extension header and dispatches to the wrapped lookup type, so pair kerning hidden behind an
  extension — what most large fonts do — is reachable. An extension wrapping an extension warns
  and is dropped. Unit test: `extension_lookup_reaches_pair_adjustment`.
* **F27 (2026-11-02):** the two `.unwrap()`s on the PairPos format 1 coverage offset/iterator are
  gone: a zero or out-of-bounds coverage offset is an error, and a coverage table shorter than
  `pairSetCount` stops the loop instead of panicking.
* **F21 (2026-11-02):** an unknown `minorVersion` (`v => panic!("unsupported GPOS version
  1.{}")`) is now a warning: the header fields before the feature variations offset are the same
  in every 1.x version, so the table is still read. Tests: `c18`, `c18b`.
* **F1 (2026-11-02):** the PairPos format 1 header no longer reads a phantom `pairSetOffsets`
  field before the real array; the first pair set offset is the one the spec puts there, so
  format 1 kerning works.
* **F2 (2026-11-02):** PairPos format 2 parses `classDef1Offset`/`classDef2Offset` into
  `KernTable::left_classes`/`right_classes` (via `parse_class_def`), so the class matrix is
  reachable from `KernTable::get`.

## Differences from the specifications
* Only the **first** `ValueRecord`'s `xAdvance` is used (`:130`, `:158`); `ValueRecord2`
  (the second glyph's advance/placement) and `xPlacement`/`yPlacement` are discarded. For PDF
  text that is defensible (you only need the advance delta), but it is not what GPOS says.
* ~~**F36 (OT §GPOS, "Extension lookups"):**~~ lookup type 9 (Extension) was not followed — the
  dispatch (`parse_positioning`) now unwraps it and parses the wrapped pair adjustment or
  mark-to-base subtable; an extension wrapping an extension warns and is dropped. Unit test:
  `extension_lookup_reaches_pair_adjustment`.
* Lookup types 1 (single), 3 (cursive), 5/6 (mark-to-ligature, mark-to-mark), 7 (context),
  8 (chaining context) are skipped without a warning.
* `ValueRecord` correctly skips the device-table offsets (`(flags & 0x00F0).count_ones() * 2`) but
  does not read them — hinting-only, fine for outlines.
* MarkBasePos (`:190-215`): format must be 1 ✓; mark/base coverage, `markClassCount`, mark array
  and base array are handled with the right offset bases; `require!(mark_class < mark_class_count)`
  ✓. `parse_base_array` computes `base_count * mark_class_count` entries ✓. `AnchorTable` reads
  only `format, x, y`, which is correct for formats 1-3 (the extra fields come after).
* **F5/F21:** `coverage_off.of(data).unwrap()` (`:121`), `coverage.next().unwrap()` (`:123`) and
  `panic!("unsupported GPOS version 1.{}", v)` (`:64`) — all reachable from a malformed font.
* The version-1.1 `featureVariationsOffset` is read as `u32` ✓ (OT §GPOS header), and then
  ignored.

## Cross references
`opentype/mod.rs.md` (`parse_lookup_list`, `coverage_table`, `parse_class_def`),
`opentype/kern.rs.md` (the legacy table that fills the same `KernTable`).
