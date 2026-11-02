# `src/opentype/gpos.rs` — GPOS: adjustments, kerning and mark attachment

## What it does
`parse_gpos` walks the lookup list and keeps what the rest of the crate can use: a `KernTable`
(glyph pairs + class pairs, `KernTable::get(left, right)`) filled from **PairPos** lookups
(type 2), a `HashMap<(base_gid, mark_gid), (dx, dy)>` from **MarkBasePos** lookups (type 4) and
**MarkMarkPos** (type 6, `GPos::mark_to_mark`), `GPos::single` with the full `ValueRecord` of
each glyph from **SinglePos** (type 1), `GPos::cursive` with the entry/exit anchors from
**CursivePos** (3) and `GPos::mark_to_ligature` from **MarkLigPos** (5, first component).
`GPos::from_kern` lets a `kern` table stand in when GPOS is absent (`mod.rs:110-117`).

## Fixed
* **F36 (2026-11-02):** single adjustment (1, formats 1 and 2), cursive attachment (3),
  mark-to-ligature (5) and mark-to-mark (6) are parsed now, each with an accessor
  (`get_single`, `get_cursive`, `get_mark_to_ligature`, `get_mark_to_mark`) and with the
  `ValueRecord` fields public. MarkBasePos and MarkMarkPos share one parser
  (`parse_mark_attachment`) because their subtables have the same layout; mark-to-ligature uses
  the first component's anchors, which is where a mark following the whole ligature goes.
  Tests: `single_adjustment_format2_reads_one_record_per_glyph`,
  `cursive_attachment_reads_entry_and_exit_anchors`,
  `mark_to_mark_attachment_fills_its_own_map`,
  `mark_to_ligature_attachment_uses_the_first_component`.
* **F36 (2026-11-02):** malformed tables no longer abort the process. PairPos format 2 computed
  `class_1_count * class_2_count` in `u16` (300 × 300 classes overflow — a debug panic) and
  reserved that many map entries up front; the count is now `usize`, clamped to the bytes the
  subtable actually has, and the reservation is capped. A mark whose `markClass` is not below
  `markClassCount` used to fail the whole GPOS table via `require!`; it now warns and skips that
  mark, as does an anchor index past the end of the base array. Tests:
  `pair_adjustment_with_huge_class_matrix_does_not_overflow`,
  `mark_in_an_unknown_class_is_skipped_not_fatal`.
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
* In PairPos only the **first** `ValueRecord`'s `xAdvance` is used (`:130`, `:158`);
  `ValueRecord2` (the second glyph's advance/placement) and `xPlacement`/`yPlacement` are
  discarded. For PDF text that is defensible (you only need the advance delta), but it is not
  what GPOS says. SinglePos keeps the whole record.
* ~~**F36 (OT §GPOS, "Extension lookups"):**~~ lookup type 9 (Extension) was not followed — the
  dispatch (`parse_positioning`) now unwraps it and parses the wrapped pair adjustment or
  mark-to-base subtable; an extension wrapping an extension warns and is dropped. Unit test:
  `extension_lookup_reaches_pair_adjustment`.
* Contextual positioning (7) and chaining contextual positioning (8) are still skipped without a
  warning — the crate has no positioning engine that could run the lookups their rules name.
* ~~Lookup types 1 (single), 3 (cursive), 5/6 (mark-to-ligature, mark-to-mark), 7 (context),
  8 (chaining context) are skipped without a warning.~~ 1, 3, 5 and 6 are parsed now (see
  "Fixed"); 7 and 8 are not.
* `ValueRecord` correctly skips the device-table offsets (`(flags & 0x00F0).count_ones() * 2`) but
  does not read them — hinting-only, fine for outlines.
* MarkBasePos: format must be 1 ✓; mark/base coverage, `markClassCount`, mark array
  and base array are handled with the right offset bases; a `markClass` outside
  `markClassCount` (and an anchor index past the base array) is warned about and skipped per mark
  (see "Fixed", F36). `parse_base_array` computes `base_count * mark_class_count` entries ✓.
  `AnchorTable` reads only `format, x, y`, which is correct for formats 1-3 (the extra fields
  come after).
* **F5/F21:** `coverage_off.of(data).unwrap()` (`:121`), `coverage.next().unwrap()` (`:123`) and
  `panic!("unsupported GPOS version 1.{}", v)` (`:64`) — all reachable from a malformed font.
* The version-1.1 `featureVariationsOffset` is read as `u32` ✓ (OT §GPOS header), and then
  ignored.

## Cross references
`opentype/mod.rs.md` (`parse_lookup_list`, `coverage_table`, `parse_class_def`),
`opentype/kern.rs.md` (the legacy table that fills the same `KernTable`).
