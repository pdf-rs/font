# `src/opentype/kern.rs` — legacy `kern` table (MS v0 and Apple v1)

## Fixed
* **F32 (2026-11-02):** the MS subtable `coverage` byte is treated as the bit field it is
  (bit 0 horizontal, bit 1 minimum, bit 3 override); only vertical/cross-stream subtables are
  skipped. Unknown formats are skipped with a warning instead of failing the font, a `length`
  below the 6-byte header is an error instead of an underflow, and the Apple path hands the
  bounded subtable slice to `apple_kern_0` instead of the unbounded remainder of the table
  (coverage bit 0x80 = vertical is skipped). Tests: `c15`, `c15b`.
* **F45 (2026-11-03):** format 2 no longer explodes into a per-glyph-pair map. The two class
  tables are read into `KernTable::left_classes`/`right_classes` (glyph → its row / column
  offset) and the values into `class_pairs`, so the storage is the size of the array the subtable
  actually carries (`distinct rows × distinct columns`) instead of `num_left_glyphs *
  num_right_glyphs` — the old code also `reserve`d the whole cross product up front, so a
  legitimate 5 000-glyph class table allocated 25 M map entries. `KernTable::get` already falls
  back to the class maps, so lookups are unchanged. Unit tests in `kern.rs`: `kern_format2_maps`
  and `kern_format2_memory_follows_the_array_not_the_glyph_count` (5 000 glyphs sharing one row →
  `class_pairs.len() == 2`, no glyph pairs).

## What it does
`kern` version 0 (Microsoft) → subtable formats 0 (sorted glyph pairs) and 2 (class matrix);
version 1 (Apple, `0x00010000`) → 32-bit subtable headers, format 0 only. Both fill the same
`KernTable` as GPOS (`gpos.rs`), so `Font::kerning()` is uniform.

## Differences from the specifications
* **F32 (OT §kern, "Coverage"):** the MS subtable is only accepted when the coverage byte is
  *exactly* `0x01` (`:145-148`). Coverage is a bit field — `0x01` horizontal, `0x02` minimum,
  `0x04` cross-stream, `0x08` override — so a horizontal subtable that also sets "minimum" or
  "override" (`0x03`, `0x09`, `0x0B`) is rejected, and because the error propagates out of
  `TrueTypeFont::from_shapes_and_metrics` the **whole font fails to load**. Nor is
  `is_horizontal = coverage & 0x01 != 0` checked before use, so a vertical-only subtable would
  be applied as horizontal kerning if it got through.
* `length - 6` (`:141`) underflows for a subtable shorter than its 6-byte header.
* `parse_kern_apple` (`:87-92`): `let (new_input, data) = take(length)(i)?;` then calls
  `apple_kern_0(i, …)` with the **unbounded** `i` instead of `data`, so a short Apple subtable
  reads into its successor; `tuple_index` (variation index, for `morx`/variable fonts) is read
  and ignored.
* Apple format 2/3 subtables (class matrix / index arrays) are `error!`; only format 0 works.
* ~~**F45:**~~ `parse_kern_format2` used to compute the value address as `leftOffset +
  rightOffset` (still the documented layout) but expand the whole glyph × glyph cross product
  into `glyph_pairs` and `reserve(num_left * num_right)` — for a 5 000-glyph class table that is a
  25 M-entry allocation before a single value is read. It fills the class maps instead now, see
  "Fixed" above. `rowWidth` is still ignored: the right-hand class values are the column offsets
  inside the row, so only `left + right` is needed.
* The first `u16` of the Apple header is read as `version` and then re-read as `version` again
  (`:69`/`:73`) — harmless, but the outer `be_u16` only consumes the high half of Apple's 32-bit
  version, which is why both reads exist.

## Audit PR #7 (2026-11-03)
* **C5 (format 2):** a left or right "class" is a byte offset, and offset 0 means "not in a
  class" — it is skipped instead of being used as a class index, so `address = 0 + col` no
  longer reads the subtable header as kerning. `left_first + n` is a `checked_add`, and an
  unreadable `(row, column)` pair is skipped rather than failing the whole table.

## Cross references
`opentype/gpos.rs.md` (`KernTable`, whose class-pair path this table's format 2 now feeds
directly), `truetype.rs.md` (`Font::kerning`).
