# `src/opentype/kern.rs` — legacy `kern` table (MS v0 and Apple v1)

## Fixed
* **F32 (2026-11-02):** the MS subtable `coverage` byte is treated as the bit field it is
  (bit 0 horizontal, bit 1 minimum, bit 3 override); only vertical/cross-stream subtables are
  skipped. Unknown formats are skipped with a warning instead of failing the font, a `length`
  below the 6-byte header is an error instead of an underflow, and the Apple path hands the
  bounded subtable slice to `apple_kern_0` instead of the unbounded remainder of the table
  (coverage bit 0x80 = vertical is skipped). Tests: `c15`, `c15b`.

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
* `parse_kern_format2` (`:28-60`): computes the value address as `leftOffset + rightOffset`
  relative to the subtable, which is the documented layout ("address = leftClass value
  (row offset) + rightClass value (column offset)"), but ignores `rowWidth` and prefills
  `glyph_pairs` with `reserve(num_left * num_right)` — for a 5 000-glyph class table that is a
  25 M-entry allocation before a single value is read (**F45**).
* The first `u16` of the Apple header is read as `version` and then re-read as `version` again
  (`:69`/`:73`) — harmless, but the outer `be_u16` only consumes the high half of Apple's 32-bit
  version, which is why both reads exist.

## Cross references
`opentype/gpos.rs.md` (`KernTable`, the class-pair path this table's format 2 should feed
directly rather than exploding into glyph pairs), `truetype.rs.md` (`Font::kerning`).
