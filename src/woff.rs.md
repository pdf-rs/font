# `src/woff.rs` — WOFF 1.0 and WOFF2 wrappers (plus the transformed `glyf` decoder)

## Fixed
* **F26 (2026-11-02):** no panic on either WOFF path any more. The two decompression
  `.expect()`s became `FontError::Other` carrying the table tag; `flavor == "ttcf"` errors out
  instead of `unimplemented!()`; a `n_contour` outside `{-1, 0, >0}` is an `error!`; the WOFF2
  `bbox` size uses `checked_sub`; a glyph stream that ends mid-triplet or a flag stream that ends
  before its points are `error!` instead of an index panic. The feature **did not compile at all**
  before (a `?` inside an `Iterator::map` closure in `parse_glyf_t0`), so `--features woff` was
  dead code; it now builds. Tests: `tests/woff_cases.rs` `c23`, `c23b`, `c23c`.

## What it does
* `parse_woff`: `wOFF` header (flavor, length, numTables, totalSfntSize, versions,
  meta/priv offsets), the 20-byte directory entries, then zlib-inflate or copy each table, build a
  `Tables<Cow<[u8]>>` and hand it to `OpenTypeFont::from_tables`.
* `parse_woff2`: `wOF2` header, the variable-length table directory (255UInt16 tag index, flags,
  origLength, optional transformLength), one Brotli stream for all tables, then the two WOFF2
  transforms this crate understands: `hmtx` (flag 1 → `parse_hmtx_woff2_format1`, omitted
  left-side bearings / constant advance) and `glyf` (flag 0 → `parse_glyf_t0`, the transformed
  glyf with separate contour/point/flag/glyph/composite streams and the 128-entry triplet
  encoding; flag 3 → the ordinary `loca`+`glyf` path).
* `TRIPLET_LUT` and `Triplet` decode the WOFF2 point encoding: byte count in 2 bits, x/y bit
  widths in units of 4, 11-bit biases, and one sign bit each.

## Differences from the specifications
* **F46 (W3C WOFF 1.0 §"WOFF Header"/"Table Directory"):** the directory `offset` field is read
  into `WoffDirEntry` and then **ignored** — tables are read sequentially after the directory.
  WOFF requires tables to be stored in the directory's order, so this works for conforming fonts,
  but padding/gaps or a reordered directory silently shift every table. Also unvalidated:
  `length`, `totalSfntSize`, per-table `origChecksum`, the metadata/private blocks.
* **F26:** (fixed above — the two decompression panics, the `ttcf` `unimplemented!()`, the
  `n_contour` panic and the WOFF2 stream-end panics.)
* WOFF2 (`:100-160`): the table directory is stored in an `IndexMap` (`count_map`), so the
  *order* is preserved and the sequential read from the Brotli stream matches the directory
  order — that part is right. But the spec allows a collection directory (`ttcf`) plus a
  `CollectionHeader`, which is not handled, and `totalCompressedSize` is not cross-checked.
* WOFF2 `is_null_transform` (`:243-250`): `glyf`/`loca` treat only flag 3 as "untransformed" and
  `hmtx` only flag 0 — that matches the spec's per-table transform-version table (glyf: 0 =
  transformed, 3 = null; hmtx: 0 = null, 1 = transformed). Other tables must use 0.
* `parse_glyf_t0` (`:171-217`): `n_contour_stream` offsets are **points** for the transformed
  format, and the code takes `n_contour` *triplets* per contour, using flag bit 0x80 as
  "off-curve" — this matches the WOFF2 transformed-glyf description. `bbox`/`instruction` streams
  are read and discarded (fine for outlines). A `n_contour` outside `{-1, 0, >0}` panics
  (`:213`).
* `parse_hmtx_woff2_format1` (`opentype/mod.rs:423-450`): the flags' bit 0 (constant advance) uses
  `head.x_min` as every left-side bearing — per spec the transformed form omits the advance array
  entirely when bit 0 is clear and the *lsb* array when bit 1 is set; the implementation follows
  that shape but `maxp.num_glyphs - num_metrics` underflows and panics when
  `numberOfHMetrics > numGlyphs`.
* No WOFF2 `hmtx` transform for `vmtx` (proportional to the above), and no support for the
  `glyf` transform's `overlapSimpleBitmap` flags beyond the 0x80 bit.

## Cross references
`opentype/mod.rs.md` (`Tables`, `parse_hmtx*`), `truetype.rs.md` (`compound`, `contour`,
`parse_shapes` are reused by the WOFF2 transformed path), `woff_lut.rs.md` (duplicate LUT).
