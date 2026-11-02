# `src/opentype/mod.rs` — table directory and the sfnt tables shared by glyf/CFF

## Fixed
* **F38 (2026-11-02):** `vmetrics` no longer comes from `hhea` alone. `OpenTypeFont::from_tables`
  parses `OS/2` before the metrics and uses `Os2::typo_metrics()` when the font asks for it
  (`fsSelection` bit 7, USE_TYPO_METRICS), falling back to `hhea`; `sxHeight`, `sCapHeight` and
  `ulUnicodeRange1..4` are passed on to `Info`. Tests: `c25`, `c25b` in `tests/spec_cases.rs`.
* **F28 (2026-11-02):** `parse_hmtx_woff2_format1` computed `maxp.numGlyphs - numberOfHMetrics`
  as `usize` and wrapped to a huge count when the WOFF2 hmtx transform claimed more metrics
  than glyphs (debug: "attempt to subtract with overflow"). It now uses `checked_sub` and
  rejects `numberOfHMetrics == 0` with `Failure(Complete)` before reading any stream; test
  `c24`/`c24b` in `tests/spec_cases.rs`.
* **F30/F31 (2026-11-02):** `parse_hmtx` requires `1 <= numberOfHMetrics <= numGlyphs` and
  reads exactly `numGlyphs - numberOfHMetrics` left side bearings (was `many0`);
  `Hmtx::metrics_for_gid` returns `None` at or beyond `numGlyphs`; `parse_head` checks
  `unitsPerEm ∈ 16..=16384` and `indexToLocFormat ∈ {0,1}`; `parse_maxp` accepts only the
  versions 0.5/1.0 and requires at least one glyph; `parse_loca` requires non-decreasing
  offsets. Tests: `c13`, `c13b`, `c13c`, `c14`, `c14b`, `c16b`.

## What it does
`parse_tables` reads the sfnt directory into a `HashMap<[u8;4], T>`; the rest of the file is the
"base" table set: `Head`, `Maxp`, `Hhea`, `Hmtx`, `parse_loca`, `parse_name`, `parse_vhea`, plus
the GSUB/GPOS common building blocks (`parse_lookup_list`, `coverage_table`,
`parse_class_def`, `Tag`) and the `OpenTypeFont` type that dispatches to glyf or CFF outlines.

## Fixed
* **F3 (2026-11-02):** the GDEF lookup is `b"GDEF"`; `OpenTypeFont::gdef` is populated.

## Differences from the specifications
* **F3 (OT §GDEF):** `:139` looks up `b"gdef"`. The tag is `GDEF`; `Tables::get` is an exact
  `[u8; 4]` match, so GDEF is always `None`. (Same class of bug would apply to any mixed-case
  tag; `OS/2`, `BASE`, `MATH`, `SVG ` are spelled correctly.)
* **F16 (OT §vhea):** `parse_vhea` (`:430-442`) reads one `u16` version field and then takes
  `minorVersion` as `vertTypoAscender`. `vmetrics()` uses `hhea` instead (`:129`), so this is
  latent.
* **F30 (OT §hmtx):** `parse_hmtx` (`:409-421`) reads `numberOfHMetrics` pairs and then
  `many0(be_i16)` over *all remaining bytes* as left-side bearings. The spec fixes the trailing
  count at `numGlyphs - numberOfHMetrics`; extra bytes are accepted, missing ones silently become
  0 in `Hmtx::metrics_for_gid` (`:398-406`). `numberOfHMetrics` is never validated against
  `maxp.numGlyphs` (must be `1..=numGlyphs`). `metrics_for_gid(gid)` answers for `gid >=
  numGlyphs` instead of `None`.
* **F31 (OT §head, §maxp, §loca):** `parse_head` checks version `1.0`, the `0x5F0F3CF5` magic and
  `glyphDataFormat == 0` — good — but not `unitsPerEm ∈ 16..=16384`, so a `unitsPerEm` of 0 makes
  `font_matrix()` divide by zero. `parse_maxp` ignores the version (0.5 vs 1.0) and `numGlyphs`
  is never cross-checked against `loca`/CFF CharStrings/`post`. `parse_loca` reads
  `numGlyphs + 1` entries but does not check monotonicity, alignment (`indexToLocFormat == 0`
  ⇒ offsets are even) or a final entry equal to the `glyf` length.
* **F33 (OT §name):** `parse_name` accepts platform 0 (any encoding), 3/0 and 3/1; it ignores
  platform 1 (Macintosh Roman) and 3/10 (UCS-4), and takes the *last* record that matches instead
  of applying the spec's platform/encoding/language preference order. Name IDs handled: 1, 2, 4,
  6 (family, subfamily, full name, PostScript name). Format 1's `langTagRecord` array is not
  validated (and is not needed).
* **D2:** the table directory's `searchRange`/`entrySelector`/`rangeShift`, 4-byte alignment of
  table offsets and `checkSumAdjustment` are not validated (OT §"Table Directory", `head`).
  Duplicate tags overwrite each other in the `HashMap`.
* **D1:** `Tables::get` filters out zero-length tables (`:262-264`) while `contains()` returns
  `true` for them — a zero-length optional table (`kern`, `post`, `cmap`) behaves inconsistently.
* `coverage_table` (OT §"Coverage table") format 1 ✓; format 2 ignores `startCoverageIndex` and
  simply numbers `start..=end`, which is only right when the ranges are contiguous *and* sorted.
  No check for either, so an unsorted or gapped Coverage produces wrong lookup indices instead of
  an error.
* `parse_class_def` (OT §"Class Definition table") format 1/2 ✓, but format 2 expands
  `start..=end` with no bound and no `end >= start` check (a corrupt record can allocate
  gigabytes).
* `parse_lookup_list` returns subtable data as `&data[subtableOffset..]` for each lookup; it does
  not honour the lookup `LookupFlag` (ignored) nor the `Extension` indirection (GSUB 7 / GPOS 9).
* `parse_name` decodes strings with `utf16_be` that uses `chunks_exact(2)`, so an odd trailing
  byte is dropped instead of erroring (harmless).

## Cross references
`opentype/base.rs.md` (BASE, unused), `opentype/cmap.rs.md`, `opentype/gpos.rs.md`,
`opentype/gsub.rs.md`, `opentype/gdef.rs.md`, `opentype/kern.rs.md`, `opentype/post.rs.md`,
`opentype/os2.rs.md`.
