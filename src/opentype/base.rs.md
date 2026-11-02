# `src/opentype/base.rs` — BASE table

## What it does
Parses the BASE table (OT §BASE): header (`majorVersion`, `minorVersion`, `horizAxis`,
`vertAxis`), `BaseTagList`, `BaseScriptList` → `BaseScriptTable` → `BaseValues` → `BaseCoord`.

## Differences from the specifications
* The parsed result is discarded: `parse_base` returns `()` and `OpenTypeFont::from_...` stores
  nothing (`mod.rs:132`). The table is effectively only *validated*, and even that is partial.
* `parse_base_script_list` (`:41-53`) reads `defaultMinMaxOffset` and never follows it, so
  `MinMax` records (featMinMax), used for optical-size style baseline shifts, are not read.
* `parse_base_coord` (`:76-81`) reads only `format` and `x`; `BaseCoordFormat2` (reference
  glyph + point) and `Format3` (device offset) additionally carry `referenceGlyph`/`deviceTable`
  and are not read — fine for validation, wrong if the value is ever used.
* `require_eq!(major, 1)` on the version is stricter than the spec allows (a 1.x BASE with a
  higher minor version parses fine, but only because the minor version is not checked).
* `parse_axis_table` requires both `BaseTagList` and `BaseScriptList` offsets to be non-zero
  (`offset!` → `slice!`), and a zero offset means "absent" per the spec; an absent list therefore
  produces an `OutOfBounds` error rather than "nothing to do".

## Status
No behavioural impact today (nothing consumes BASE). Documented so that a later
"baseline positioning" feature does not trust it.
