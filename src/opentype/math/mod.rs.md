# `src/opentype/math/mod.rs` — MATH table

## What it does
A table-driven transcription of OT §MATH: `MathHeader` (1.0, `MathConstants`, `MathGlyphInfo`,
`MathVariants` offsets), `MathConstants` (all ~56 values: percent scale downs, min heights, and
the long list of `MathValueRecord` shifts/gaps), `MathGlyphInfo` (italics correction, top accent,
extended shape coverage, math kern), `MathVariants` (min connector overlap, vertical/horizontal
glyph construction with variant records and glyph assemblies).

`MathValueRecord` reads `value` and drops the `deviceTableOffset` (`:29-35`) — appropriate, since
device tables are hinting-only. Field order for the constants matches the spec (spot-checked
against the OT §MATH "MathConstants table": `scriptPercentScaleDown`,
`scriptScriptPercentScaleDown`, `delimitedSubFormulaMinHeight`, `displayOperatorMinHeight`, …).

## Differences from the specifications
* `parse_math` never checks `majorVersion == 1`/`minorVersion == 0` — a future MATH 2.0 would be
  parsed as 1.0 and silently misread.
* The three header offsets are parsed with the mandatory (non-optional) `@uint16` form, so a MATH
  table with a zero `MathGlyphInfo` offset (which the spec allows to omit? — it is listed as
  required, so this is acceptable) errors out.
* Feature-gated behind `math` (`Cargo.toml`), off by default; `pdf`/`pdf_render` never read it.
  Only `pdf-render`-level math typesetting would, and there is none.

## Cross references
`opentype/math/assembly.rs.md` (the variant construction algorithm).
