# `src/truetype.rs` — `glyf`/`loca` outlines and composite glyphs

## Fixed
* **F38 (2026-11-02):** `TrueTypeFont` now implements `vmetrics()` (it returned the trait default,
  `None`, before): `OS/2` `sTypo*` when `fsSelection` bit 7 asks for it, `hhea` otherwise, and
  `sxHeight`/`sCapHeight`/`ulUnicodeRange1..4` go into `Info`.
* **F19/F20 (2026-11-02):** `compound()` no longer panics when `ARGS_ARE_XY_VALUES` is clear —
  the arguments are stored as `ComponentOffset::MatchPoints { parent, child }` and resolved in
  `get_outline` once the component outline exists (`spec_points`/`point_at` count §glyf point
  numbers, ignoring the explicit closing point `contour()` appends). `get_outline` is now a
  recursive `expand_outline` with a stack, so nested composites are expanded, missing children
  and self-references produce an empty or partial outline with a warning, and nesting is capped
  at `MAX_COMPONENT_DEPTH` (16). `Shape::Compound` holds `Vec<Component>` (glyph id, 2×2 matrix,
  `ComponentOffset`). Tests: `c17a`, `c17b`, `c17c`.
* **F31 (2026-11-02):** `parse_shapes` checks that the last `loca` offset stays inside `glyf`
  and turns an unparseable or out-of-range glyph record into `Shape::Empty` (with a warning)
  instead of failing the whole font. `TrueTypeFont::num_glyphs()` answers `maxp.numGlyphs`,
  the same number as `OpenTypeFont`, not `shapes.len()`. Tests: `c16`, `c16b`, `c16c`.

## What it does
Parses the `glyf` table from a `loca` offset array into `Shape::{Simple(Outline),
Compound(Vec<Component>), Empty}`, resolves composites recursively in `get_outline`, and
implements `Font` for a pure-TrueType font (`head`, `maxp`, `hhea`, `hmtx`,
`cmap`, `post`, `kern`, `OS/2`).

The simple-glyph decoder is derived from stb_truetype and is correct in the parts that matter:
flag repeat counts (`flagcount + 1` total, clamped to the point count), the four
`parse_coord` cases (including the "same as previous" case with no data), on-curve/off-curve
midpoint insertion, and closing the contour with a quadratic back to the start point
(OT §glyf "Simple glyph description").

## Differences from the specifications
* **F19/F20** (panic on point matching; nested composites dropped) — fixed above.
* Composite flags handled: `ARGS_ARE_WORDS` (0x0001) ✓, `WE_HAVE_A_SCALE` (0x0008) ✓,
  `MORE_COMPONENTS` (0x0020) ✓, `WE_HAVE_AN_X_AND_Y_SCALE` (0x0040) ✓, `WE_HAVE_A_TWO_BY_TWO`
  (0x0080) ✓ with the spec's `xscale, scale01, scale10, yscale` order. Ignored:
  `ROUND_XY_TO_GRID` (0x0004), `WE_HAVE_INSTRUCTIONS` (0x0100, harmless — we stop reading),
  `USE_MY_METRICS` (0x0200), `OVERLAP_COMPOUND` (0x0400),
  `SCALED_COMPONENT_OFFSET`/`UNSCALED_COMPONENT_OFFSET` (0x0800/0x1000). Not scaling the offset
  matches the documented default on Apple/Microsoft, so that part is fine; `USE_MY_METRICS`
  would be a real (minor) metrics difference for a few fonts.
* `numberOfContours` other than `-1` for a composite is rejected (`:213`). The spec says any
  negative value *may* appear ("the value -1 should be used"), so a font using `-2` is rejected.
* **F31:** `num_glyphs()` returns `shapes.len()` (derived from `loca`) rather than
  `maxp.numGlyphs`; `loca` is not checked for monotonicity or for ending at the `glyf` length,
  and a single bad `loca` entry fails the whole font (`:203-208` uses `expect!` on
  `data.get(start..end)`).
* `is_empty_glyph` (`:145-155`) returns `false` for a composite whose components are all empty.
* `parse_coord(false, true)` (x/y not short, "same" set) correctly yields 0.

## Audit PR #7 (2026-11-03)
* **C8b (component flags):** `SCALED_COMPONENT_OFFSET` (0x0800) and `UNSCALED_COMPONENT_OFFSET`
  (0x1000) are read and applied: the Apple-style scaled offset, the Microsoft default of an
  offset applied after the matrix.
* **C8e (point numbering):** `spec_points()` no longer drops the last point when it equals the
  first, so point-number matching of a composite glyph uses the original glyf numbers.
* **§3 (expansion budget):** `expand_outline` spends a `ComponentBudget` of
  `MAX_COMPONENT_EXPANSIONS = 10_000` component expansions, so a glyph with k components nested
  `MAX_COMPONENT_DEPTH = 16` deep can no longer fan out to k¹⁶.
* **§4 (loca):** a last offset past the end of `glyf` only warns now; the glyphs it covers are
  read as empty, which is what the per-glyph branch was for.

## Cross references
`lib.rs.md` (shared `State`), `opentype/mod.rs.md` (`parse_loca`, `parse_hmtx`, `Head`),
`woff.rs.md` (the WOFF2 *transformed* glyf reuses `compound`/`contour`).
