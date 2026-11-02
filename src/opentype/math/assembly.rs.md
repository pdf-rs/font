# `src/opentype/math/assembly.rs` — MATH glyph assembly (variant construction)

## What it does
Implements the OT §MATH "GlyphAssembly" algorithm: `vert_variant(gid, size)` /
`horz_variant(gid, size)` return either a plain replacement glyph or a `Constructable`
instruction list (`gid` + `overlap`) for a stack of parts, choosing between
`smallest_upper_bound` (vertical) and `greatest_lower_bound` (horizontal), interpolating the
connector overlap between `maxOverlap` and `minConnectorOverlap`.

## Differences from the specifications
* **Panics on optional-but-empty data:** `vert_variant` (`:46`) calls
  `construction.variants.last().unwrap()`, and `horz_variant` (`:62`) indexes
  `construction.variants[0]`; both panic when the `MathGlyphConstruction` has zero
  variant records. The spec does not forbid an empty `variants` array.
* **Division by zero:** `smallest_upper_bound` (`:139-175`) divides by `(large - small)` and by
  `opt_large`; when the optional parts have zero advance (`opt_large == 0`) or a construction has
  a single size (`large == small`), that is `0/0` → `NaN` → `diff_ratio as u16 == 0`, silently
  producing a wrong assembly.
* `smallest_upper_bound`'s first branch returns `(0, 0.0)` and comments "using smallest variant
  glyph" — the caller then builds an assembly with `repeats = 0`, i.e. only the non-optional
  parts, which is the right *shape* but duplicates the logic that should just return the
  smallest variant record.
* `successor()` (`:115-129`) simply returns `variants[1]` regardless of the requested size,
  assuming "glyphs are in increasing advance" — the spec orders them that way, so this is a
  documented shortcut rather than a bug.
* `overlap as u16` truncates rather than rounds, and clamps nothing; `(1.0 - diff_ratio)` can go
  negative if `diff_ratio > 1`, which `as u16` turns into 0.
* `GREATEST_LOWER_BOUND`'s `ITERATION_LIMIT` mentioned in the comment is not implemented.

## Status
Feature-gated (`math`), unused by the PDF crates. Low priority; listed so a future math layout
does not inherit the panics.
