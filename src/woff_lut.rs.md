# `src/woff_lut.rs` — duplicate WOFF2 triplet table (not compiled)

## What it is
A second, independent copy of the WOFF2 128-entry triplet LUT (`Entry`/`TRIPLET_LUT`), with a
different representation (`Entry(u32)` packing byte count, x/y bit counts, biases and sign bits).

## Differences from the specifications
* **Not part of the crate:** `lib.rs` declares `mod woff;` (`:184`) but never `mod woff_lut;`.
  The file is never compiled, never tested, and cannot be reached — it is dead source that has
  drifted (or will drift) from the live table in `woff.rs:300-435`.
* Two hand-transcribed copies of a 128-row numeric table is exactly the kind of thing a typo
  survives in; if the transformed-glyf decode ever needs auditing, `woff.rs`'s copy is the one to
  check against the W3C WOFF2 spec's triplet table, and this file should be deleted rather than
  resurrected.

## Cross references
`woff.rs.md` (the live LUT and `parse_glyf_t0`).
