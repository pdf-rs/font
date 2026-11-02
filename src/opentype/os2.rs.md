# `src/opentype/os2.rs` — OS/2 table (usWeightClass only)

## What it does
Reads `version`, `xAvgCharWidth` and `usWeightClass`, and exposes `Os2 { weight }`. That weight
is the only OS/2 datum that reaches `Font::info().weight`.

## Differences from the specifications
* **F38 (OT §OS/2):** everything else is dropped. The ones that change output elsewhere in this
  crate:
  * `sTypoAscender` / `sTypoDescender` / `sTypoLineGap` plus `fsSelection` bit 7
    (`USE_TYPO_METRICS`) — the *recommended* source of line spacing for Windows fonts.
    `Font::vmetrics()` returns `hhea`'s values instead (`opentype/mod.rs:129`), which for many
    Microsoft-authored fonts gives the wrong line height.
  * `sCapHeight`, `sxHeight` — needed by any vertical alignment/MATH work.
  * `usWinAscent`/`usWinDescent` — the clipping box.
  * `fsSelection` italic/bold bits and `ulUnicodeRange*` for font substitution.
* Only `version` 0's layout is read, and only its first fields, so versions 1-5 parse fine (the
  extra fields are simply never touched) — but `version` itself is not validated at all.
* `xAvgCharWidth` is read as `i16`; per the spec it is `int16` for versions 0-2 and `uint16`
  from version 3. Irrelevant here since it is discarded.

## Cross references
`opentype/mod.rs.md` (`vmetrics` from `hhea`), `opentype/post.rs.md`.
