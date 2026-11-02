# `src/opentype/os2.rs` — OS/2 table: the layout metrics Windows asks for

## Fixed
* **F38 (2026-11-02):** the table is no longer cut off after `usWeightClass`. The fixed prefix
  is parsed (`version`, `xAvgCharWidth`, `usWeightClass`, `usWidthClass`, `fsType`, the
  `ySubscript*`/`ySuperscript*`/`yStrikeout*` fields, `sFamilyClass`, `panose`,
  `ulUnicodeRange1..4`, `achVendID`, `fsSelection`, `usFirstCharIndex`/`usLastCharIndex`,
  `sTypoAscender`/`sTypoDescender`/`sTypoLineGap`, `usWinAscent`/`usWinDescent`), plus the
  versioned tails: `ulCodePageRange1/2` (version ≥ 1) and `sxHeight`/`sCapHeight` (version ≥ 2).
  A tail that the table is too short to hold only costs those optional fields, never the font.
  `Os2::use_typo_metrics()` (version ≥ 4 and `fsSelection` bit 7) and `Os2::typo_metrics()` feed
  `Font::vmetrics()` in `opentype/mod.rs` and `truetype.rs`; `sxHeight`, `sCapHeight` and
  `ulUnicodeRange1..4` reach `Font::info()`. Tests: `c25`, `c25b` in `tests/spec_cases.rs`.

## What it does
`Os2` carries the prefix fields listed above, `x_height`/`cap_height` as `Option<i16>` (version ≥ 2
and long enough), and the derived line metrics. `usWinAscent`/`usWinDescent`, `fsType` and the PANOSE
bytes are parsed but not used anywhere yet.

## Differences from the specifications
* Only versions 0-5 are considered; the version number itself is not validated, and a field that
  a version defines is read only after a length check (a truncated table is accepted).
* `xAvgCharWidth` is read as `int16`, which is the version 0-2 type; from version 3 it is
  `uint16`. It is discarded, so only a value ≥ 32768 would be affected.
* `usWinAscent`/`usWinDescent` (the clipping box), `fsType`, PANOSE, `sFamilyClass` and the
  `ulCodePageRange*` bits are parsed but not exposed — nothing in this crate needs them yet.

## Cross references
`opentype/mod.rs.md` (`vmetrics` preference), `truetype.rs.md` (same rule for the `true` format),
`opentype/post.rs.md`.
