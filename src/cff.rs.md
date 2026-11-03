# `src/cff.rs` — Compact Font Format: INDEX/DICT parsing, charsets, encodings, outlines

## Fixed
* **F11 (2026-11-02):** `CffSlot::outlines()` now also returns the `seac` composition the
  charstring recorded (`Option<Seac>`), and `composed_outlines()` assembles it: the base glyph
  plus the accent glyph translated by `(adx, ady)`. The components are found by mapping the
  StandardEncoding code to its SID and that SID to a gid through the charset (`sids()` was
  extracted from `parse_font`); CID-keyed fonts never match, so they compose nothing. Also used
  by the CFF-inside-OpenType path (`opentype/mod.rs`). Test: `c24` in `tests/cff_cases.rs`.
* **F22/F23 (2026-11-02):** nothing on the parse path panics any more. A missing `Private`
  TopDICT entry is an empty private dict (TN5176 makes it optional), `index()` rejects an
  INDEX offset of 0 with `error!` instead of underflowing, an unknown `CharstringType` and an
  unknown charset/encoding format are `Err` (charset/encoding) or a warning (`CharstringType`,
  which only affects outlines), and a charstring that sets both an explicit and a delta width
  keeps the explicit one instead of hitting `panic!("BUG: both char_width and delta_width set")`.
  Tests: `c19`, `c19b`, `c19c`, `c19d` in `tests/cff_cases.rs`.
* **F17 (2026-11-03):** the glyph-name→Unicode map is built with
  `sids.iter().enumerate().take(glyphs.len().saturating_sub(1))` instead of `0..glyphs.len()-1`,
  so there is no `len()-1` underflow left on the path. The report's "the last glyph is never
  mapped" was wrong: gid 0 is `.notdef` and the charset names gids 1..n, so the old bound already
  covered every charset entry (verified — `c17` passes on the old code too), and an empty
  CharStrings INDEX is already rejected by `Cff::slot` (`num_glyphs >= 1`). Hardening only.
  Tests: `c17`, `c17b` in `tests/cff_cases.rs`.
* **F18 (2026-11-03):** an encoding supplement is applied through `sid_map.get()` instead of
  `sid_map[&sid]`, so a supplement that names an SID the charset does not contain (legal — a
  supplement may name a glyph the font lacks, TN5176 §"Encoding") leaves that code unmapped
  instead of panicking. Test: `c18` in `tests/cff_cases.rs`.

## What it does
* `read_cff`: header (major=1, hdrSize, offSize), the four leading INDEXes (Name, TopDICT,
  String, GlobalSubr); the Name INDEX (font name) is kept for `CffFont::name()`.
* `Cff::slot(idx)`: one TopDICT → `CharStrings` INDEX, `Private`/`Subrs`, `FDArray`/`FDSelect`
  for CID fonts, `FontMatrix` (default `0.001`), per-glyph Private DICT + local Subrs.
* `CffSlot::outlines()`: runs each charstring (Type 1 or Type 2 dialect per
  `CharstringType`) through `type1::charstring`/`type2::charstring` and derives
  `(outline, width, lsb, seac)` using `defaultWidthX`/`nominalWidthX` and the `deltaWidth` from the
  first stack-clearing operator.
* `CffSlot::parse_font()`: charset (predefined ISOAdobe/Expert/ExpertSubset, formats 0/1/2),
  SID→gid and name→gid maps, the 8-bit built-in encoding (Standard/Expert/custom + supplements),
  and a glyph-name→Unicode map via `pdf_encoding::glyphname_to_unicode`.
* `dict`/`value`/`float`/`operator`: the DICT operand syntax including the 5-nibble real format.

The SID tables (`STANDARD_STRINGS`, `ISO_ADOBE_CHARSET`, `EXPERT_CHARSET`,
`EXPERT_SUBSET_CHARSET`, `STANDARD_ENCODING`, `EXPERT_ENCODING`) match TN5176 Appendix A/B.

## Differences from the specifications
* **F35:** `require_eq!(major, 1)` (`:105`) — CFF2 (variable fonts) is rejected.
* **F47 (TN5176 §"Name INDEX", §"Top DICT") — fixed 2026-11-03:** `CffFont::name()` was
  `Name::default()` and `info.weight` always `None`, although the data was parsed. `read_cff` now
  keeps the Name INDEX (index 0 = the PostScript name) and `parse_font` resolves the Top DICT's
  `FamilyName`/`FullName`/`Weight` SIDs through the String INDEX. `Weight` is a *string*
  ("SemiBold") and `weight_class` maps the TN5176 A.3 words to the numeric OS/2 class (an
  unrecognised word stays `None`, nothing is guessed). Without a `FamilyName` the head of the font
  name (before `-`) is the family. Tests `c47`, `c47b`.
* **F22/F23:** (fixed above) `Private` was treated as mandatory, the charstring/charset paths
  used `panic!` for anything unexpected, and `index()` underflowed on an offset of 0.
* **F42 (TN5177/TN5176 §"Charstring width"):** width derivation is right — `(Some(w), None)` uses
  the charstring width, `(None, None)` `defaultWidthX`, `(None, Some(d))` `nominalWidthX + d`.
  `(Some(_), Some(_))` was a panic; it now keeps the explicit width (fixed above).
* `dict()` (`:481-497`) uses `while let Ok(...) = value(input)` to collect operands; an invalid
  operand byte (22-27, 31, 255 = reserved) silently ends the operand list and the next byte is
  then parsed as an operator, so a malformed DICT desynchronises instead of failing.
* `float()` (`:529-586`) indexes `data[pos/2]` directly — a truncated real operand panics
  (index out of bounds) rather than returning `Err`.
* `charset` format 0 uses `num_glyphs - 1` (`:779`), underflowing when the CharStrings INDEX is
  empty; `ranges()` (`:767`) has the same `num_glyphs - 1` pattern.
* `fd_select` format 3 (`:255-275`) validates that the first range starts at gid 0 ✓ but does not
  validate that the sentinel equals `numGlyphs`; format 4 (CFF2) is not handled.
* `parse_font_matrix` (`:156-162`) reads the six values as `row_major(a, b, e, c, d, f)`, i.e.
  the spec's `[a b c d e f]` → `[[a b][c d]]` plus translation `(e, f)`. Correct.
* `CharstringType` 1 (Type 1 charstrings inside CFF) is supported via `type1::charstring`, but the
  Type 1 *width* (`hsbw`, opcode 13) is the only source there, so `delta_width` handling differs.
* No subr-bias validation and no recursion depth limit — see `type2.rs.md`.

## Cross references
`type1.rs.md`, `type2.rs.md`, `lib.rs.md` (`State`/`Context`), `opentype/mod.rs.md`
(`OpenTypeFont::from_hmtx_glyf_and_tables` calls `read_cff` for `CFF `).
