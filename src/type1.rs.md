# `src/type1.rs` — Type 1 fonts (PFB/PFA) and the Type 1 charstring interpreter

## Fixed
* **F10 (2026-11-02):** `seac` (`12 6`) is no longer a no-op. It records the base and accent
  standard-encoding codes plus `(adx, ady)` in `State::seac`; `Type1Font::from_vm` composes them
  in a second pass (the components may be defined after the composite): base outline plus the
  accent outline translated by `(adx, ady)`. Components are found by mapping the StandardEncoding
  code to its character (`pdf_encoding`) and that to the glyph whose name maps to it; a missing
  component or a self-reference warns and leaves the glyph empty. Test: `tests/type1_cases.rs`
  `c23`.
* **F24 (2026-11-02):** `/Encoding` is no longer required to be an array. A font without
  `/Encoding`, or with `/Encoding /StandardEncoding` (a name this VM does not resolve back to an
  array), now warns and leaves the codepoint map empty — the glyphs stay reachable by name.
  `FontMatrix` items are read with `filter_map(as_f32)` + `expect!` instead of two `unwrap()`s.
* **F25 (2026-11-02):** an unknown `12 x` escape returns `error!("invalid Type 1 charstring
  operator (12 {})")` (that glyph is skipped, the font still loads, `:436`), a `12 17 pop` on an
  empty PS stack returns an error instead of `.expect("PS stack is empty")`, and a PFB block type
  other than 1/2/3 is an error instead of `panic!("unknown block type")` (`:250`).
  Tests: `tests/type1_cases.rs` `c20`/`c20b`/`c20c`/`c21`/`c22`.
* **F40 (2026-11-03):** Type 1 flex works. `rrcurveto`, `hvcurveto` and `vhcurveto` now feed
  their control points and endpoint into `State::flex_sequence` while a flex is open (new
  `push_curve`), so the seven points (`reference, c1, c2, join, c3, c4, end`) are complete at
  `flexheight x y 3 0 callothersubr`; the flex end draws the reference point and the two cubics,
  pushes `(y, x)` for the trailing `pop pop setcurrentpoint`, and only warns when the sequence has
  the wrong length or is missing (previously the glyph was dropped). Flex start accepts otherSubr
  1 or 2 with no arguments, flex end otherSubr 0 or 1 with three. `callothersubr` also warns
  instead of underflowing when `n` exceeds the stack. Test: `tests/type1_cases.rs` `c24`.

## What it does
`Type1Font::from_vm` pulls `FontName`, `FontInfo/FullName`, `Weight`, `FontMatrix`, `FontBBox`,
`/Private` (`lenIV`, `Subrs`) and `/CharStrings` out of a `postscript::Vm`, decodes each
charstring (eexec charstring key, skipping `lenIV` bytes), runs it through `charstring()` and
stores glyphs in an `IndexMap<String, Glyph>` (insertion order = gid). `/Encoding` is turned into
`codepoint -> gid`, and glyph names into a `unicode_map`.

The interpreter covers the Type 1 path operators (`hsbw`, `hstem`/`vstem`, `vmoveto`/`rmoveto`/
`hmoveto`, `rlineto`, `hlineto`/`vlineto`, `rrcurveto`, `closepath`, `endchar`, `vhcurveto`,
`hvcurveto`, `callsubr`/`return`, and the `12 x` escapes `dotsection`, `vstem3`, `hstem3`,
`seac`, `sbw`, `div`, `callothersubr`, `pop`, `setcurrentpoint`).

## Differences from the specifications
* ~~**F10 (T1 §6, "Standard Encoding Accented Character"):**~~ `seac` used to clear the stack and
  do nothing, so every accented composite glyph rendered empty — composed in `from_vm` now (see
  "Fixed" above).
* ~~**F40:**~~ the flex mechanism used to never work: the 7-point sequence was only appended to
  by `rmoveto`, while the curve operators never pushed into `s.flex_sequence`, so
  `TupleElements::from_iter` failed and the glyph was dropped (drawn by `push_curve` now, see
  "Fixed" above).
* **F24:** (fixed above) `/Encoding` had to be an array.
* **F41:** `lenIV` is cast with `as usize`; `lenIV = -1` ("charstrings are not encrypted", legal)
  becomes a gigantic skip. Values > the charstring length silently produce an empty glyph.
* Glyph ids follow the `/CharStrings` *dict* iteration order, and `postscript::Dictionary` is a
  `std::collections::HashMap`, so gids differ between runs of the same font. Only name- and
  codepoint-based lookups are stable.
* `char_width` is mandatory (`expect!(state.char_width, "CharWidth not set")`, `:149`): a
  charstring without a valid `hsbw`/`sbw` aborts the *whole font* instead of that one glyph,
  while a charstring error only `continue`s — inconsistent, and the `continue` path changes gid
  numbering relative to `/CharStrings` order (the `codepoints` map is rebuilt from names, so the
  result stays self-consistent, but it is surprising).
* **F25:** (fixed above) the `12 x` panic and the PFB block-type panic.
* `hsbw` sets `current = (sbx, 0)` and `lsb = sbx` ✓ (T1 §8: "hsbw sets the current point to
  (sbx, 0)"). `sbw` (`:379-386`) sets `char_width = wx`, `current = (sbx, sby)` and *ignores*
  `wy`/`sbw` — the spec's `sbw` is the "sidebearing plus width" form used inside
  `callothersubr` sequences where only the x-advance matters, so this is acceptable.
* Type 1 `hlineto`/`vlineto`/`rlineto` take exactly one argument (`stack.len() >= 1/2` and then
  read `stack[0..]`) — correct for Type 1 (the multi-argument forms are Type 2 only).
* The `255` escape pushes the raw `i32` (T1: 32-bit integer) — for the 16.16 fixed convention used
  by some Type 1 fonts this should be divided by 65536; the code treats it as an integer, which is
  what the Type 1 spec says ("255 followed by a 4-byte signed integer"), so the interpretation is
  right and it is the *fonts* that are non-conforming.
* `unicode_map` uses `glyphname_to_unicode` and a `uniXXXX` fallback (`:154-168`); `uXXXX`-style
  names and `f`-ligature names beyond the table are not covered.

## Cross references
`postscript.rs.md` (the interpreter that produces the `Vm`), `eexec.rs.md` (the `lenIV`-based
decoder), `lib.rs.md` (`State`), `cff.rs.md` (Type 1 charstrings inside CFF reuse this file).
