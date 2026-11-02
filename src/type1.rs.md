# `src/type1.rs` — Type 1 fonts (PFB/PFA) and the Type 1 charstring interpreter

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
* **F10 (T1 §6, "Standard Encoding Accented Character"):** `seac` (`:376-379`) clears the stack
  and does nothing. Every accented composite glyph renders empty.
* **F40:** the flex mechanism (`callothersubr` 1 → `callothersubr` 0, `:396-420`) can never
  work: the 7-point sequence is only appended to by `rmoveto` (`:481-495`), while the curve
  operators never push into `s.flex_sequence`, so `TupleElements::from_iter` on the 7-tuple fails
  and the glyph is dropped (`:139-142` warns and `continue`s).
* **F24 (T1 §8 "Encoding"):** `/Encoding` is required and must be an array
  (`:129 .expect(...).as_array().expect(...)`). Standard-encoding fonts (no `/Encoding`) and
  `/Encoding /StandardEncoding` (a name) both panic. The array form is handled, including
  `.notdef` and `RefItem::Null` placeholders, but only for literals — `/Encoding [ /A /B ]`,
  which is the common form, works; `/Encoding 32 /space put`-style dicts do not.
* **F41:** `lenIV` is cast with `as usize`; `lenIV = -1` ("charstrings are not encrypted", legal)
  becomes a gigantic skip. Values > the charstring length silently produce an empty glyph.
* `char_width` is mandatory (`expect!(state.char_width, "CharWidth not set")`, `:149`): a
  charstring without a valid `hsbw`/`sbw` aborts the *whole font* instead of that one glyph,
  while a charstring error only `continue`s — inconsistent, and the `continue` path changes gid
  numbering relative to `/CharStrings` order (the `codepoints` map is rebuilt from names, so the
  result stays self-consistent, but it is surprising).
* **F25:** unknown two-byte escapes hit `_ => panic!("invalid operator")` (`:436`); reserved
  opcodes must be skipped. PFB block types other than 1/2/3 `panic!` (`:250`).
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
