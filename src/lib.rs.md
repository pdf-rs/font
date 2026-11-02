# `src/lib.rs` — crate root: the `Font` trait, type dispatch, shared charstring state

## Fixed
* **F38 (2026-11-02):** `Info` gained `x_height`, `cap_height` (font units, `None` when the font
  has no `OS/2` version 2+ table) and `unicode_ranges` (`OS/2` `ulUnicodeRange1..4`); it derives
  `Default` so constructors only set what they have.
* **F10/F11 (2026-11-02):** `State` gained `seac: Option<Seac>` (cleared by `State::clear`), and the
  crate root now defines `pub struct Seac { adx, ady, base, accent }` — the accent composition
  shared by Type 1 `seac` and Type 2 `endchar` with four arguments.
* **F29 (2026-11-02):** the private `print_err(e: nom::Err<VerboseError<&[u8]>>) -> !` is gone. It
  was never called (no caller in the crate), and it panicked with "need more data" / "error
  converting an `Err::Failure`" / "error converting an `Err::Incomplete`" — so it was dead code
  that could only ever abort. Errors now travel as `FontError` everywhere. `src/error.rs.md`
  updated to match.

## What it does
* Defines `Font`, the single trait every backend implements (`num_glyphs`, `font_matrix`,
  `glyph`, `is_empty_glyph`, `gid_for_codepoint`/`gid_for_name`/`gid_for_unicode_codepoint`,
  `encoding`, `bbox`, `vmetrics`, `kerning`, `name`, `info`), plus an unsafe
  `_type_id`-based `downcast_ref`/`downcast_box` (a hand-rolled `Any`).
* `font_type(data)` / `parse(data)`: magic sniffing (`OTTO`/`0x00010000` → `OpenTypeFont`,
  `true` → `TrueTypeFont`, `wOff`/`wOF2` → WOFF wrappers, `0x80 0x01` → Type 1 PFB, `%!PS` /
  `0x25 0x21` → Type 1 PFA, `0x01 xx xx xx` → bare CFF).
* `State` — the shared interpreter state for Type 1 and Type 2 charstrings (`stack`, `outline`,
  `contour`, `current`, `lsb`, `char_width`, `delta_width`, `stem_hints`, `flex_sequence`,
  `seac`).
* `Context` — subr index + bias table for both charstring dialects.

## Differences from the specifications
* **F34 (OT §"Font Collections"):** `ttcf` and `typ1` are recognised by `font_type()` and then
  rejected (`"FontCollections not implemented"` in `parse`, `:499`). A TrueType Collection is a
  legal container and is exactly what PDF `/FontFile2` may hold.
* ~~**F29:**~~ `print_err` (`:441-449`) panicked instead of reporting — removed (see "Fixed" above).
* `parse()` requires 4 bytes (`slice!(data, 0..4).try_into().unwrap()` at `:493`) — a 2-byte
  input makes it return `Err` via the macro, but `font_info()` (`:557`) indexes `data[0..4]`
  directly and panics.
* `gid_for_unicode_codepoint` in the default impl (`:105-112`) goes through `encoding()`'s
  reverse map, i.e. it assumes the font's native encoding is a legacy 8-bit one. For
  Unicode-cmap fonts the backends override it; for CFF/Type 1 it is the only path, and it is
  limited to glyph names that `pdf_encoding::glyphname_to_unicode` knows.
* `Value` distinguishes `Int`/`Float` and `to_int()` *rejects* floats — Type 2 allows mixed
  arithmetic, so `(1.5) 2 mul` style charstrings depend on `to_float()` being used; the
  interpreter is careful, but a float where an index is expected (e.g. `callsubr`) errors out
  where a truncating conversion would be more forgiving.
* `State::pop_tuple`/`args` require the *bottom* `N` elements to exist, else `expect!` → error
  (correct), but `args()` accepts a stack that is longer than `N` and silently ignores the
  extra entries.

## Cross references
`truetype.rs.md`, `type1.rs.md`, `type2.rs.md`, `opentype/mod.rs.md`.
