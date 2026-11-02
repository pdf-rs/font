# `src/opentype/post.rs` — `post` table and the 258 standard Macintosh glyph names

## What it does
Reads the `post` header, and for version 2.0 decodes the glyph-name array: indices `< 258` are
the standard Macintosh names (`MAC_NAMES`, which is transcribed correctly, 258 entries), indices
`>= 258` index into the Pascal-string array that follows. `OpenTypeFont` turns the resulting
`Vec<&str>` into `name_map: HashMap<String, u16>` so `Font::gid_for_name` works — this is what PDF
`/Differences` and Type 0 `/CIDToGIDMap`-less lookups lean on.

## Differences from the specifications
* **F15 (fixed 2026-11-02):** version 1.0 now yields the 258 standard Macintosh names in glyph
  order, as the spec defines; before, only 2.0 was decoded, so older TrueType fonts got an empty
  `name_map` and no `/Differences` lookup at all. Version 3.0 still correctly yields no names.
  Tests: `c12`, `c12b`.
* Version 4.0 (Apple advanced typography) is not decoded either (it has a `uint16` glyph-name
  array pointing into the standard names + strings; rare).
* `numGlyphs` in the version-2.0 sub-header is not cross-checked against `maxp.numGlyphs`; if it
  is larger, `glyph_name_index` reads past the table (`array::<Offset>` errors, so it fails the
  font rather than truncating).
* The count of new names is derived by counting indices `>= 258` — correct per spec
  ("the number of Pascal strings is the number of glyphs with index >= 258") — and `idx - 258`
  is bounds-checked against that count (`:36-42`), which is right.
* `name_strings` are required to be UTF-8 (`std::str::from_utf8`); the spec defines them as
  ASCII/UTF-8, so a font with a Latin-1 name string fails the whole `post` parse (and therefore
  the font) instead of dropping that one name.
* `underlinePosition`/`underlineThickness`/`italicAngle`/`isFixedPitch` are read and discarded;
  nothing in the crate exposes them.

## Cross references
`opentype/mod.rs.md` (`name_map` construction, `gid_for_name`), `opentype/name.rs` = `name` part
of `mod.rs` (`Font::name()`).
