# Font -- Pure Rust font parsers

## Supported Formats

### TrueType
- `kern` Table supported
- Full outline support
- Most `CMAP`s implemented
- all glyphs listed in any cmap can be accessed with `gid_for_unicode_codepoint` (or `gid_for_codepoint` which calls the former)

### CFF (Compact Font Format)
 - Charstring format 1 and 2 are fully implemented
 - All glyphs that are listed by `/Encoding` can be accessed via `gid_for_codepoint`. Glyphs can be looked up from unicode values if they are defined in Adobes `StandardEncoding`.
 - All glyphs can be accessed by name using `gid_for_name`
 - (not yet implemented: Translating codepoints into glyph names and using them for lookup)

### Type1
 - Contains a PostScript interpreter (without file access)
 - Calling PostScript from CharStrings (used for Hinting) is not implemented. Instead they are emulated and the correct outline is produced.
 - Glyphs can accessed with:
   - `gid_for_name` using the name of the charstring
   - `gid_for_codepoint` using the built in `/Encoding`
   - `gid_for_unicode_codepoint` using the [AFL-Glyphlist](https://github.com/adobe-type-tools/agl-aglfn)

### OpenType
- The `glyf` (TrueType) and `CFF ` (Type1) outlines are supported.
- `SVG ` outlines are supported.
- Most of the `CMAP` formats are implemented.
- Kerning using the `kern` and `GPOS` table is implemented.

### WOFF / WOFF2
- essentially OpenType fonts. implemented and working.

## API
NOTE: The code may change a bit. Especially multi-codepoint glyphs cannot be looked up yet.
Fonts are loaded with `parse`, which returns a trait object.

## Demo
[You can try it out here](https://s3bk.github.io/font_wasm/)
Drop font files on the page to add them.
