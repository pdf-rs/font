# Font --- Pure Rust font parsers

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
- `SVG ` outlines are not implemented yet, so emoticon fonts do not work.
- Most of the `CMAP` formats are implemented.
- Kerning using the `kern` table is implemented (working on `GPOS`).

### WOFF / WOFF2
 - Implemented but not tested.

## API
NOTE: The code may change a bit. Especially multi-codepoint glyphs cannot be looked up yet.
Fonts are loaded with `parse`, which returns a trait object.
	
    pub trait Font<O: Outline> {
	    fn num_glyphs(&self) -> u32;
	    fn font_matrix(&self) -> Transform;
	    fn glyph(&self, gid: u32) -> Option<Glyph<O>>;
	    fn glyphs(&self) -> Glyphs<O>;
	    fn gid_for_codepoint(&self, _codepoint: u32) -> Option<u32>;
	    fn gid_for_name(&self, _name: &str) -> Option<u32>;
	    fn gid_for_unicode_codepoint(&self, codepoint: u32) -> Option<u32>;
	    fn encoding(&self) -> Option<Encoding>;
	    fn get_notdef_gid(&self) -> u32;
	    fn bbox(&self) -> Option<Rect>;
	    fn vmetrics(&self) -> Option<VMetrics>;
	    fn kerning(&self, left: u32, right: u32) -> f32;
    }

## Demo
[You can try it out here](https://s3bk.github.io/font_wasm/)
Drop font files on the page to add them.
