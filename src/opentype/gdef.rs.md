# `src/opentype/gdef.rs` — GDEF glyph classes (parsed, never reached)

## What it does
Parses `majorVersion`/`minorVersion` (1.0-1.3), then `glyphClassDefOffset`,
`attachListOffset`, `ligCaretListOffset`, `markAttachClassDefOffset`; parses the *mark
attachment* class definition into `GDef::mark_classes` and exposes
`GDef::mark_class(gid) -> Option<MarkClass>`.

## Fixed
* **F36 (2026-11-02):** the classes are used: `GDef::ignored(lookupFlag, gid)` answers OT
  §"Lookup Flags" — IgnoreBaseGlyphs (0x0002), IgnoreLigatures (0x0004), IgnoreMarks (0x0008) by
  glyph class, and a non-zero `markAttachmentType` (flag >> 8) hiding every mark that is not of
  that `markAttachClassDef` class. `GSUB` and `GPOS` both ask it through `glyph_ignored` before
  applying a lookup at a glyph. Test: `gpos::tests::ignore_marks_lookup_flag_skips_a_mark`.
* **F3/F4/F5/F27 (2026-11-02):** the table is now looked up as `GDEF` (`opentype/mod.rs`), both
  class definitions are parsed independently (glyphClassDef drives `mark_class()`,
  markAttachClassDef is exposed as `mark_attach_class()`), offset 0 is treated as "subtable
  absent" via `Offset::of_opt`, and GDEF 1.0 (`minor == 0`) is accepted. Test: `c01`-`c03` in
  `tests/spec_cases.rs`.

## Differences from the specifications
* **F3:** fixed 2026-11-02 — the tag lookup in `opentype/mod.rs` reads `b"GDEF"`, so
  `OpenTypeFont::gdef` is filled and the table is reachable (`GSUB`/`GPOS` lookup flags use it).
* **F4 (OT §GDEF, "Glyph Class Definition table"):** `mark_class()` maps the stored values
  1-4 to `Base`/`Ligature`/`Mark`/`Component`, but the values come from
  `markAttachClassDef`, whose classes are font-defined and carry no such meaning. The mapping
  belongs to `glyphClassDef` (`:17` reads it as `glyph_class_offset` and then ignores it).
* **F5:** `mark_attach_class_def_offset.of(data).unwrap()` (`:21`) panics both for a zero offset
  ("table absent", the common case) and for an out-of-range one. `Offset::of` treats 0 as an
  error by design, so the call site must check for 0 first.
* Version 1.2's `markGlyphSetsDef` and 1.3's `itemVarStore` are accepted (the version check is
  `1..=3`) and then ignored.
* `attachList` (attachment point lists) and `ligCaretList` (carets) are read as offsets and
  dropped, i.e. not validated either.

## Audit PR #7 (2026-11-03)
* **C8c:** `glyph_ignored` honours `UseMarkFilteringSet` (0x0010): for that lookup only the
  marks its MarkFilteringSet names count as marks.
* **C8:** a ligature's components may be separated by glyphs the lookup's flag ignores.
* **§5 (semantics):** `GDef::mark_class` reads `glyphClassDef` (base / ligature / mark /
  component); the `markAttachClassDef` of the same table is `mark_attach_class`. Callers that
  used `mark_class` for the attachment classes must switch to `mark_attach_class`.

## Cross references
`opentype/mod.rs.md` (table directory, tag lookup), `opentype/gpos.rs.md` (mark attachment uses
the classes GDEF should provide).
