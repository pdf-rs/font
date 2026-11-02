# `src/opentype/gdef.rs` — GDEF glyph classes (parsed, never reached)

## What it does
Parses `majorVersion`/`minorVersion` (1.0-1.3), then `glyphClassDefOffset`,
`attachListOffset`, `ligCaretListOffset`, `markAttachClassDefOffset`; parses the *mark
attachment* class definition into `GDef::mark_classes` and exposes
`GDef::mark_class(gid) -> Option<MarkClass>`.

## Fixed
* **F3/F4/F5/F27 (2026-11-02):** the table is now looked up as `GDEF` (`opentype/mod.rs`), both
  class definitions are parsed independently (glyphClassDef drives `mark_class()`,
  markAttachClassDef is exposed as `mark_attach_class()`), offset 0 is treated as "subtable
  absent" via `Offset::of_opt`, and GDEF 1.0 (`minor == 0`) is accepted. Test: `c01`-`c03` in
  `tests/spec_cases.rs`.

## Differences from the specifications
* **F3:** the table is looked up as `b"gdef"` in `opentype/mod.rs:139`; the real tag is `GDEF`,
  so `OpenTypeFont::gdef` is always `None`. Everything here is dead code today.
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

## Cross references
`opentype/mod.rs.md` (table directory, tag lookup), `opentype/gpos.rs.md` (mark attachment uses
the classes GDEF should provide).
