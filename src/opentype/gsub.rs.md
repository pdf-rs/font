# `src/opentype/gsub.rs` — GSUB: scripts, features, lookups, ligatures

## What it does
Parses the full GSUB skeleton — ScriptList → Script → LanguageSystem (default + per-language),
FeatureList → Feature (tag + lookup indices), LookupList → single (1), multiple (2),
alternate (3) and ligature (4) substitutions — and exposes them as `GSub::scripts()`,
`default_language()`, `language(tag)`, `subs(lang, enabled)`, `lang_features(lang)`. Nothing in
the crate applies them; `pdf` uses ligature lookup manually where it can.

## Fixed
* **F36 (2026-11-02):** Multiple (2) and Alternate (3) subtables are parsed and applied:
  `parse_multiple_subst` reads one-glyph-to-sequence records (an empty sequence deletes the
  glyph, as OT §GSUB allows) and `parse_alternate_subst` reads alternate sets. They live in the
  new `LookupSubtable` enum (`Substitution` | `Multiple` | `Alternate`), which is what
  `GSub::lookup` now stores — `Substitution` itself keeps its two variants because consumers
  (`svg_text::apply_subs`) match it exhaustively. `LookupSubtable::apply` returns the glyphs
  that replace the consumed ones, so `GSub::substitute` can splice a sequence; the glyphs a
  lookup produces are not fed back into the same lookup. `apply_with(glyphs, n)` chooses the
  nth alternate (OT leaves that to the application; the default is the first, an out-of-range
  index falls back to it). `subs()` still yields `&Substitution` only. Tests:
  `multiple_substitution_expands_and_deletes`, `alternate_substitution_picks_the_requested_alternate`.
* **F36 (2026-11-02):** malformed tables no longer abort the process:
  `parse_ligatures` used `count(be_u16, component_count - 1)`, which underflowed (a panic in
  debug, a huge count in release) for `componentCount == 0`; such a ligature now warns and is
  skipped. `subs()` and `lang_features()` no longer index `self.features[idx]` /
  `self.lookup[idx]` with indices read from the font — a bad `featureIndex`/`lookupIndex` is
  skipped instead of panicking. `Substitution::apply` refuses glyph ids above `0xFFFF` rather
  than truncating them with `as u16` and substituting an unrelated glyph. Tests:
  `ligature_with_component_count_zero_is_skipped`,
  `out_of_range_feature_and_lookup_indices_are_ignored`,
  `apply_ignores_glyph_ids_outside_u16`.
* **F36 (2026-11-02):** Extension lookups are followed: `parse_substitution` unwraps lookup type 7
  (an extension pointing at another extension warns and is dropped), so fonts that keep their
  ligatures behind extensions now expose them. `GSub::substitute()` applies the lookups of the
  default language system whose feature tag the caller enables, in lookup-list order, once each
  (the subset of OT §"Lookups" this crate implements): `Substitution::apply` handles Single and
  Ligatures, the longest ligature winning. `Font::substitute(glyphs, features)` is the new trait
  entry point; lookups of type 2/3/5/6/8 are still not implemented and pass glyphs through.
  Unit tests: `ligature_substitution_picks_the_longest_match`,
  `single_substitution_replaces_one_glyph`, `extension_lookup_is_unwrapped`.
* **F21 (2026-11-02):** an unknown `minorVersion` no longer panics (`panic!("unsupported GPOS
  version 1.{}")` here was a copy-paste of the GPOS message); it warns and the table is read.
  Tests: `c18`, `c18b`.

## Differences from the specifications
* ~~**F36 (OT §GSUB):**~~ types 1 and 4 were the only ones collected and Extension (7) was
  dropped — extensions are followed and 1/2/3/4 are applied now (see "Fixed" above). Context
  (5), Chaining Context (6) and Reverse Chaining (8) are still logged with `debug!` and dropped.
* `GSub::default_language()` (`:36-38`) returns `scripts.get(0)`, i.e. whichever script record
  happens to come first in the table. The spec has no "first script"; consumers want `DFLT`
  (then `latn`). `pdf` side-steps this by looking up `latn`/`DFLT` itself.
* ~~`subs()` indexes `self.features[idx]` and `self.lookup[lookup_idx]` with indices read
  straight from the font — a bad `featureIndex`/`lookupIndex` is a panic, not an error.~~
  Fixed (see "Fixed", F36): out-of-range indices are skipped.
* `parse_single_subst` (OT §GSUB 1.1/1.2): format 1 uses `gid.wrapping_add(deltaGlyphId)` ✓
  ("modulo 65536"), format 2 zips coverage with the substitute array ✓, but a short substitute
  array silently ends the zip instead of erroring.
* ~~`parse_ligatures` (OT §GSUB 4): `count(be_u16, component_count - 1)` underflows and
  panics when `componentCount == 0`;~~ fixed (see "Fixed", F36). The ligature-set and ligature
  offsets use the correct bases (resolved from the LigatureSet, not the subtable).
  `Substitution::Ligatures` maps first-glyph → list of (component list, ligature gid) and
  `GlyphList::matches` compares glyph ids ✓.
* `parse_feature_table` (`:270-276`) reads `featureParamsOffset` and drops it; for `size`
  features and `ss01`-style parameters that is the whole point of the feature, so this is
  "structure only".
* **F21:** (fixed above) the GSUB copy of the GPOS version panic.
* The version-1.1 `featureVariationsOffset` is read and ignored.

## Cross references
`opentype/mod.rs.md` (`parse_lookup_list`, `coverage_table`), `opentype/gpos.rs.md` (same
skeleton, same Extension gap), `lib.rs.md` (`Font::gid_for_name` is the only substitution-ish
API the PDF layer uses).
