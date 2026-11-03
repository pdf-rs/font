# `src/svg.rs` — SVG table (colour/outline glyphs as SVG documents)

## Fixed
* **F39 (2026-11-02):** a gzip-compressed document (magic `1F 8B`) is decompressed before it
  reaches `svg_dom`: the header is skipped (FEXTRA/FNAME/FCOMMENT/FHCRC included), the deflate
  body inflated with `inflate::inflate_bytes`, the CRC32/ISIZE trailer dropped. Anything else is
  passed through unchanged (`document_data`). Tests: `c26`, `c26b` in `tests/spec_cases.rs`.

## What it does
Parses the `SVG ` table header (`version`, `svgDocumentListOffset`, reserved), then the document
list: `(startGlyphID, endGlyphID, svgDocOffset, svgDocLength)` records. Each document is handed
to `svg_dom::Svg::from_data`, and for every gid in the range the element `glyph<gid>` is looked up
and stored, so `Font::svg_glyph(gid)` can return `(document, element)`.

## Differences from the specifications
* ~~**F39 (OT §SVG, "SVG Document List"):**~~ documents may be gzip-compressed — handled (see
  "Fixed" above). The decompressed size is not capped, so a small gzip stream that expands to
  gigabytes still allocates that much before `svg_dom` can reject it.
* `version` is read and not checked (must be 0).
* The `0x0000FFFF` "special" glyph ID reservation and the spec's rule that a document's range may
  be empty (`startGlyphID > endGlyphID`) are not handled: the loop `for gid in start_gid ..=
  end_gid` is empty in that case, which is fine, but a range covering huge IDs would iterate.
* Document offsets are relative to the *document list* start, which is what the code uses
  (`slice!(input, …)`) ✓.
* The element id convention `glyph<gid>` matches the spec ✓ (decimal, no padding).
* A missing element only `warn!`s and skips that glyph ✓ — the right behaviour.
* Feature-gated behind `svg` (and `svg_dom` from `github.com/s3bk/svg`), off by default; the PDF
  crates never enable it, so a PDF with an OpenType SVG font today renders nothing for those
  glyphs because the table is not parsed at all.

## Audit PR #7 (2026-11-03)
* **§3 (gzip):** a document is inflated through `inflate_limited::deflate` with the size the
  gzip `ISIZE` field announces, capped at `MAX_SVG_BYTES = 64 MiB`.
* **§4 (policy):** a document that cannot be decompressed is skipped with a warning, the way a
  document the SVG parser rejects already was; it no longer costs the whole table.

## Cross references
`opentype/mod.rs.md` (`parse_svg` is called from `from_hmtx_glyf_and_tables`),
`lib.rs.md` (`Font::svg_glyph`, a defaulted trait method).
