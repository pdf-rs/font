# `src/inflate_limited.rs` — DEFLATE and zlib with an output limit

## What it does
`zlib(data, limit)` and `deflate(data, limit)` decompress a zlib stream and a raw DEFLATE
stream, and stop with an error as soon as the output would pass `limit` bytes. Both drive
`inflate::InflateStream::update` in a loop: each call decodes into the stream's own ~32 KiB
window and reports `(bytes consumed, decoded chunk)`, so the caller keeps a running total and
never holds more than the limit. The module is compiled for feature `woff` or `svg`, the two
places a font hands it a compressed stream.

## Why
`inflate::inflate_bytes` / `inflate_bytes_zlib` have no output cap, so a few KiB of gzip in an
`SVG ` table or in a WOFF table could expand until the process runs out of memory (audit PR #7
§3). Callers pass the size the container announces — WOFF `origLength`, the gzip `ISIZE` field —
and a ceiling of their own (`woff::MAX_TABLE_BYTES`, `woff::MAX_SFNT_BYTES`,
`svg::MAX_SVG_BYTES`).

## Tests
`a_deflate_stream_stops_at_its_limit` and `a_zlib_stream_stops_at_its_limit_too` build a stored
DEFLATE block by hand (`stored_block(len, byte)`), decompress it with a limit below and at its
size, and check the error and the exact output.

## Cross references
`woff.rs.md`, `svg.rs.md`, `lib.rs.md`.
