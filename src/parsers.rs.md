# `src/parsers.rs` — shared nom helpers, offsets, arrays, PostScript tokens

## Fixed
* **F28 (2026-11-02):** `array_iter` did `input.split_at(P::SIZE * count)` and `array` the same
  product unchecked, so a count taken from the file either panicked in `split_at` or wrapped
  around on 32-bit. Both now go through `P::SIZE.checked_mul(count)` and return
  `Failure(Complete)` when the product does not fit or exceeds the remaining input; unit test
  `array_iter_rejects_counts_past_the_end` in `mod array_iter_tests` (4 lib tests now).

## What it does
Two parse conventions coexist:
* `R<T> = IResult<&[u8], T, VerboseError<&[u8]>>` — nom-style, used for untrusted table data.
* `ParseResult<T> = Result<(&[u8], T), FontError>` — the crate's own, used by the generic
  `Parser`/`NomParser`/`FixedSize` machinery (`table!`/`parser!` in `macros.rs`).

Contents: `iterator`/`iterator_n`/`count`/`count_map` (bounded iteration over a byte slice),
`offset`/`Offset` (u16 offsets into the *start* of a table), `array`/`array_iter`/`ArrayBase`/
`ArrayMap` (fixed-size arrays with `Result` elements), `pascal_string`, `hex_string`,
`varint_u16`/`varint_u32` (the WOFF2 variable-length integers), and the PostScript token
parsers (`name`, `literal_name`, `string`, `delimited_literal`, `integer`, `float`,
`token`, `procedure`, `comment`, `space`).

## Fixed
* **F27 (2026-11-02):** `Offset::of_opt` returns `None` for offset 0, the normal encoding of an
  absent optional subtable; `parse_gdef` and GPOS PairPos use it instead of `unwrap()`.

## Differences from the specifications
* **F28:** `array_iter` (`:355-362`) does `input.split_at(P::SIZE * count)` — a short input is a
  panic, not an error (`array()` at `:364-380` does check and returns `nom::Err::Failure`, so the
  two disagree).
* `Offset` (`:373-395`): `Offset::of` accepts `0 < off <= len`, i.e. **offset 0 is an error**.
  In every OpenType table an offset of 0 means "subtable absent", so callers that `unwrap()`
  (`gpos.rs:121`, `gdef.rs:21`) panic on the normal case instead of skipping the subtable.
* `varint_u32`/`varint_u16` (`:290-315`) implement WOFF2's 255UInt16/Base128 encodings: 253 →
  `be_u16`, 254 → `byte + 506`, 255 → `byte + 253` for the 16-bit form, and the Base128 form
  rejects 0x80 as the first byte (`error!("0x80 not allowed")`) and checks
  `acc & 0xFE_00_00_00 == 0` before shifting, which matches the WOFF2 spec's overflow rule.
* `delimited_literal` (`:88-140`) handles nested parentheses, the `\n \r \t \b \f` escapes, the
  `\ddd` octal escape (as a single byte, *not* as up to three octal digits — PostScript allows
  `\053`), line continuations (`\` + EOL, including CRLF) and the general `\x` → `x`. The octal
  case is a real (if minor) incompatibility with PostScript strings in Type 1 prologs.
* `float` (`:76-87`) accepts `[+-]?digits? '.' digits? ([eE][+-]?digits)?` and `integer` has no
  leading-zero rule; both are used for PostScript tokens, where `1.` and `.5` are legal — both
  are accepted. `token()` prefers `float` before `integer`, so `123` parses as `Real(123.0)`,
  i.e. an integer literal in a DICT becomes a `Value::Float` when it re-enters `cff::dict` —
  `to_int()` then fails. This only affects the PostScript path (`Integer` first would be more
  faithful).
* `name`/`literal_name` (`:30-48`) treat `()<>[]{}/%` and whitespace as separators, which is the
  PostScript rule; `%` correctly starts a comment for `comment()`, but `name` cannot parse a
  name containing `#` or `"` (legal in PostScript names) — rare in Type 1 prologs.
* `take_until_and_consume` (`:142-153`) silently returns the whole input when the filter never
  matches (no `Incomplete`), so a prolog without a trailing newline consumes the rest of the file
  as a comment.
* `integer`/`float` `map_res` closures `unwrap()` on `from_utf8` — always valid here, since the
  recogniser matched ASCII, but it is an unwrap in a parser.

## Cross references
`macros.rs.md` (`Parser`/`NomParser`/`FixedSize`, `table!`/`parser!`), `error.rs.md`
(`FontError`), `cff.rs.md` (`value`/`operator` use the same conventions),
`postscript.rs.md` (token consumers).
