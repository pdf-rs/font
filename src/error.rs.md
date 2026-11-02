# `src/error.rs` — `FontError` and the `t!`/`slice!`/`expect!`/`require!` family

## What it does
`FontError` variants: `ParseCharString`, `NoSuchSlot`, `Parse(nom errors)`, `UnknownMagic`,
`Other(String)`, `TypeError(&'static str)`, `OutOfBounds(file, line)`, `Get(file, line, what)`,
`Require(&'static str)`, `Reserved(file, line, byte)`, `Key(file, line, byte)`,
`Context(file, line, inner)`.

Macros: `t!` (wrap an error with file/line context), `slice!` (bounds-checked subslice, `Err` on
failure), `offset!`, `expect!` (turn `None` into `Other(msg)`), `require!`/`require_eq!`, `error!`,
`key!`, `reserved!`, `get!` (indexed `get` with an error).

## Differences from the specifications / usability
* **D3:** `From<nom::Err<VerboseError<&[u8]>>>` (`:20-25`) throws away the *byte offsets* and
  keeps only the `VerboseErrorKind`s. `FontError::Parse(Vec<VerboseErrorKind>)` therefore says
  "Tag/Char/etc." without saying where — the single most useful datum when a font fails. Keeping
  the input slices (or the offset relative to the table) would make every parse error actionable.
* `slice!` reports `OutOfBounds(file!(), line!())` — the *crate* location, not the table and
  offsets that failed. `Get`/`Key`/`Reserved` at least carry a byte value, never an offset.
* **F29:** `lib.rs:441-449` (`print_err`) panics after printing the nom error context; it is the
  only "diagnostic" path and it aborts instead of reporting.
* `error!` produces `Other(String)`, so every "unsupported feature" and every "malformed input"
  end up in the same bucket — callers cannot distinguish "this font uses a feature you do not
  implement" (recoverable: fall back, skip the glyph) from "this font is corrupt" (fatal).
* `require_eq!` expands to `require!($a == $b)` and loses the values from the message
  (`Require("a == b")`), so a failing `require_eq!` never says what it saw.
* `FontError` is not `Clone`/`PartialEq`, which makes it awkward to assert on in tests
  (`matches!(err, FontError::Other(_))` is all you get).
