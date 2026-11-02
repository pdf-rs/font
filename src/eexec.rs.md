# `src/eexec.rs` — Type 1 eexec / charstring decryption

## What it does
The standard Type 1 `eexec` cipher: `plain = cipher ^ (r >> 8)`,
`r = (cipher + r) * 52845 + 22719`, with the documented starting keys — `55665` for the font
program (eexec) and `4330` for charstrings. `decode(data, skip)` decrypts and drops the first
`skip` bytes (that is `lenIV`); `ExecReader` exposes the same thing as `io::Read`.

## Differences from the specifications
* Correct per T1 §7 ("Encryption") — constants, key schedule and both start keys are right, and
  `decode` skipping `lenIV` bytes matches "the first four bytes are discarded".
* `decode_inline` exists for in-place decryption and is unused.
* The `skip` handling is `skip` bytes of *plaintext* (`decode` then skip on the decrypted
  iterator) — same result, but it means a `skip >= data.len()` returns an empty vector rather
  than an error.
* `lenIV = -1` ("not encrypted at all") is the caller's problem: `type1.rs` casts it to `usize`
  (**F41** in the audit).
* `ExecReader` is dead code — `eexec` in `postscript.rs` uses `Decoder::decode` on the remaining
  input instead.
