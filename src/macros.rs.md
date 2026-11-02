# `src/macros.rs` — `parser!` and `table!` declarative table parsers

## What it does
* `parser!(name : fn -> Type)` turns a nom parser fn into a zero-sized `NomParser` with a known
  `FixedSize` (`size_of::<Type>()`).
* `table!(Name { [@|?]Parser field, … })` generates a struct plus `NomParser`/`FixedSize` impls by
  folding the field parsers: plain fields are parsed in place from the cursor, `@ptr` fields are
  mandatory offsets (`Offset::of`-style `slice!`), `?ptr` fields are optional offsets (zero ⇒
  `Default::default()`), and `FixedSize::SIZE` is the sum of the field sizes, with `@`/`?` pointer
  fields contributing the *pointer's* size.
* `field_size!` sums pointer sizes for `@`/`?` fields.

## Differences from the specifications
This is a mechanism, not a spec implementation; the differences it causes live in the tables it
builds.
* `FixedSize` for a table containing `@`/`?` fields is the size of the *serialised header*
  (pointers are `u16`), which is what you want for array indexing — but the macro does not stop
  you from using a table with variable-size members as an array element, which would silently
  mis-index.
* The `?ptr` form yields `Default::default()` for the whole struct (`(i, Default::default())` —
  note `i`, the cursor *after* the pointer, is kept ✓) and the `@ptr` form `require!(offset != 0)`.
  So "absent" is expressible only via `?`, and the `Offset` type itself rejects 0 — see
  **F27** in the audit for the callers that `unwrap()` instead.
* `parser!` uses `std::mem::size_of::<$out>()` as `SIZE`; correct for the integer/newtype
  wrappers it is used with (`int16`, `uint16`, `Tag`, `Offset`), wrong in general for Rust types
  whose size differs from their serialised size (none are used that way today).
* No bounds checks are generated: `table!` reads fields with nom parsers that return `Err` on
  short input, which is correct, but the offsets are `slice!`'d without a length check beyond the
  end of the table.
