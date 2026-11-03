# `src/type2.rs` — Type 2 charstring interpreter (CFF outlines)

## What it does
A byte-code interpreter over `State`/`Context`, implementing TN5177 §4. A `#[macro_use]`-style
macro pair (`point!`, `bezier!`, `lines!`) keeps the multi-argument operators short: each operator
consumes `k` operands from the bottom of the stack, one curve/line at a time, and returns the
remaining slice.

Implemented: all path operators (1, 3, 4, 5, 6, 7, 8, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30,
31), `endchar` (14), subroutines (10, 29, 11), and most `12 x` escapes (9 abs, 10 add, 11 sub,
12 div, 14 neg, 18 drop, 23 random, 24 mul, 26 sqrt, 27 dup, 28 exch, 29 index, 30 roll,
34 hflex, 35 flex, 36 hflex1, 37 flex1).

## Fixed
* **F11 (2026-11-02):** `endchar` with four arguments (plus an optional leading width) is the
  deprecated `seac` accent composition (TN5177 Appendix C). It now records the operands in
  `State::seac`; `CffSlot::composed_outlines` (`src/cff.rs`) composes base + accent, the accent
  translated by `(adx, ady)`. Test: `c24` in `tests/cff_cases.rs`.
* **F6 (2026-11-02):** `hstem`/`vstem` use `maybe_width(s, |n| n % 2 == 0)`, so a width argument is
  only taken when the operand count is odd, like `hstemhm`/`vstemhm`. Tests: `c06`, `c06b`, `c06c`
  in `tests/cff_cases.rs`.
* **F9 (2026-11-02):** `roll` rotates the last `N` operands (`&mut s.stack[len - n .. len]`) and
  accepts `N == stack.len()` (`require!(n <= len)`). Tests: `c07`, `c07b`.
* **F8 (2026-11-02):** `hflex1` no longer uses the generic `bezier!` pattern. The last segment's
  delta is `(dx6, -(dy1 + dy2 + dy5))`, so the curve ends on the starting y as the operator's
  precondition requires (TN5177 §4.1: "the starting and ending points have the same y value").
  Tests: `c08c`, `c08d`, `c08e` (the `hflex` control is unchanged).
* **F43 (2026-11-03):** `callsubr` (10) and `callgsubr` (29) go through the new `call_subr`, which
  refuses to nest more than `crate::MAX_SUBR_DEPTH` (16) levels — TN5177 allows 10, the extra room
  is for real fonts that overreach. A subr that calls itself now fails that charstring (and so
  that glyph) with an error instead of recursing until the native stack dies and aborts the whole
  process. The counter (`State::subr_depth`, shared with Type 1) is restored on every path, so a
  bad glyph does not poison the next one. Tests: `c27` (`tests/cff_cases.rs`, self-recursive
  global subr) and `c27b` (positive control, a legal two-level nesting still works).

## Differences from the specifications
* **F7 — withdrawn (2026-11-02):** `flex1` consumes exactly 11 arguments (6 for the first curve,
  10 for the `(dx, dy)` decision — read from the start of the stack, which is the same 10 values —
  and 5 for the second curve) and picks `(d6, 0)` vs `(0, d6)` correctly. The original finding
  counted the decision loop as consuming new operands. `c08`/`c08b` pin the horizontal and
  vertical cases.
* ~~**F11 (TN5177 Appendix C):**~~ `endchar` with four arguments is the deprecated accent
  composition (`adx ady bchar achar`) — implemented, see "Fixed" above.
* **F42:** `and`/`or`/`not` (12 3/4/5), `put`/`get`/`ifelse` (12 20/21/22) and the
  storage `put`/`get` return an error, which fails the whole font. They are deprecated but legal.
* **F44 (TN5177 §4.4 `index`):** "if `i` is negative, the top element is copied"; the code calls
  `to_usize()` (`:287`) and errors instead.
* ~~**F43:**~~ `callsubr`/`callgsubr` used to recurse with no depth limit (`:161`, `:490`), so a
  cyclic subroutine was a stack overflow — a process abort, not an error (guarded by
  `call_subr` now, see "Fixed" above).
* `12 0` (dotsection) is treated as "clear the stack and continue" — it is deprecated/no-op in
  Type 2, so this is fine, but it also clears the stack first (correct) and does *not* count as a
  stack-clearing operator for the width (`first_stack_clearing_operator` is left alone), which is
  the right choice.
* `12 23` (random) uses `rand::thread_rng()` — non-deterministic output means two runs of the
  same font can differ in glyph outlines (only in fonts that use `random`, which is rare, but it
  also makes golden-image tests flaky).
* `12 30 roll` aside, the arithmetic operators use `to_float()` and mix `Int`/`Float` per the
  spec's "numbers are numbers" model ✓.
* Operators are handled with `error!` for reserved values (2, 9, 13, 15-17, 25, 31-33, 38-255,
  `12 1/2/6/7/8/13/15/16/17/19/25/31/32/33`) — the spec says undefined operators' behaviour is
  unspecified, so erroring is defensible, but a single reserved byte then disables the whole font.

## Cross references
`lib.rs.md` (`State`, `maybe_width`, `Context`), `cff.rs.md` (`CffSlot::outlines` is the only
caller), `type1.rs.md` (the other dialect of the same state machine).
