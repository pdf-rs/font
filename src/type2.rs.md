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
* **F6 (2026-11-02):** `hstem`/`vstem` use `maybe_width(s, |n| n % 2 == 0)`, so a width argument is
  only taken when the operand count is odd, like `hstemhm`/`vstemhm`. Tests: `c06`, `c06b`, `c06c`
  in `tests/cff_cases.rs`.
* **F9 (2026-11-02):** `roll` rotates the last `N` operands (`&mut s.stack[len - n .. len]`) and
  accepts `N == stack.len()` (`require!(n <= len)`). Tests: `c07`, `c07b`.

## Differences from the specifications
* **F6 (TN5177 §3.1 + §4.3):** `hstem`/`vstem` decide "is there a width argument?" with
  `maybe_width(s, |n| n == 2)` (`:90`, `:98`). The rule is "width present iff the argument count
  is odd", so with two or more stem pairs the first stem coordinate becomes `deltaWidth` and the
  glyph's advance is wrong. `hstemhm`/`vstemhm` (`:460`, `:475`) use the correct `n % 2 == 0`;
  `rmoveto`/`hmoveto`/`vmoveto`/`endchar` use the correct per-operator counts.
* **F7 (TN5177 §4.1 `flex1`):** `:352-370` wants 16 arguments for an 11-argument operator and
  errors out with "stack empty" on the 3rd/4th `point!`. Every glyph using `flex1` fails, and
  because `CffSlot::outlines()` propagates the error, the **entire font** fails to load.
* **F8 (TN5177 §4.1 `hflex1`):** `:345-350` builds the last segment as `(dx6, 0)`; the spec's
  end point returns to the starting y, i.e. the final delta is `dy6 = -dy5`. (`hflex` at `:335`
  is correct: there all y deltas are zero.)
* **F9 (TN5177 §4.4 `roll`):** `:294` rotates `stack[len-n-1 .. len-1]`; the operand group is
  `stack[len-n .. len]`. The window is shifted by one element.
* **F11 (TN5177 Appendix C):** `endchar` with four arguments is the deprecated accent composition
  (`adx ady bchar achar`); `:462-468` treats the first argument as a width and drops the rest, so
  those glyphs are empty.
* **F42:** `and`/`or`/`not` (12 3/4/5), `put`/`get`/`ifelse` (12 20/21/22) and the
  storage `put`/`get` return an error, which fails the whole font. They are deprecated but legal.
* **F44 (TN5177 §4.4 `index`):** "if `i` is negative, the top element is copied"; the code calls
  `to_usize()` (`:287`) and errors instead.
* **F43:** no recursion depth limit in `callsubr`/`callgsubr` (`:161`, `:490`): a cyclic
  subroutine is a stack overflow, i.e. a process abort, not an error.
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
