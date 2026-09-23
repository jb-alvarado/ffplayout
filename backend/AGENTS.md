# Engine code guidelines

These instructions apply to `backend/`. Prefer clear, maintainable Rust
over clever or overly compact code. Follow the existing architecture and run
`cargo fmt` after editing Rust files.

## Readability

- Give functions, variables, and types names that reveal their purpose. Keep
  functions focused on one coherent task; extract a helper when it makes the
  control flow easier to understand.
- Prefer straightforward control flow and early exits over deeply nested
  branches. Keep related operations together and separate distinct steps with
  a blank line.
- Put a blank line between local variable setup and a following `if`, `for`,
  `while`, `loop`, or `match` statement.
- Put a blank line after an `if`, `for`, `while`, `loop`, or `match` block before
  the next variable declaration or assignment. Keep `else` and `else if`
  attached to their `if`.
- Put a blank line between consecutive control-flow statements (`if`, `for`,
  `while`, `loop`, `match`, and similar constructs). Do not separate an `else`
  or `else if` from its `if`, or add blank lines between `match` arms solely
  for this rule.
- Put a blank line before a function's final return expression, including
  `Ok(...)`, `Err(...)`, an explicit `return`, or another returned value, when
  earlier statements precede it. Do not add an empty line to a function that
  consists only of that expression.
- Separate adjacent methods in `impl` and `trait` blocks with a blank line,
  including methods declared as `pub(crate) fn` or preceded by attributes or
  documentation comments.
- Use comments to explain intent, invariants, and non-obvious trade-offs, not
  to repeat what the code already says.

## Imports and module layout

- Order the top-level groups in each Rust file as follows, with a blank line
  between groups: (1) `std` imports, (2) external-crate imports, (3) `mod` and
  `pub mod` declarations, (4) imports from this crate via `crate::`, and
  (5) relative imports via `super::` or `self::`. Omit groups that are empty.
- Import types used in a file instead of repeating long qualified paths in
  expressions. For example, use `use std::sync::PoisonError;` and then
  `PoisonError::into_inner`, not `std::sync::PoisonError::into_inner` throughout
  the code. Keep a qualified path when it is needed to disambiguate names.

## Reuse and correctness

- Avoid duplicated behavior. Reuse an existing helper or extract shared logic
  when the cases have the same meaning and are likely to change together.
  Do not force unrelated cases into a generic abstraction just to eliminate
  similar-looking lines.
- Keep interfaces small and make ownership, units, and timing assumptions
  explicit, especially at FFmpeg and thread boundaries.
- Propagate errors with useful context. Avoid `unwrap` and `expect` in
  production paths unless an invariant makes failure impossible and that
  invariant is clear nearby.
- Preserve existing behavior unless the task calls for a change. Add or
  update focused tests when behavior changes or a bug is fixed.
