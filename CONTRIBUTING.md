# Contributing to Shaoline

First: Thank you for helping keep the echo area clean and bright in Emacs!

## Philosophy

- Each segment should be a pure function (no side effects!).
- No change should break persistent center or right/left stability.
- All new features should be fully *optional* (user can customize them away).
- "If in doubt, make it zen by default." (minimalism is the yardstick!)

## How to submit

- Open an issue with [bug], [question], or [feature] in the title.
- For bug reports, please include:
  - `emacs-version`, OS, and terminal/gui info.
  - Your value of `shaoline-segments`.
  - Set `(setq shaoline-debug t)` and include relevant logs from `*shaoline-logs*`.
- For features/segments:
  - Make it pure.
  - Use only public Emacs symbols/emacs-lisp constructs.
  - Add tests if possible (see `shaoline-core-test.el`).
  - Update documentation in README.

## Pull Requests

- Use a topic branch (`feature/segment-x`, `bugfix/message-capture`, etc.)
- Run all ERT tests (`M-x ert t` in the root dir, or `emacs -batch -l ert -l shaoline-core-test.el -f ert-run-tests-batch-and-exit`)
- Add yourself to the changelog if you wish.
- Quotes, koans, and docs improvements are *very* welcome!

## Coding style

- Keep all lisp idiomatic and as clear as possible.
- Comments in haiku/random zen are appreciated.
- Remember: silence is a valid patch if nothing needs fixing.

Thank you!
