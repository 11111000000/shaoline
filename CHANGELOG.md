# Changelog

## 3.3.6 — 2026-07-24

- Use the actual echo-area window width instead of the frame width.
- Replace display alignment with a bounded plain-text spacer.
- Keep a two-column safety margin so echo-area output stays on one row.
- Restore Shaoline after the echo area is cleared without redundant redraws.
- Refresh cursor position on movement commands while keeping the display stable.
- Add regression coverage for width calculation, overflow, and visibility restoration.
