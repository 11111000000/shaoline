# Changelog

## 3.3.8 — 2026-07-24

- Preserve the rightmost segment, including the moon, during pixel fitting.
- Remove yang reassertion and repeating display timers from adaptive mode.
- Track and clean global hooks consistently across buffer-local hook values.
- Stop treating unrelated system load as a reason to switch display effects.
- Add regression coverage for moon preservation and hook reconciliation.

## 3.3.7 — 2026-07-24

- Keep adaptive as a stable strategy instead of repeatedly reinstalling yin/yang effects.
- Install one consistent hook and timer set across context changes.
- Trim the composed line against the actual echo-area pixel width.
- Make hook registration idempotent and cancel stale yang timers.
- Set single-line truncation in the minibuffer buffer used by the echo area.

## 3.3.6 — 2026-07-24

- Use the actual echo-area window width instead of the frame width.
- Replace display alignment with a bounded plain-text spacer.
- Keep a two-column safety margin so echo-area output stays on one row.
- Restore Shaoline after the echo area is cleared without redundant redraws.
- Refresh cursor position on movement commands while keeping the display stable.
- Add regression coverage for width calculation, overflow, and visibility restoration.
