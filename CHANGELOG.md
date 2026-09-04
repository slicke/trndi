# Changelog

All notable user-facing changes to Trndi. The most recent release is at the top.

## Version 19 (build 247) — 2026-09-04

Changes since Trndi 18 (build 246, 2026-08-26).

### Highlights

- **Cleaner main window.** The forecast, change, time-in-range and reading-age texts have been rebuilt as clearer badges and strips that read well at any size and look the same on every platform.
- **Optional trend line** through the dots, in thin, normal or thick weight.
- **New "Trend graph" settings page** with its own live preview, taking the crowded graph options off the Display page.
- **Errors are no longer swallowed.** An unhandled error now shows once in a dialog with the details ready for a bug report.

### Main window

- **Forecast strip.** The text forecast is no longer one long label. Each horizon gets its own cell with a muted "+5 min" header over the arrow and value, coloured by the range the prediction lands in. The two code paths that used to render it had drifted apart, so mg/dL no longer shows a stray decimal and the "no change predicted" message is no longer overwritten a second later.
- **Change tint.** The change-since-last-reading label is tinted high or low when the rate is steep, and stays muted for gentle drift.
- **Time-in-range badge.** The percentage now sits over an "in range" caption, with the average (when enabled) as its own "avg" part. Hover it to see a thin bar filled to your in-range share. When space is short it drops the "avg" word, then the mean, so the percentage always fits.
- **Reading-age badge.** The "🕑 3 min" label, which rendered differently on every platform and as a box on GTK2, is now a badge matching the time-in-range one. With the timestamp display on it shows the clock time over a "last reading" caption. The fullscreen clock keeps the badge's colour.
- **Refresh countdown bar.** Reworked as two flush lines: one fills until the next fetch is due, and a thin second line appears only while a reading is late. When both are full the bar breathes red until a reading lands. The hover hint is gone.

### Trend graph

- **Optional trend line** connecting the dots, off by default. It wears the dots' own colours, takes the neutral gap tone across missing readings, and hides while dots are expanded into their values. Thickness is selectable next to the checkbox.
- The dots are drawn on one surface instead of one control per dot, which makes resizing and relayout noticeably lighter. Clicks, hover tooltips, the popup menu and window dragging work as before.
- The settings miniature previews the line and its thickness as you change them.

### Settings

- **New "Trend graph" page** with the window span, dot options and graph overlays, each with its own preview miniature. The Display page keeps the window and label settings.

### Dialogs and messages

- Enter now activates the focused button rather than always answering OK, so a dialog defaulting to *Snooze* no longer answers *Close*.
- On touch screens, a new on-window message replaces the one already showing instead of opening a modal on top of it, and the drawn title bar stays visible underneath.
- Unhandled errors are shown once in an error dialog with the details available for bug reports, instead of being silently discarded. Repeats while the dialog is up are logged, not stacked.
- Windows: emoji icons fall back to plain glyphs when Direct2D is unavailable (remote desktop, broken drivers) instead of crashing the error dialog itself.
- The first-run wizard's web-login prompts no longer collapse into a wall of text.

### Extensions

- **TypeScript declarations** for the whole extensions API ship in `guides/extensions/trndi.d.ts`, with notes on type-checked JavaScript and compiling extensions from TypeScript. A leading `"use strict";` emitted by the compiler no longer strips the manifest of its name and permissions.

### Reliability

- Fetches wait while a prediction call is running instead of racing it.
- The startup fetch takes the same path as every other refresh.
- Worker threads are detached cleanly at shutdown instead of racing the runtime.

### macOS

- Build fixes.

### Translations

- Swedish translations updated for all new strings.
