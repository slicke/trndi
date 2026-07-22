# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Trndi is a cross-platform (Windows/macOS/Linux/BSD/Haiku) Free Pascal/Lazarus desktop app that displays CGM (continuous glucose monitor) data from Nightscout, Dexcom, Tandem Source, CareLink, and xDrip. See also AGENTS.md, which contains the working rules for this repo (including the mandatory GPLv3 + medical-disclaimer file header for all source files).

## Commands

Windows (this machine — `lazbuild` is not on PATH; the scripts find it at `C:\lazarus\lazbuild.exe`):

```powershell
.\make.ps1              # release build (Extensions (Release) mode)
.\make.ps1 debug        # debug build
.\make.ps1 noext        # build without extensions (no QuickJS dependency)
.\make.ps1 test         # build tests/TrndiTestConsole.lpi and run it
.\make.ps1 clean        # remove build artifacts (-n for dry run)
.\make.ps1 list-modules # tree of all units
.\make.ps1 assets       # regenerate assets/carelink_assets.lrs via lazres
$env:TRNDI_NO_TESTSERVER = '1'; .\make.ps1 test   # skip integration tests
```

Linux/BSD/Haiku use `make` with the same target names (`make`, `make debug`, `make test`, `make test-noserver`, `make noext`, `make list-modes`); macOS uses `gmake`. On this Windows machine, Linux-side checks can be run through WSL (e.g. `wsl fpc`). Prefer the smallest relevant target; run `make help` before inventing commands.

### Tests

- The runner (`tests/TrndiTestConsole.lpr`) always runs **all** registered FPCUnit tests — there is no single-test filter. To add a test unit, add it to the runner's `uses` clause.
- Integration tests spawn an embedded fake Nightscout server in-process on `localhost:8080` (`tests/testserver/pascal_testserver.pp`). `TRNDI_TEST_SERVER_URL` reuses an external server; `TRNDI_NO_TESTSERVER=1` skips integration tests. If port 8080 is busy, those tests fail.
- `tests/mock/` contains LCL stubs so tests build without a GUI; tests must be deterministic and offline.

## Architecture

Build modes (in `Trndi.lpi`): "Extensions" modes define `TrndiExt` and link the prebuilt QuickJS libraries in `externals/quickjs/prebuilt/`; "No Ext" modes strip JS extension support. Extension-related code is gated with `{$IFDEF TrndiExt}`. Linux defaults to Qt6 widgetset modes; Windows/macOS use native widgetsets.

### Layering

- **`units/forms/`** — LCL forms. `umain.pp` is the main window; it is deliberately thin and pulls its implementation from **`inc/umain_*.inc`** include files (`umain_glucose.inc`, `umain_alerts.inc`, `umain_paint.inc`, `umain_settings.inc`, `umain_async.inc`, …). New umain code goes in the matching `.inc`, not into `umain.pp` itself. `uconf` is settings, `ufloat` the floating window, `uwizard` first-run setup.
- **`units/trndi/api/`** — backend drivers. All subclass `TrndiAPI` (`trndi.api.pp`) and are registered in `trndi.api.registry.pp`. Contract (see `guides/API.md` and CONTRIBUTING.md): `constructor create(user, pass, extra)` sets `ua` and `baseUrl`; `connect: boolean` populates thresholds and `timeDiff`; `getReadings(...): BGResults` returns readings; errors go to `lastErr` with a `false` return. The many `trndi.api.debug_*.pp` units are synthetic backends simulating sensor conditions (missing readings, expiry, etc.) used for manual and automated testing.
- **`units/trndi/trndi.native.*`** — platform abstraction. `trndi.native.base.pp` declares the interface; `trndi.native.win/linux/mac/bsd/haiku.pp` implement it; `trndi.native.pp` selects the platform class. **Never add platform `{$IFDEF}` blocks to `trndi.native.base`** — put platform code in the platform unit. HTTP requests go through `native.request`.
- **`units/trndi/ext/`** — JavaScript extension engine (quickjs-ng via the in-tree binding `trndi.ext.quickjs.pp` and the C ABI shim in `externals/quickjs/`). `trndi.ext.engine.pp` is the interpreter host; JS-callable functions live in `inc/js_*_funcs.inc`; `trndi.ext.grant/perm` handle the permission system.
- **`units/slicke/`** — reusable UI/system helpers (custom alert dialogs in `slicke.ux.alert.pp`, media controller, touch detection).
- **`units/misc/`** — platform utility bundles (winutils, linutils, nsutils, libpascurl, Razer Chroma).
- **`lang/`** — `.po` translations (6 languages; `Trndi.jm.po` is Jämtlandic, not Jamaican).

Shared value types (`BGReading`, `BGResults`, trend enums) live in `trndi.types.pp` and `units/trndi/api/` drivers map every backend into them — the UI layer only ever sees these types.

## Conventions

- Branch from `develop`; PRs target `develop`, not `main`.
- Unit files use the `.pp` extension. Formatting follows `JCFSettings.xml` (JEDI Code Formatter).
- Every source file starts with the standard Trndi header (GPLv3 + medical disclaimer) — copy it verbatim from AGENTS.md when creating files.
- PasDoc `{** ... }` comments on public APIs in interfaces; banner-style comments in implementations. Keep PasDoc out of implementation sections.
- Keep changes focused; no unrelated refactors or formatting-only edits. Update user-facing docs (README.md, MANUAL.md, guides/) when behavior changes.
- Cross-platform gotcha: `{$IFDEF UNIX}` does not cover Haiku — use `{$IF DEFINED(UNIX) OR DEFINED(HAIKU)}` where both apply.
- Platform defines: `inc/native.inc` (included at the top of the native units) maps FPC's raw target symbols to Trndi's own `X_*` conditionals and most platform switches use these. The dispatch matches in order `WINDOWS→X_WIN`, `DARWIN→X_MAC`, `HAIKU→X_HAIKU`, then generic `UNIX→X_PC` — so the buckets are: `X_POSIX` = all Unix incl. macOS and Haiku; `X_PC` = **Linux/BSD only** (Haiku is deliberately excluded, matched before the UNIX test); `X_LINUXBSD` = Linux/BSD only. Because Haiku is matched explicitly and before `UNIX`, the classification is robust whether or not a given FPC defines `UNIX` for Haiku. Gate Haiku-specific behavior on `X_HAIKU`/`DEFINED(HAIKU)`. Exception to the X_* convention: `trndi.native.pp`'s unit/class selection switches on raw `HAIKU`/`BSD`.
- Don't edit `backup/` directories, `link*.res`, or other generated artifacts.
