# Contributing to Trndi

Thanks for your interest in contributing! This guide explains how to propose changes, coding/documentation style, and how to run docs/build locally.

## Branching and Pull Requests

**Always work from the `develop` branch:**
- Clone the repository and checkout the `develop` branch:
  ```bash
  git clone https://github.com/slicke/trndi.git
  cd trndi
  git checkout develop
  ```
- Create your feature branch from `develop`.
- When ready, open a pull request (PR) targeting the `develop` branch (not `main`).

This keeps main stable and ensures new features and fixes are integrated smoothly.

## Getting started

- Fork the repo and create a feature branch from `develop`.
- For Lazarus/FPC projects, keep edits minimal and focused. Prefer small PRs.
- If your change affects public behavior or API, update docs and add a brief test/example when practical.

## Code style

- Language: Free Pascal/Lazarus. Keep cross-platform code in platform units under `units/trndi/trndi.native.*.pp`.
- Avoid platform `{$IFDEF}` blocks in the base unit (`trndi.native.base`); implement platform specifics in `trndi.native.win|linux|mac`.
- Keep methods small and intention-revealing; add a short comment explaining why when non-obvious.
- Prefer pure Pascal for helpers over shelling out unless a native tool is the correct choice.

## Code Quality

- **Keep builds clean:** Aim to reduce compiler hints, notes, and warnings. Run builds regularly and address issues like unused units, variables, or parameters.
- **Remove unused units:** Before committing, check for "Unit X not used" hints and remove unnecessary `uses` clauses to keep code tidy and improve compile times.
- **Avoid unused parameters:** If a parameter is not used in a method (common in event handlers), prefix it with an underscore (e.g., `Sender: TObject`) or remove it if possible. If removal breaks interfaces, document why it's unused.
- **Initialize variables:** Ensure local variables are properly initialized to avoid "not initialized" warnings.
- **Type safety:** Use explicit type conversions and avoid implicit ones that may lose data.
- **Testing:** Add tests for new features and ensure existing tests pass. Use the Makefile targets for running tests.

## Documentation (PasDoc)

We use PasDoc comments throughout. Please follow this syntax:


Example:

```
{**
  @abstract(Windows implementation of @link(TTrndiNativeBase).)
  Uses SAPI for speech and DWM for window appearance tweaks.
}
TTrndiNativeWindows = class(TTrndiNativeBase)
public
  {** Speaks @param(Text) using SAPI; falls back to default voice if a
      locale-matching voice is not found. }
  procedure Speak(const Text: string); override;

  {** Simple HTTP GET.
      @param(url URL to fetch)
      @param(res Out parameter receiving response body or error message)
      @returns(True on success) }
  class function getURL(const url: string; out res: string): boolean; override;
end;
```

### Implementation vs interface comments

Use PasDoc blocks in the interface and banner-style separators in the implementation:

- Interface (public API): `{** ... }` with `@abstract`, `@param`, `@returns`, `@seealso`.
- Implementation (for navigation/readability): banner-style comments; PasDoc ignores these.

Example banner in implementation:

```
{------------------------------------------------------------------------------
  SetBadge
  --------
  Compose app icon with a badge showing Value; applies to taskbar icon.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.SetBadge(const Value: string; BadgeColor: TColor; badge_size_ratio: double; min_font_size: integer);
```

This keeps public docs in one place (interface) while making implementation sections skimmable.

### Local/nested helper functions

For private/local helpers inside the implementation section (including nested functions used within a single method), add a short comment describing intent. Choose the lightest-weight form that stays readable in the file:

- Use a small banner when the helper is non-trivial, widely reused within the unit, or important for navigation (e.g., DetectKDEDark, DetectGnomeDark). Example:

  ````
  {------------------------------------------------------------------------------
    DetectKDEDark
    -------------
    KDE Plasma: read General/ColorScheme via kreadconfig5. Returns True if a
    decision was made and sets isDark accordingly.
   ------------------------------------------------------------------------------}
  function DetectKDEDark(out isDark: boolean): boolean;
  ````

- Use a single-line `//` comment for small, obvious helpers (e.g., `ContainsDark`, `EnvValue`). Example:

  ````
  // True if S contains the substring "dark" (case-insensitive)
  function ContainsDark(const S: string): boolean; inline;
  ````

Rule of thumb: if the helper’s behavior wouldn’t be obvious from the name and a quick skim, prefer a banner; otherwise a concise `//` is fine. Keep PasDoc blocks in interfaces only.

### API units (TrndiAPI and drivers)

API implementations live under `units/trndi/api/` and subclass `TrndiAPI`.

Required methods:
- `constructor create(user, pass, extra: string)`
- `function connect: boolean` (establishes connectivity, sets thresholds/timeDiff)
- `function getReadings(minNum, maxNum: integer; extras: string; out res: string): BGResults`

Guidelines:
- Set `ua` (User-Agent) and `baseUrl` in the constructor.
- Use `native.request` or platform helpers (`TrndiNative.getURL`) for HTTP.
- Populate `core` thresholds and `timeDiff` during `connect`.
- Keep errors in `lastErr` and prefer returning False on failures.
- Add PasDoc to classes and methods with `{** ... }`, `@param`, `@returns`, and `@link` to shared types (e.g., `BGReading`, `BGResults`).
- Keep behavior consistent with the contract described in `guides/API.md`.

PasDoc example for an API driver method:

```
{** Fetch SGV readings and map them to @code(BGResults).
    @param(minNum Time-span hint in minutes; driver-specific)
    @param(maxNum Maximum number of readings to fetch)
    @param(extras Optional endpoint or query override)
    @param(res    Out parameter receiving the raw response text)
    @returns(Array of @link(BGReading); empty on error) }
function getReadings(minNum, maxNum: integer; extras: string; out res: string): BGResults; override;
```

See also: `guides/API.md` for an overview of the API surface and expectations.
Tips:
- When PasDoc struggles to parse helpers, guard them with `{$IFNDEF PASDOC}` and define `PASDOC` during doc generation.
- Keep unit-level docs at the top with a short description and responsibilities list.

## Building

- Build with Lazarus using the project file `Trndi.lpi`.
- If you encounter environment-related unit errors (e.g., missing widgetset), ensure your Lazarus packages and widgetsets are installed.

### Makefile (common targets)

- `make` — release build (default).
- `make debug` — debug build.
- `make build` — generic build (honors `BUILD_MODE` and `WIDGETSET`).
- `make test` — build and run tests (starts PHP test server when available).
- `make test-nophp` — console tests without PHP (`TRNDI_NO_PHP=1`).
- `make noext` — build without mORMot2 (temporary project).
- `make fetch-mormot2` — clone mORMot2 into `externals/mORMot2` and attempt to extract QuickJS static artifacts into `./static`.
- `make check-mormot2` — verify presence of mORMot2 and QuickJS static artifacts.
- `make clean` — remove build artifacts.
- `make dist` — create a minimal tarball in `build/`.
- `make run` — build (if needed) and run the binary (use `RUN_ARGS` to pass args).
- `make install` — install built binary to `/usr/local/bin` (requires `sudo`).

**Common variables:** `LAZBUILD`, `WIDGETSET`, `BUILD_MODE`, `IGNORE_MORMOT`.

See the project `Makefile` or the README `Makefile` section for full details.

**Example workflows:**
- Build and run tests: `make test` (or `make test-nophp` for CI).
- Local development: `make debug` → `make run`.

## Running tests

- Use the project Makefile from the repository root for convenient, documented test runs:
  - `make test` — builds the console runner and runs tests; it will start PHP's built-in test server automatically when `php` is on PATH (used for integration tests).

    - _Note:_ On macOS, use ```gmake```

  - On Windows, `make.ps1` auto-detects PHP using `TRNDI_PHP_EXECUTABLE` (if set), `C:\php\php.exe`, then `php` on PATH. If a usable PHP binary is found, `make.ps1` will clear `TRNDI_NO_PHP` so PHP-backed integration tests can run. You can also explicitly set `TRNDI_PHP_EXECUTABLE` to a full path (or `php` to probe PATH).

  - The console tests print a short one-time message `Test Server tests ignored, missing php` when PHP is not available and will print `Running Test Server` when the helper starts the local PHP server.

  - `make test-nophp` — builds and runs the console tests with `TRNDI_NO_PHP=1` so integration tests that require PHP are skipped (useful on CI).

- Run the console test runner manually:
  ```bash
  lazbuild tests/TrndiTestConsole.lpi
  ./tests/TrndiTestConsole
  ```
  To enable integration tests with an existing test server, set `TRNDI_TEST_SERVER_URL` (e.g. `export TRNDI_TEST_SERVER_URL=http://localhost:8080`).

- GUI tests can be run with `TrndiTest` (uses the GUI test runner); for headless environments use `xvfb-run -a ./tests/TrndiTest`.

## Generating docs locally

Run from the repo root:

```
# Native units
pasdoc --output=doc/pasdoc-all --name "Trndi Native (All)" \
  units/trndi/trndi.native.base.pp \
  units/trndi/trndi.native.win.pp \
  units/trndi/trndi.native.mac.pp \
  units/trndi/trndi.native.linux.pp \
  units/trndi/trndi.native.pp

# Extensions (skip helpers via PASDOC define)
pasdoc --define=PASDOC --output=doc/pasdoc-ext --name "Trndi Extensions" \
  units/trndi/ext/trndi.ext.functions.pp \
  units/trndi/ext/trndi.ext.ext.pp

# API units
pasdoc --output=doc/pasdoc-api --name "Trndi API" \
  units/trndi/api/trndi.api.pp \
  units/trndi/api/trndi.api.nightscout.pp \
  units/trndi/api/trndi.api.dexcom.pp \
  units/trndi/api/trndi.api.xdrip.pp \
  units/trndi/api/trndi.api.debug.pp \
  units/trndi/api/trndi.api.debug_edge.pp
```

## Submitting a pull request

- Make sure your branch builds (or note any environment-only blockers).
- Include concise, descriptive commit messages.
- Reference issues (e.g., "Fixes #123") when applicable.
- Be ready to iterate based on review feedback.

Thanks for helping make Trndi better!