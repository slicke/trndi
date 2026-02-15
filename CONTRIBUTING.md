# Contributing to Trndi

Welcome! Thank you for your interest in improving Trndi. This guide will help you get started, understand our coding and documentation style, and run builds/tests locally.

---

## Quickstart

1. **Fork & Clone**: Fork the repo, then clone your fork.
2. **Branch**: Always create your feature branch from `develop`.
3. **Code & Document**: Follow our code style and documentation guidelines.
4. **Build & Test**: Use the Makefile for builds and tests.
5. **Pull Request**: Open a PR targeting `develop`.

---

## Branching & Pull Requests

- Work from the `develop` branch (not `main`).
- Create your feature branch from `develop`.
- Open your PR against `develop` when ready.

Example:
```bash
git clone https://github.com/slicke/trndi.git
cd trndi
git checkout develop
git checkout -b my-feature
```

This keeps `main` stable and integrates new features smoothly.

## Getting Started

- Fork the repo, clone, and branch from `develop`.
- Keep edits minimal and focused (small PRs are best).
- If your change affects public behavior or API, update docs and add a brief test/example.

## Code Style

- Language: Free Pascal/Lazarus.
- Cross-platform code: Use platform units under `units/trndi/trndi.native.*.pp`.
- Avoid platform `{$IFDEF}` blocks in `trndi.native.base`; use `trndi.native.win|linux|mac` for specifics.
- Keep methods small and intention-revealing. Add a short comment if the purpose isn’t obvious.
- Prefer pure Pascal for helpers unless a native tool is required.
- **Formatting:** We use JEDI Code Formatter (see `JCFSettings.xml` in the repo) for consistent code style. Please format your code before submitting a PR.

## Code Quality

- **Keep builds clean:** Aim to reduce compiler hints, notes, and warnings. Run builds regularly and address issues like unused units, variables, or parameters.
- **Remove unused units:** Before committing, check for "Unit X not used" hints and remove unnecessary `uses` clauses to keep code tidy and improve compile times.
- **Avoid unused parameters:** If a parameter is not used in a method (common in event handlers), prefix it with an underscore (e.g., `Sender: TObject`) or remove it if possible. If removal breaks interfaces, document why it's unused.
- **Initialize variables:** Ensure local variables are properly initialized to avoid "not initialized" warnings.
- **Type safety:** Use explicit type conversions and avoid implicit ones that may lose data.
- **Testing:** Add tests for new features and ensure existing tests pass. Use the Makefile targets for running tests.

## Documentation Guidelines (PasDoc)

We use PasDoc comments for public APIs. Please use:

- `{** ... }` blocks in interfaces, with `@abstract`, `@param`, `@returns`, `@seealso`.
- Banner-style comments in implementations for navigation/readability.

**Example (interface):**
```pascal
{**
  @abstract(Windows implementation of @link(TTrndiNativeBase).)
  Uses SAPI for speech and DWM for window appearance tweaks.
}
TTrndiNativeWindows = class(TTrndiNativeBase)
public
  {** Speaks @param(Text) using SAPI; falls back to default voice if a locale-matching voice is not found. }
  procedure Speak(const Text: string); override;
```

**Example (implementation banner):**
```pascal
{------------------------------------------------------------------------------
  SetBadge
  --------
  Compose app icon with a badge showing Value; applies to taskbar icon.
 ------------------------------------------------------------------------------}
procedure TTrndiNativeWindows.SetBadge(...);
```

**Helpers:**
- Use a banner for non-trivial or widely reused helpers.
- Use a single-line `//` comment for small, obvious helpers.

**Rule of thumb:** If the helper’s behavior isn’t obvious, use a banner. Keep PasDoc blocks in interfaces only.

### Local/nested helper functions

For private/local helpers inside the implementation section (including nested functions used within a single method), add a short comment describing intent. Choose the lightest-weight form that stays readable in the file:

- Use a small banner when the helper is non-trivial, widely reused within the unit, or important for navigation (e.g., DetectKDEDark, DetectGnomeDark). Example:

  ```pascal
  {------------------------------------------------------------------------------
    DetectKDEDark
    -------------
    KDE Plasma: read General/ColorScheme via kreadconfig5. Returns True if a
    decision was made and sets isDark accordingly.
   ------------------------------------------------------------------------------}
  function DetectKDEDark(out isDark: boolean): boolean;
  ```

- Use a single-line `//` comment for small, obvious helpers (e.g., `ContainsDark`, `EnvValue`). Example:

  ```pascal
  // True if S contains the substring "dark" (case-insensitive)
  function ContainsDark(const S: string): boolean; inline;
  ```

Rule of thumb: if the helper’s behavior wouldn’t be obvious from the name and a quick skim, prefer a banner; otherwise a concise `//` is fine. Keep PasDoc blocks in interfaces only.

## API Units & Drivers

API implementations live under `units/trndi/api/` and subclass `TrndiAPI`.

**Required methods:**
- `constructor create(user, pass, extra: string)`
- `function connect: boolean` (establishes connectivity)
- `function getReadings(minNum, maxNum: integer; extras: string; out res: string): BGResults`

**Guidelines:**
- Set `ua` (User-Agent) and `baseUrl` in the constructor.
- Use `native.request` or platform helpers for HTTP.
- Populate thresholds and `timeDiff` during `connect`.
- Keep errors in `lastErr` and return False on failures.
- Add PasDoc to classes/methods, linking to shared types.
- Follow the contract in `guides/API.md`.

**Example:**
```pascal
{** Fetch SGV readings and map them to @code(BGResults).
  @param(minNum Time-span hint in minutes; driver-specific)
  @param(maxNum Maximum number of readings to fetch)
  @param(extras Optional endpoint or query override)
  @param(res    Out parameter receiving the raw response text)
  @returns(Array of @link(BGReading); empty on error) }
function getReadings(minNum, maxNum: integer; extras: string; out res: string): BGResults; override;
```

## Building & Makefile

- Build with Lazarus using `Trndi.lpi`.
- If you see unit errors (e.g., missing widgetset), check your Lazarus packages.

**Common Makefile targets:**
- `make` — release build
- `make debug` — debug build
- `make build` — generic build
- `make test` — build and run tests
- `make test-nophp` — tests without PHP
- `make noext` — build without mORMot2
- `make fetch-mormot2` — fetch mORMot2 and QuickJS
- `make clean` — remove build artifacts
- `make dist` — create tarball
- `make run` — build and run
- `make install` — install binary (needs `sudo`)

**Variables:** `LAZBUILD`, `WIDGETSET`, `BUILD_MODE`, `IGNORE_MORMOT`

See the Makefile or README for more details.

## Running Tests

- Use the Makefile for test runs:
  - `make test` — builds and runs tests (starts PHP test server if available)
  - `make test-nophp` — skips PHP-backed tests (for CI)
- On macOS, use `gmake`.
- On Windows, use `make.ps1` (auto-detects PHP or set `TRNDI_PHP_EXECUTABLE`).
- Console tests print a message if PHP is missing.
- Run console tests manually:
  ```bash
  lazbuild tests/TrndiTestConsole.lpi
  ./tests/TrndiTestConsole
  ```
  To enable integration tests, set `TRNDI_TEST_SERVER_URL` (e.g. `export TRNDI_TEST_SERVER_URL=http://localhost:8080`).
- GUI tests: run with `TrndiTest` or use `xvfb-run -a ./tests/TrndiTest` for headless environments.

## Generating Documentation Locally

Run from the repo root:

```bash
# Native units
pasdoc --output=doc/pasdoc-all --name "Trndi Native (All)" \
  units/trndi/trndi.native.base.pp \
  units/trndi/trndi.native.win.pp \
  units/trndi/trndi.native.mac.pp \
  units/trndi/trndi.native.linux.pp \
  units/trndi/trndi.native.pp

# Extensions
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

---

## Helpful Links

- [guides/API.md](guides/API.md) — API overview and expectations
- [README.md](README.md) — Project overview and build instructions
- [doc/](doc/) — Documentation output

---

If you have questions, open an issue or ask in discussions. Happy coding!

## Submitting a pull request

- Make sure your branch builds (or note any environment-only blockers).
- Include concise, descriptive commit messages.
- Reference issues (e.g., "Fixes #123") when applicable.
- Be ready to iterate based on review feedback.

Thanks for helping make Trndi better!