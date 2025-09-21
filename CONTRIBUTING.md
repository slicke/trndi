# Contributing to Trndi

Thanks for your interest in contributing! This guide explains how to propose changes, coding/documentation style, and how to run docs/build locally.

## Getting started

- Fork the repo and create a feature branch from `develop`.
- For Lazarus/FPC projects, keep edits minimal and focused. Prefer small PRs.
- If your change affects public behavior or API, update docs and add a brief test/example when practical.

## Code style

- Language: Free Pascal/Lazarus. Keep cross-platform code in platform units under `units/trndi/trndi.native.*.pp`.
- Avoid platform `{$IFDEF}` blocks in the base unit (`trndi.native.base`); implement platform specifics in `trndi.native.win|linux|mac`.
- Keep methods small and intention-revealing; add a short comment explaining why when non-obvious.
- Prefer pure Pascal for helpers over shelling out unless a native tool is the correct choice.

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