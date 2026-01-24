# VS Code (Lazarus/FPC)

This repo is a Lazarus / Free Pascal project. VS Code can work well for editing, but for reliable code completion (your own units/functions, go-to-definition, etc.) you should use OmniPascal.

## Recommended extension

```vscode-extensions
wosi.omnipascal
```

## Quick setup

1. Open the repository folder in VS Code.
2. Run `OmniPascal: Load project` and select `Trndi.lpi`.
3. If you change OmniPascal settings, run `Developer: Reload Window`.

When the project is loaded, OmniPascal reads Lazarus project settings (notably unit search paths) from `Trndi.lpi`. This is what makes completion/navigation work for units under `units/`.

## Build modes and conditional defines

Some code is guarded by compiler defines. If OmniPascal is not resolving symbols that are only present in a specific build mode, configure the same defines you build with.

From `Trndi.lpi`:
- Extensions debug: `-dTrndiExt -dDEBUG`
- Extensions release: `-dTrndiExt`
- No extensions debug: `-dDEBUG`

## Toolchain paths (Linux)

If OmniPascal asks for tool locations, these are typical on Linux installs:
- `lazbuild`: `/usr/bin/lazbuild`
- `fpc`: `/usr/bin/fpc`
- `ppcx64`: `/usr/bin/ppcx64`

## Building from VS Code

This repository includes VS Code tasks in `.vscode/tasks.json`.
Use `Terminal -> Run Task...` and pick one of the Lazarus build tasks (Qt6). These tasks use `lazbuild` and are independent of OmniPascal, but having OmniPascal configured improves editing (completion, navigation, symbol lookup).
