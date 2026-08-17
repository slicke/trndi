# VS Code (Lazarus/FPC)

This repo is a Lazarus / Free Pascal project. VS Code works well for editing, but
code completion for your own units, go-to-definition and symbol lookup need a
Pascal language server.

## Recommended extensions

```vscode-extensions
coolchyni.fpctoolkit
coolchyni.beyond-debug
```

`FreePascal Toolkit` bundles a `pasls` language server (CodeTools based), parses
`Trndi.lpi` including its build modes, drives `lazbuild`, and formats through
`jcf-cli`. `GDB Debugger - Beyond` is only needed if you want to debug from the
editor.

Earlier versions of this document recommended `wosi.omnipascal`. It still works,
but it has had no release since 2022 and does not understand Lazarus build
modes, so everything behind `{$IFDEF TrndiExt}` reads as dead code.

## Setup

`.vscode/` is in `.gitignore`, so nothing is shipped with the repo and each
contributor configures this locally. Create `.vscode/settings.json`:

```jsonc
{
  // FPCDIR is the FPC *source* tree - the directory containing rtl/ and
  // packages/ - not the compiled units. Many distros version it, so the
  // unversioned parent is one level too high and gives an empty index.
  "fpctoolkit.env.FPCDIR": "/usr/share/fpcsrc/3.2.2",
  "fpctoolkit.env.PP": "/usr/bin/fpc",
  "fpctoolkit.env.LAZARUSDIR": "/usr/share/lazarus",
  "fpctoolkit.env.FPCTARGET": "linux",
  "fpctoolkit.env.FPCTARGETCPU": "x86_64",

  "fpctoolkit.lsp.initializationOptions.program": "/path/to/trndi/Trndi.lpr",

  "fpctoolkit.format.cfgpath": "/path/to/trndi/JCFSettings.xml",
  "fpctoolkit.format.tabsize": 2,

  "[objectpascal]": {
    "editor.defaultFormatter": "coolchyni.fpctoolkit",
    "editor.insertSpaces": true,
    "editor.tabSize": 2,
    "editor.detectIndentation": false
  }
}
```

Run `Developer: Reload Window` after changing these.

Pointing `fpctoolkit.format.cfgpath` at the repo's own `JCFSettings.xml` makes
the editor format to the project's JEDI Code Formatter rules rather than the
extension's bundled defaults.

### LCL search paths

`Trndi.lpi` lists the project's own unit directories, but LCL and its packages
come from the LCL package rather than from `OtherUnitFiles`, so the language
server needs them spelled out:

```jsonc
"fpctoolkit.searchPath": [
  "/usr/share/lazarus/lcl",
  "/usr/share/lazarus/lcl/forms",
  "/usr/share/lazarus/lcl/widgetset",
  "/usr/share/lazarus/lcl/nonwin32",
  "/usr/share/lazarus/lcl/include",
  "/usr/share/lazarus/lcl/interfaces/qt6",
  "/usr/share/lazarus/components/lazutils",
  "/usr/share/lazarus/components/lazcontrols",
  "/usr/share/lazarus/components/turbopower_ipro",
  "/usr/share/lazarus/packager/registration"
]
```

List only the widgetset you build with. Adding `lcl/interfaces` as a whole pulls
in gtk2, gtk3, win32 and cocoa at once and every widgetset unit ends up with a
duplicate name.

## Build modes and conditional defines

Much of the codebase is guarded by compiler defines. FreePascal Toolkit reads
the build modes straight out of `Trndi.lpi`, so select the mode you are working
in from the status bar and the defines follow; there is no separate define list
to maintain.

For reference, from `Trndi.lpi`:
- Extensions debug: `-dTrndiExt -dDEBUG`
- Extensions release: `-dTrndiExt`
- No extensions debug: `-dDEBUG`

Platform defines (`X_WIN`, `X_MAC`, `X_HAIKU`, `X_PC`, …) are derived in
`inc/native.inc` from the target, not set per build mode.

## Toolchain paths (Linux)

Verified on Fedora with FPC 3.2.2 and Lazarus 4.8:

- `lazbuild`: `/usr/bin/lazbuild`
- `fpc`: `/usr/bin/fpc`
- `ppcx64`: `/usr/bin/ppcx64`
- FPC sources: `/usr/share/fpcsrc/<version>`
- Lazarus sources: `/usr/share/lazarus`

Other distributions lay these out differently. The FPC source directory is
whichever one contains `rtl/` and `packages/`; `fpc -iV` gives the version
number the path is usually keyed on.

## Building from VS Code

FreePascal Toolkit can build the selected `Trndi.lpi` build mode directly via
`lazbuild`.

The repository's own build entry points are `make` on Linux/BSD/Haiku, `gmake`
on macOS and `make.ps1` on Windows — see `CLAUDE.md` for the target list
(`make`, `make debug`, `make test`, `make noext`). Wiring those into
`.vscode/tasks.json` is left to individual contributors, since `.vscode/` is not
tracked.
