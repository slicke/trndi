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
tracked; the macOS debugging section below has a complete example.

## Debugging on macOS

`GDB Debugger - Beyond` needs GDB, which does not run on Apple Silicon. Use
LLDB through `vadimcn.vscode-lldb` (CodeLLDB) instead. Build with `gmake debug`;
on macOS the Makefile emits DWARF 2, which LLDB reads, and breakpoints in the
`inc/*.inc` files resolve to the right lines.

Launch the executable inside the bundle directly, not through `open`, so the
debugger owns the process:

```jsonc
{
  "name": "Trndi (debug)",
  "type": "lldb",
  "request": "launch",
  "program": "${workspaceFolder}/build/Trndi.app/Contents/MacOS/Trndi",
  "args": ["--no-multi"],
  "cwd": "${workspaceFolder}/build",
  "preLaunchTask": "Trndi: build debug"
}
```

`preLaunchTask` names a task by its `label` in `.vscode/tasks.json`, so F5
builds first and only starts the debugger if the build succeeds:

```jsonc
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Trndi: build debug",
      "type": "shell",
      "command": "gmake debug",
      "options": { "cwd": "${workspaceFolder}" },
      "group": { "kind": "build", "isDefault": true },
      "problemMatcher": {
        "owner": "fpc",
        "fileLocation": ["autoDetect", "${workspaceFolder}"],
        "pattern": {
          "regexp": "^(.+)\\((\\d+),(\\d+)\\)\\s+(Error|Fatal|Warning|Note|Hint):\\s+(.*)$",
          "file": 1, "line": 2, "column": 3, "severity": 4, "message": 5
        }
      }
    }
  ]
}
```

Marking it as the default build task also binds it to Cmd+Shift+B, and the
problem matcher turns FPC's `file(line,col) Error: …` output into entries in
the Problems panel.

If CodeLLDB reports `could not find 'debugserver'`, point it at Apple's copy
from the Command Line Tools (or Xcode) in `.vscode/settings.json`:

```jsonc
"lldb.adapterEnv": {
  "LLDB_DEBUGSERVER_PATH": "/Library/Developer/CommandLineTools/Library/PrivateFrameworks/LLDB.framework/Versions/A/Resources/debugserver"
}
```

CodeLLDB's "C++: on throw / on catch" filters in the Breakpoints view do not
apply: FPC does not use C++ exceptions. To stop where a Pascal exception is
raised, add a function breakpoint (`+` in the Breakpoints view) on
`FPC_RAISEEXCEPTION`, upper case as FPC exports it. LCL raises and handles
exceptions of its own, so keep it unchecked until you need it.

### Useful function breakpoints

The Debug build modes compile with range and overflow checks
(`-Cr -Co`), so these RTL entry points fire on the actual fault rather
than on the exception SysUtils later turns it into. They are quiet in normal
use and safe to leave enabled:

| Breakpoint | Stops when |
|---|---|
| `FPC_RANGEERROR` | an index or assignment is out of range |
| `FPC_OVERFLOW` | integer arithmetic overflows |
| `FPC_DIVBYZERO` | integer division by zero |
| `FPC_ABSTRACTERROR` | an abstract method is called |
| `FPC_ASSERT` | an `Assert(...)` fails |

Broader catches:

| Breakpoint | Stops when |
|---|---|
| `FPC_BREAK_ERROR` | any runtime error, including access violations |
| `FORMS$_$TAPPLICATION_$__$$_HANDLEEXCEPTION$TOBJECT` | an exception went unhandled and LCL is about to show its error dialog; the call stack shows where it came from |
| `FPC_RERAISE` | an `except ... raise;` block passes an exception on |
| `FPC_RAISEEXCEPTION` | any exception is raised (noisy, see above) |

Avoid the checkers themselves, which run on every call rather than only on
failure: `FPC_STACKCHECK` (every procedure entry, with `-Ct`), `FPC_IOCHECK`
(every I/O operation under `{$I+}`), `FPC_CHECK_OBJECT` (every method call)
and `FPC_DYNARRAY_RANGECHECK` (every dynamic-array access). Stack overflow
and I/O errors still reach `FPC_BREAK_ERROR`.

Function breakpoints are stored in VS Code's per-workspace state, not in
`.vscode/`, so they persist across sessions but are not shared through the
repository.
