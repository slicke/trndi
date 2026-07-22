# QuickJS binding for Trndi

Trndi's JavaScript extension engine binds [quickjs-ng](https://github.com/quickjs-ng/quickjs)
directly, replacing the QuickJS binding previously used from mORMot2.

Two libraries are involved:

| library | contents |
|---|---|
| `libqjs` | quickjs-ng, completely unmodified |
| `tqshim` | the ABI shim in `tq_shim.c` |

Keeping the engine unpatched means upgrading quickjs-ng is a rebuild, not a re-patch.

## Why a shim is needed

quickjs-ng's `JSValue` is a 16-byte struct on 64-bit targets. Passing or returning
it by value across an FPC/C `cdecl` boundary depends on per-platform struct
classification rules that FPC and GCC are not guaranteed to agree on — SysV splits
it across two registers, Win64 returns it through a hidden pointer.

`tq_shim.c` moves every `JSValue` across the boundary **by pointer**:

- a `JSValue` argument becomes `const JSValue *`
- a `JSValue` result becomes a leading `JSValue *out` parameter

Callbacks run the same hazard in reverse — QuickJS calls a function that must
*return* a `JSValue` struct. No Pascal function does that here. A single C
trampoline receives every JS call and forwards it to one Pascal dispatch routine,
using QuickJS's `magic` integer as the index into Trndi's native registry.

The Pascal side is `units/trndi/ext/trndi.ext.quickjs.pp`. Note the binding rule
documented there: every `JSValue` parameter must be an explicit pointer type.
Writing `const v: JSValue` reintroduces exactly the by-value hazard the shim
removes, and fails at runtime rather than at compile time.

Do **not** build quickjs-ng with `JS_NAN_BOXING` on a 64-bit target. Its NaN
boxing packs the pointer into the low 32 bits and is 32-bit-only.

## Rebuilding

`build.sh` builds both libraries. It needs `cmake`, `ninja`, a host `gcc`, and —
for the Windows cross-build — mingw-w64.

```sh
# Fedora
sudo dnf install -y cmake ninja-build gcc mingw64-gcc mingw64-winpthreads-static
# Debian/Ubuntu
sudo apt install -y cmake ninja-build gcc gcc-mingw-w64-x86-64

./build.sh            # host .so + win64 .dll
./build.sh linux      # host only
./build.sh win        # win64 only
```

Output lands in `prebuilt/<arch>/`.

## Platform coverage

| target | how |
|---|---|
| `x86_64-linux` | `build.sh linux`, or cross from any host |
| `x86_64-win64` | `build.sh win` (mingw cross), or natively with mingw |
| `aarch64-linux` | build natively on the target (e.g. a Raspberry Pi) |
| macOS, Windows ARM64 | build natively on the platform |

There is no cross-glibc in Fedora's repos, so arm64 Linux is built natively
rather than cross-compiled.

## A note on `libqjs.so` symlinks

The Linux build produces `libqjs.so.0.15.1` with `SONAME libqjs.so.0`, plus
`libqjs.so` and `libqjs.so.0` symlinks. Only the real file is stored here —
symlinks do not survive a copy onto a Windows filesystem. `build.sh` recreates
them, and so does the Makefile's install step.
