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

`build.sh` builds both libraries. It needs `cmake`, a C compiler, and — for the
Windows cross-build — mingw-w64. `ninja` is used when present.

```sh
# Fedora
sudo dnf install -y cmake ninja-build gcc mingw64-gcc mingw64-winpthreads-static
# Debian/Ubuntu
sudo apt install -y cmake ninja-build gcc gcc-mingw-w64-x86-64
# macOS (the compiler comes from the Xcode command line tools)
brew install cmake ninja

./build.sh            # everything this host can produce
./build.sh linux      # host .so
./build.sh mac        # host .dylib
./build.sh win        # win64 .dll (mingw cross)
```

Output lands in `prebuilt/<cpu>-<os>/`, named the way FPC names targets — so
Apple Silicon is `aarch64-darwin`, not `arm64-darwin`.

Two macOS knobs, both optional:

```sh
TRNDI_QJS_MACOS_MIN=11.0 ./build.sh mac                 # raise the minimum OS
TRNDI_QJS_MAC_ARCHS='arm64;x86_64' ./build.sh mac       # universal libraries
```

A universal build still lands in the host's directory; copy it to the other
target's directory as well if you want both to link against it.

## Platform coverage

| target | how |
|---|---|
| `x86_64-linux` | `build.sh linux`, or cross from any host |
| `x86_64-win64` | `build.sh win` (mingw cross), or natively with mingw |
| `aarch64-darwin`, `x86_64-darwin` | `build.sh mac`, on a Mac |
| `aarch64-linux` | build natively on the target (e.g. a Raspberry Pi) |
| Windows ARM64 | build natively on the platform |

`x86_64-linux`, `aarch64-linux`, `x86_64-win64` and `aarch64-darwin` are
committed. The two missing ones — `x86_64-darwin` (Intel Mac) and Windows ARM64
— have to be built on the target itself; until they are, those hosts can only
build Trndi's "No Ext" modes. Anything `build.sh` produces is safe to commit —
that is the point of `prebuilt/`.

There is no cross-glibc in Fedora's repos, so arm64 Linux is built natively
rather than cross-compiled.

### glibc floor

A Linux library only runs on glibc at least as new as the one it was linked
against, so the build host — not the code — decides which distros can load it.
Check any of them with:

```sh
readelf -V libqjs.so.0.15.1 | grep -o 'GLIBC_[0-9.]*' | sort -uV | tail -1
```

| target | built on | floor |
|---|---|---|
| `aarch64-linux` | Rocky 9 container (podman, on a Pi) | `GLIBC_2.34` |
| `x86_64-linux` | Rocky 9 container (podman, on Fedora/WSL) | `GLIBC_2.34` |

`GLIBC_2.34` is the number to hold: it matches the floor the `Trndi` binary
itself records, so the engine is not what limits distro support. An earlier
Fedora-built set floored at 2.38, which excluded Debian 12, Raspberry Pi OS
bookworm, Ubuntu 22.04 and RHEL/Rocky 9 — all otherwise fine — and because the
binding is link-time on Linux (`external QJSLIB`), `libqjs.so.0` is a `DT_NEEDED`
entry the loader resolves before `main` runs. So the floor is not a limit on
extensions, it is a limit on the whole program: an Extensions build below it
fails to *launch* — no window, just a loader error — rather than starting up
without extensions. "No Ext" builds link no engine and are unaffected by any of
this, which is the fallback for a system that cannot meet the floor and cannot
rebuild.

Nothing in quickjs-ng needs 2.38; that floor was an artifact of Fedora's libc.
Building the same source in a Rocky 9 container gives 2.34 on any host, without
a VM — the sysroot supplies glibc, the host kernel is irrelevant, and
old-userspace-on-new-kernel is the safe direction:

```sh
cd <repo root>
podman run --rm \
  -v "$PWD:/src" -w /src/externals/quickjs \
  -e TRNDI_QJS_WORK=/tmp/qjsbuild \
  quay.io/rockylinux/rockylinux:9 \
  sh -c 'dnf install -y gcc cmake git && ./build.sh linux'
```

`TRNDI_QJS_WORK` keeps the quickjs-ng clone and the CMake tree inside the
container, so only the finished libraries are written back through the bind
mount — worth it on any slow mount, and a large difference under WSL. The image
is named in full because Fedora's podman defaults to
`short-name-mode = "enforcing"`, where a bare `rockylinux:9` prompts or fails.
On an SELinux host (native Fedora/RHEL) add `:Z` to the volume; do *not* on
WSL, where `/mnt/c` is drvfs and has no xattrs to relabel.

Run it once per architecture, on a machine of that architecture (`build.sh
linux` only ever builds for its host). Use it whenever these binaries are
refreshed — rebuilding on a bleeding-edge host silently raises the floor for
everyone, and nothing in CI catches it.

Rootless podman under WSL usually needs one fix first: the distro's rootfs
import strips file capabilities from `newuidmap`/`newgidmap`, and without them
UID mapping fails with `should have setuid or have filecaps setuid`. Restore
them with `sudo setcap cap_setuid+ep /usr/bin/newuidmap` and
`sudo setcap cap_setgid+ep /usr/bin/newgidmap`, then `podman system migrate`.

## How the libraries are found at runtime

They ship *beside* the executable rather than being installed system-wide, so
each platform needs the loader pointed at the executable's own directory:

| platform | mechanism |
|---|---|
| Windows | automatic — the executable's directory is searched first |
| Linux | `-k-rpath=$ORIGIN`, set per build mode in `Trndi.lpi` |
| macOS | `-k-rpath -k@loader_path`, same place |

In a macOS `.app`, that directory is `Contents/MacOS`, which is where
`dist/macos.sh` puts them.

## A note on `libqjs.so` symlinks

The Linux build produces `libqjs.so.0.15.1` with `SONAME libqjs.so.0`, plus
`libqjs.so` and `libqjs.so.0` symlinks. Only the real file is stored here —
symlinks do not survive a copy onto a Windows filesystem. `build.sh` recreates
them, and so does the Makefile's install step.

The symlinks are a *build-tree* convenience only: `libqjs.so` exists so `-lqjs`
resolves at link time, and `libqjs.so.0` is the `SONAME` the loader asks for.
Linux packages never carry them — `dist/linux/stage-qjs.sh` installs the real
file under its `SONAME` instead, because 7-Zip 21.07+, fpm and mksquashfs all
dereference symlinks by default and would otherwise put three identical copies
of the library into the ZIP, DEB, RPM and AppImage.

macOS sidesteps this: `build.sh mac` flattens the versioned dylib into a single
unversioned `libqjs.dylib` and rewrites its install name to match, so nothing
here depends on a symlink. Versioning would buy nothing — FPC links the library
by name and the copy beside the executable is the only one it ever loads.
