## Dependencies

### JavaScript engine (Extensions builds only)
> Ignore this section if you build with a "No Ext" mode — those compile without `TrndiExt` and link nothing below.

Trndi embeds __QuickJS__ (the [quickjs-ng](https://github.com/quickjs-ng/quickjs) fork) through its own binding, `units/trndi/ext/trndi.ext.quickjs.pp`. There is nothing to install: the engine and a small ABI shim are committed as shared libraries under `externals/quickjs/prebuilt/<cpu>-<os>/` and the build copies them next to the executable.

`make` / `.\make.ps1` handle this for you. Two details matter if you build by hand:

- On Linux the linker resolves `-lqjs` through an unversioned `libqjs.so` symlink. Symlinks are not tracked in git (a checkout onto NTFS flattens them into empty files), so `make` recreates them before calling lazbuild — run `make qjs-links` if you invoke `lazbuild` directly.
- The binaries must sit beside `Trndi`/`Trndi.exe` at runtime. Linux builds carry an `$ORIGIN` runpath and macOS an `@loader_path` one; Windows resolves DLLs from the executable's directory. In a macOS `.app` the relevant directory is `Contents/MacOS`, which is where `dist/macos.sh` puts them.

Prebuilt libraries currently ship for `x86_64-linux`, `aarch64-linux`, `x86_64-win64` and `aarch64-darwin` (Apple Silicon) — the four targets CI builds Extensions modes for. On any other target (Intel macOS, Windows on ARM), build them with `externals/quickjs/build.sh` (needs a C toolchain and CMake) or use a "No Ext" build mode:

```sh
externals/quickjs/build.sh mac    # -> externals/quickjs/prebuilt/<cpu>-darwin/
gmake                             # or gmake test
``` See [externals/quickjs/README.md](/externals/quickjs/README.md) for how the shim works and why it exists.

The same applies to a host that is old rather than exotic. Because the binding is link-time on Linux, `libqjs.so.0` is a `DT_NEEDED` entry resolved before `main` runs — so if the committed library's glibc floor is above the running system's, an Extensions build does not start at all (no window, just a loader error), rather than starting without extensions. A "No Ext" build links no engine and is bound only by the `Trndi` binary's own floor. Both Linux targets currently floor at `GLIBC_2.34`, the same value that binary records, so the engine does not narrow distro support; verify with:

```sh
readelf -V externals/quickjs/prebuilt/x86_64-linux/libqjs.so.0.15.1 | grep -o 'GLIBC_[0-9.]*' | sort -uV | tail -1
```

That covers everything back to Debian 12 and Raspberry Pi OS bookworm (2.36), Ubuntu 22.04 (2.35) and RHEL/Rocky 9 (2.34). Should you ever face a system below the floor, note that no amount of `apt`/`dnf` updating helps — glibc never moves within a distro release — so rebuild locally instead:

```sh
externals/quickjs/build.sh linux
```

See [externals/quickjs/README.md](/externals/quickjs/README.md#glibc-floor) for how the committed libraries are built against an old glibc without needing an old machine.

#### Running from the Lazarus IDE

`make`/`gmake`/`.\make.ps1` copy the executable *and* the QuickJS libraries into `build/`, but Lazarus itself builds and runs `Trndi` in the repo root — where those libraries are not. Extensions builds started from the IDE (Run > Run, F9) therefore fail to load the engine. The `ide-libs` target puts the pair for your platform there (picking the right `prebuilt/<cpu>-<os>/` directory and recreating the `.so` symlinks on Linux); run it once per checkout, and again whenever you refresh `externals/quickjs/prebuilt/`:

```sh
make ide-libs        # gmake ide-libs on macOS, .\make.ps1 ide-libs on Windows
```

It does not build anything and does not care which build mode you use, so it costs nothing to re-run. The copies are gitignored in the repo root and `make clean` removes them. On Windows the build targets already copy the DLLs there, so `.\make.ps1 ide-libs` is only needed if you have never run a build.

### Qt6
You need __libqt6pas__, and its development packages. These are normally available with your distro. See the _Linux section in [README.md](/README.md)_ on how to install libqt6pas.

## Code Style
### Linter
Trndi uses __JEDI__ to format code, in Lazarus: Source > JEDI Code Format > Global Settings. Choose _JCFSettings.xml_ from this repo.

### Naming
Units should end with ```.pp```, 

### VSCode
Should you choose not to use Lazarus IDE, these are the recommendations for Visual Studio Code:
* Language Support: https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal

* Formatting: Name: Pascal Formatter
VS Marketplace Link: https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal-formatter

* Debugging: https://marketplace.visualstudio.com/items?itemName=CNOC.fpdebug
* Debugging with GDB: https://marketplace.visualstudio.com/items?itemName=coolchyni.beyond-debug

### Ubuntu notice
If your Ubuntu installation complains about -lgcc, consider making a symlink:
```sudo ln -s /usr/lib/gcc/x86_64-linux-gnu/11/libgcc.a /usr/lib/libgcc.a```

### Podman / Docker
`dist/docker/Dockerfile` builds a Linux dev container that mirrors CI's Linux jobs (Lazarus/FPC + Qt6 from `.github/actions/setup-lazarus`), on amd64 and arm64 alike. At run time its entrypoint clones (or updates) the `develop` branch, builds it via `make release` — Extensions on both architectures, linking the committed QuickJS libraries — then drops you into a shell in the checkout. Set `TRNDI_BUILD_TARGET=noext-release` to build without the engine.

The file uses no BuildKit-only syntax, so the same commands work under either engine — replace `podman` with `docker` if that is what you have:
```
podman build -t trndi-dev -f dist/docker/Dockerfile .
podman run -it --rm trndi-dev
```
Keep the checkout (and its build artifacts) between runs with a named volume, so repeat runs only fetch changed sources:
```
podman run -it --rm -v trndi-checkout:/root/trndi trndi-dev
```

Podman notes:
* On Windows and macOS podman runs the containers inside a VM, which must be up first: `podman machine start`. The default machine gets 2 GiB of RAM; if `lazbuild` is killed mid-compile, give it more — `podman machine stop && podman machine set --memory 4096 && podman machine start`.
* To compile your working tree instead of a fresh clone, mount it over `$TRNDI_DIR`: `podman run -it --rm -v .:/root/trndi:Z trndi-dev`. The `:Z` is needed on SELinux hosts (Fedora, RHEL) and harmless elsewhere; add `--userns=keep-id` to keep the produced files owned by your user rather than by root.
* Rootless podman is enough — nothing in the image needs `--privileged` or host devices.

### WSL (Windows)
WSL2 gives a Windows box a real Linux build without a VM or a container, which makes it a convenient place to check the Qt6 and GTK3 paths. The recipe below is deliberately the *edge* one: it builds FPC and Lazarus from trunk, so you can test against compiler and LCL changes long before any distro ships them. For a build that mirrors CI, use the container above or your distro's Lazarus packages instead.

```powershell
wsl --install FedoraLinux-44      # wsl --list --online for other distros
```

Inside the distro, install a toolchain and the Trndi dependencies (Fedora shown; substitute `apt`/`zypper` elsewhere):

```sh
sudo dnf install -y gcc binutils make git qt6pas gtk3-devel libX11-devel gdb
```

`qt6pas` is the same libqt6pas the Qt6 section above asks for. WSLg forwards the GUI, so the Lazarus IDE and Trndi itself both run with no X server to configure.

Build the trunk toolchain with **fpclazup** — not `fpcup`, which installs FPC alone and has no Lazarus support — from the [Reiniero-fpcup releases](https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/releases):

```sh
wget https://github.com/LongDirtyAnimAlf/Reiniero-fpcup/releases/latest/download/fpclazup-x86_64-linux
chmod +x fpclazup-x86_64-linux
./fpclazup-x86_64-linux \
  --installdir="$HOME/fpcupdeluxe" \
  --fpcURL=https://gitlab.com/freepascal.org/fpc/source.git --fpcBranch=main \
  --lazVersion=trunk \
  --fpcOPT=-k-znotext --disablejobs --noconfirm
```

Then rebuild the IDE against Qt6, matching the widgetset Trndi's Linux modes target:

```sh
cd "$HOME/fpcupdeluxe/lazarus"
./lazbuild --pcp="$HOME/fpcupdeluxe/config_lazarus" --build-ide= --ws=qt6 --max-process-count=2
```

WSL notes:
* Pass the FPC source as `--fpcURL` plus `--fpcBranch`. A `--fpcVersion=3.2.2` shorthand can resolve to an empty URL and fail the checkout with nothing but `Checkout/update of FPC sources failure` to go on, even when the equivalent `--lazVersion` shorthand works.
* `--fpcOPT=-k-znotext` is required from Fedora 44 / binutils 2.46 onward. Without it FPC compiles to 96% and then fails linking `libpas2jslib.so` with `read-only segment has dynamic relocations`: the RTL is not PIC, and newer `ld` refuses that inside a shared library. This is a distro hardening default rather than an FPC-version problem, so an older FPC hits it just the same.
* `--disablejobs` and `--max-process-count=2` matter for the same reason the podman note above mentions RAM. Both tools default to one compiler process per core, so a 16-core host against the `[wsl2] memory=6GB` in `%USERPROFILE%\.wslconfig` gets its VM OOM-killed mid-compile — which drops every shell in that distro and loses recently written files, not just the build. Raise the cap or lower the parallelism; on a memory-tight host, lower the parallelism.
* Trndi's own targets expect FPC 3.2.2, so treat a failure that only appears under a trunk toolchain as suspect until you reproduce it on 3.2.2.