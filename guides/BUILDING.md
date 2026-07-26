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

`make`/`gmake` copy the executable *and* the QuickJS libraries into `build/`, but Lazarus itself builds and runs `Trndi` in the repo root — where those libraries are not. Extensions builds started from the IDE (Run > Run, F9) therefore fail to load the engine. Copy the pair for your platform to the repo root once; they are gitignored there:

```sh
make qjs-links                                          # Linux only: recreate the .so symlinks
cp -P externals/quickjs/prebuilt/x86_64-linux/* .       # or aarch64-linux, aarch64-darwin, …
```

`.\make.ps1` already copies the DLLs to the repo root on Windows, so the IDE works there without an extra step.

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

### Docker
`dist/docker/Dockerfile` builds a Linux dev container that mirrors CI's Linux jobs (Lazarus/FPC + Qt6 from `.github/actions/setup-lazarus`), on amd64 and arm64 alike. On `docker run` its entrypoint clones (or updates) the `develop` branch, builds it via `make release` — Extensions on both architectures, linking the committed QuickJS libraries — then drops you into a shell in the checkout. Set `TRNDI_BUILD_TARGET=noext-release` to build without the engine:
```
docker build -t trndi-dev -f dist/docker/Dockerfile .
docker run -it --rm trndi-dev
```