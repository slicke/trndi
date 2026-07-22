## Dependencies

### JavaScript engine (Extensions builds only)
> Ignore this section if you build with a "No Ext" mode — those compile without `TrndiExt` and link nothing below.

Trndi embeds __QuickJS__ (the [quickjs-ng](https://github.com/quickjs-ng/quickjs) fork) through its own binding, `units/trndi/ext/trndi.ext.quickjs.pp`. There is nothing to install: the engine and a small ABI shim are committed as shared libraries under `externals/quickjs/prebuilt/<cpu>-<os>/` and the build copies them next to the executable.

`make` / `.\make.ps1` handle this for you. Two details matter if you build by hand:

- On Linux the linker resolves `-lqjs` through an unversioned `libqjs.so` symlink. Symlinks are not tracked in git (a checkout onto NTFS flattens them into empty files), so `make` recreates them before calling lazbuild — run `make qjs-links` if you invoke `lazbuild` directly.
- The binaries must sit beside `Trndi`/`Trndi.exe` at runtime. Linux builds carry an `$ORIGIN` runpath and macOS an `@loader_path` one; Windows resolves DLLs from the executable's directory. In a macOS `.app` the relevant directory is `Contents/MacOS`, which is where `dist/macos.sh` puts them.

Prebuilt libraries currently ship for `x86_64-linux` and `x86_64-win64`. On any other target, build them with `externals/quickjs/build.sh` (needs a C toolchain and CMake) or use a "No Ext" build mode.

macOS is the common case here — its libraries cannot be cross-built from Linux, so they are not committed. Build them once on the Mac and the Extensions modes work from then on:

```sh
externals/quickjs/build.sh mac    # -> externals/quickjs/prebuilt/aarch64-darwin/
gmake                             # or gmake test
``` See [externals/quickjs/README.md](/externals/quickjs/README.md) for how the shim works and why it exists.

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
`dist/docker/Dockerfile` builds a Linux dev container that mirrors CI's linux-amd64 job (Lazarus/FPC + Qt6 from `.github/actions/setup-lazarus`). On `docker run` its entrypoint clones (or updates) the `develop` branch, builds it via `make release`, then drops you into a shell in the checkout:
```
docker build -t trndi-dev -f dist/docker/Dockerfile .
docker run -it --rm trndi-dev
```