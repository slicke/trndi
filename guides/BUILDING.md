## Dependencies

### mORMot2
Trndi depends on _mORMot2_ for the JavaScript engine (nothing else from the library is used). Install it via the _Makefile helpers_ (see below), which fetch the last tested commit from [GitHub](https://github.com/synopse/mORMot2) and sets it up for Lazarus — the __Online Package Manager__ in Lazarus is outdated and no longer recommended.

**Quick start:** run `make bootstrap` (Windows: `.\make.ps1 bootstrap`). It fetches mORMot2 and the QuickJS static* libraries into `externals/mORMot2`, compiles `mormot2.lpk` so lazbuild can resolve it, then verifies everything is in place. Skip it if `externals/mORMot2` already exists, or if you already have a working mORMot2 install (e.g. via OPM) you don't want to replace.
> * statics are library files for eg quickjs (quickjs.o / quickjs.dll)

For finer control, `make bootstrap` is just these three targets in order (each also available on its own; `make.ps1` mirrors all of them):

- `fetch-mormot2` — clones the pinned commit into `externals/mORMot2` and downloads the QuickJS static archive into `externals/mORMot2/static`. Requires `git`, `7z`, and `curl`/`wget` — the target checks for these upfront and prints the right install command for your OS if any are missing. Run `make help` for the `FAIL_7ZIP` escape hatch (skips the static archive instead of aborting).
- `install-mormot2` — compiles `mormot2.lpk` with lazbuild so it can resolve the `mormot2` dependency. Run once after `fetch-mormot2`.
- `check-mormot2` — verifies mORMot2 and the QuickJS statics are present (registered `.lpk`, OPM install, `externals/mORMot2`, or `./static`) and exits non-zero with guidance if not.

Policy: `externals/mORMot2` and `./static` are gitignored to avoid committing large binaries. To track mORMot2 as a submodule instead, run `git submodule add <repo> externals/mORMot2` and remove it from `.gitignore`. DO NOT do this in a pull request!

### JavaScript (if built with Extensions on)
> Ignore this part if building with the "No Ext" mode

The JS engine, __QuickJS__, is linked into Trndi statically. `make fetch-mormot2` downloads the static libraries automatically into `externals/mORMot2/static`; if you instead have an existing OPM install, they'll be under `~/.lazarus/onlinepackagemanager/packages/mORMot2/static` (Windows: `C:\Users\<you>\AppData\Local\Lazarus\onlinepackagemanager\packages\mORMot2\static`).

To fetch the static libraries manually, get https://synopse.info/files/mormot2static.7z and keep just the QuickJS files (check the linker output to see which ones you need).

### Building without extensions
When building without extensions, you need to install the mORMot2 library - or __remove it__ from the _Project Inspector_. Trndi will build fine without changing the project, if mORMot2 is available.

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
`dist/docker/Dockerfile` builds a Linux dev container that mirrors CI's linux-amd64 job (Lazarus/FPC + Qt6 from `.github/actions/setup-lazarus`). On `docker run` its entrypoint clones (or updates) the `develop` branch, builds it via `make bootstrap && make release`, then drops you into a shell in the checkout:
```
docker build -t trndi-dev -f dist/docker/Dockerfile .
docker run -it --rm trndi-dev
```