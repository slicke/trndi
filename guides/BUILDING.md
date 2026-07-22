## Dependencies

### mORMot2
Trndi depends on mORMot2 for the JavaScript engine, it does not use anything else from the library at this time. It can be installed via the __Online Package Manager__ in Lazarus, or [GitHub](https://github.com/synopse/mORMot2).

You can use the Makefile helpers to manage mORMot2 resources locally (on Windows, `make.ps1` provides the same `fetch-mormot2`, `install-mormot2` and `check-mormot2` targets):

- `make fetch-mormot2` — clones mORMot2 into `externals/mORMot2` (shallow clone of branch `2.3.stable` by default) and downloads and extracts the recommended static archive (`mormot2static.7z`) into `externals/mORMot2/static` — the path `mormot2.lpk`'s own `LibraryPath`/`ObjectPath` (relative to its own directory) actually searches once `install-mormot2` registers the package. Requires `git`, `7z` (e.g. `sudo apt install p7zip-full` on Ubuntu <= 22.04, `sudo apt install 7zip` on Ubuntu >= 23.10 where p7zip was dropped, `sudo dnf install p7zip p7zip-plugins` on Fedora/RHEL, or `brew install p7zip` on macOS), and `curl` or `wget`; the target checks for these upfront and fails before cloning anything if they're missing. Set `FAIL_7ZIP=1` to bypass that check and clone anyway — the static-archive step is then skipped with a warning instead of aborting the target.
- `make install-mormot2` — compiles `externals/mORMot2/packages/lazarus/mormot2.lpk` with lazbuild, which registers the package so a later `lazbuild Trndi.lpi` (or plain `make`) can resolve the `mormot2` dependency. Run it once after `fetch-mormot2`.
- `make check-mormot2` — verifies that mORMot2 is present (a `mormot2.lpk` registered in `~/.lazarus/packagefiles.xml`, an OPM install, `externals/mORMot2` or `./static`) and that QuickJS static artifacts are available; it exits non-zero with guidance if something is missing.
- `make bootstrap` — one-shot opt-in setup: runs `fetch-mormot2` only if `externals/mORMot2` isn't already cloned, then `install-mormot2`, then `check-mormot2` to confirm. Not part of `all`/`build`/`release` — those still fail fast with guidance, since fetching hits the network and `fetch-mormot2` refuses to touch an existing checkout.

Policy: The repository ignores `externals/mORMot2` and `./static` by default (see `.gitignore`) to avoid committing large binaries. If you prefer to track mORMot2 as a submodule for reproducible snapshots you may add it with `git submodule add <repo> externals/mORMot2` and remove `externals/mORMot2` from `.gitignore`.

### JavaScript (if built with Extensions on)
> Ignore this part if building with the "No Ext" mode

The JS engine, __QuickJS__, is _linked_ into Trndi statically.

To compile Trndi with extensions support you need the relevant QuickJS library files. 

* They are expected to be in ```../static``` when compiling from GitHub.
* When using the _Online Package Manager_ in Lazarus, they can be placed in  ```~/.lazarus/onlinepackagemanager/packages/mORMot2/static```, or on Windows: ```C:\Users\<you>\AppData\Local\Lazarus\onlinepackagemanager\packages\mORMot2\static```


You can get the recommended libraries from https://synopse.info/files/mormot2static.7z. You only need to keep the QuickJS files. You can check the linker's output to see which files that is.

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