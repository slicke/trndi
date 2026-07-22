# Makefile for Trndi — simple wrapper around lazbuild
# Usage examples:
#   make              -> release build (default)
#   make debug        -> debug build
#   make release      -> release build
#   make test         -> build and run tests (runner spawns an in-process Pascal test server)
#   make test-noserver-> build and run console tests, skipping the embedded test server (TRNDI_NO_TESTSERVER=1)
#   make clean        -> remove build artifacts
#   make install      -> install binary to /usr/local/bin (requires sudo)
#   make list-modes   -> list available build modes from Trndi.lpi

LAZBUILD ?= lazbuild

# On native Windows (when running Make under Windows, e.g., via cmd or MSYS),
# prefer an installed Lazarus at C:/lazarus/lazbuild.exe if it exists.
# This lets users run the Makefile from a Windows prompt without having
# lazbuild on PATH but installed in the standard Lazarus location.
ifeq ($(OS),Windows_NT)
  ifneq ($(wildcard C:/lazarus/lazbuild.exe),)
    LAZBUILD := C:/lazarus/lazbuild.exe
  endif
endif

LPI ?= Trndi.lpi
TEST_LPI ?= tests/TrndiTest.lpi
OUTDIR ?= build
WIDGETSET ?= qt6

# Compiled-in resource bundles (.lrs) and the tool that (re)generates them.
# The .lrs files are committed and compiled via {$I ...}, so a build never needs
# lazres — it is only used by the 'assets'/'check-assets' targets. Discover it on
# PATH, then in a system Lazarus install, then next to lazbuild (Windows layout).
# lazres lives in Lazarus' tools/ dir, a sibling of lazbuild. Resolve it from
# lazbuild's real path first (handles system + custom installs), then PATH, then
# common locations and the Windows layout next to lazbuild.exe.
LAZRES ?= $(shell \
  lb=$$(command -v $(LAZBUILD) 2>/dev/null); \
  if [ -n "$$lb" ] && [ -x "$$(dirname "$$(readlink -f "$$lb")")/tools/lazres" ]; then \
    echo "$$(dirname "$$(readlink -f "$$lb")")/tools/lazres"; \
  else \
    command -v lazres 2>/dev/null || ls /usr/lib/lazarus/*/tools/lazres /usr/share/lazarus/*/tools/lazres 2>/dev/null | head -n1 || ls "$(dir $(LAZBUILD))tools/lazres" "$(dir $(LAZBUILD))tools/lazres.exe" 2>/dev/null | head -n1; \
  fi)
# CareLink login helper: source files -> generated resource (in repo-root assets/).
CARELINK_ASSET_LRS  = assets/carelink_assets.lrs
CARELINK_ASSET_SRCS = tools/carelink-login/carelink-login.mjs tools/carelink-login/package.json tools/carelink-login/package-lock.json
# BUILD_MODE is a short hint (Release/Debug) for backward compatibility
# BUILD_MODE_NAME is the actual project build-mode name as seen in Trndi.lpi
BUILD_MODE ?= Release
BUILD_MODE_NAME = Extensions ($(BUILD_MODE))
CPU_FLAG ?=

# Optional binary stripping (primarily for Linux release builds).
# Lazarus/FPC build-modes sometimes embed debug info; stripping makes binaries much smaller.
STRIP ?= strip
STRIP_RELEASE ?=

# Auto-detect OS and pick appropriate default build-mode
ifeq ($(OS),Windows_NT)
  UNAME_S := Windows_NT
else
  UNAME_S := $(shell uname -s)
endif
IS_MINGW := $(findstring MINGW,$(UNAME_S))
IS_CYGWIN := $(findstring CYGWIN,$(UNAME_S))

# Linux: prefer Qt6 builds; when multiple Qt6 release modes exist prefer the Extensions variant
ifeq ($(UNAME_S),Linux)
  WIDGETSET ?= qt6
	# On Linux, prefer the generic project modes if they exist; --widgetset=qt6 controls the LCL backend.
	# This avoids having to maintain widgetset-specific modes just to get correct Release flags.
	STRIP_RELEASE ?= 1

# macOS: prefer native Release builds (Extensions); default to Cocoa widgetset
else ifeq ($(UNAME_S),Darwin)
  WIDGETSET ?= cocoa

# Windows (MSYS/Cygwin): prefer native Release builds (Extensions)
else ifneq ($(IS_MINGW),)

else ifneq ($(IS_CYGWIN),)

# Fallback: use qt6 if requested, otherwise No Ext
else
  ifeq ($(WIDGETSET),qt6)
  else
    ifeq ($(BUILD_MODE),Debug)
    else
    endif
  endif
endif

# Prebuilt QuickJS engine + ABI shim for this host. The directory name matches
# FPC's $(TargetCPU)-$(TargetOS), which is what the .lpi library path expands to
# (note that macOS reports arm64 where FPC says aarch64). QJS_LIBS is the glob
# used to copy the libraries next to a binary that has to load them.
ifeq ($(OS),Windows_NT)
  QJS_DIR := externals/quickjs/prebuilt/x86_64-win64
  QJS_LIBS := *.dll
else ifeq ($(UNAME_S),Darwin)
  QJS_DIR := externals/quickjs/prebuilt/$(shell uname -m | sed s/arm64/aarch64/)-darwin
  QJS_LIBS := *.dylib
else
  QJS_DIR := externals/quickjs/prebuilt/$(shell uname -m)-linux
  QJS_LIBS := *.so*
endif

LAZBUILD_FLAGS = --widgetset=$(WIDGETSET) --build-mode="$(BUILD_MODE_NAME)" $(CPU_FLAG)

# Determine a build-mode suitable for 'noext' (prefer Qt6 No Extensions or No Ext)
NOEXT_BUILD_MODE_NAME = No Ext ($(BUILD_MODE))

NOEXT_LAZBUILD_FLAGS = --widgetset=$(WIDGETSET) --build-mode="$(NOEXT_BUILD_MODE_NAME)" $(CPU_FLAG)

.PHONY: all help check build release debug test test-noserver noext-test noext-test-noserver clean dist install run list-modes list-modules check-module-names assets check-assets

all: release

help:
	@echo "Trndi Makefile"
	@echo "  make [target]"
	@echo "Targets:"
	@echo "  release    Build release (default)"
	@echo "  debug      Build debug"
	@echo "  build      Generic build (honors BUILD_MODE and WIDGETSET)"
	@echo "  test       Build and run tests (runner spawns an in-process Pascal test server)"
	@echo "  test-noserver  Run console tests, skipping the embedded test server (TRNDI_NO_TESTSERVER=1)"
	@echo "  noext-test  Build and run tests without extension support"
	@echo "  noext-test-noserver  Run console tests without extension support, skipping the test server (TRNDI_NO_TESTSERVER=1)"
	@echo "  list-modes Show available project build modes from $(LPI)"
	@echo "  list-modules Show Pascal 'unit' modules found under units/"
	@echo "  check-module-names Check for mismatches between filenames and 'unit' declarations (uses scripts/check-module-names.pl)"
	@echo "  show-mode  Show resolved build-mode and lazbuild flags"
	@echo "  noext      Build without JavaScript extension support (no QuickJS libraries needed) - use noext-release/noext-debug to override mode"
	@echo "  assets     Regenerate compiled-in resource bundles (.lrs), e.g. the CareLink login helper (needs lazres)"
	@echo "  check-assets  Fail if a committed .lrs is out of sync with its sources (CI guard)"
	@echo "  clean      Remove common build artifacts (*.o, *.ppu, *.compiled, executables)"
	@echo "  dist       Create a minimal tarball in $(OUTDIR)"
	@echo "  run        Build (if needed) and run the built binary (use RUN_ARGS to pass args)"
	@echo "  install    Install built binary to /usr/local/bin (requires sudo)"
	@echo "Variables:" 
	@echo "  LAZBUILD (default: lazbuild)"
	@echo "  WIDGETSET (default: $(WIDGETSET))"
	@echo "  BUILD_MODE (default: $(BUILD_MODE))"

check:
ifeq ($(OS),Windows_NT)
	@if exist "$(subst /,\,$(LAZBUILD))" (echo "Using $(LAZBUILD)") else (echo "lazbuild not found; please install Lazarus build tools (lazarus_bin) or set LAZBUILD" & exit 1)
else
	@command -v $(LAZBUILD) >/dev/null 2>&1 || (echo "lazbuild not found; please install Lazarus build tools (lazarus_bin)" && exit 1)
	@echo "Using $(LAZBUILD)"
endif
.PHONY: qjs-links
qjs-links:
	@# -lqjs resolves through the unversioned libqjs.so symlink, which is not in
	@# git (checkouts onto NTFS flatten symlinks into empty files), so recreate it
	@# in the library search directory before lazbuild links. macOS needs nothing
	@# here: build.sh ships a single unversioned libqjs.dylib for that reason.
	@real=$$(cd "$(QJS_DIR)" 2>/dev/null && ls libqjs.so.[0-9]*.[0-9]*.[0-9]* 2>/dev/null | head -1); \
	if [ -n "$$real" ]; then \
	  ( cd "$(QJS_DIR)" && ln -sf "$$real" libqjs.so.0 && ln -sf libqjs.so.0 libqjs.so ); \
	fi

build: qjs-links
	mkdir -p $(OUTDIR)
	@echo "Building $(LPI) (mode=$(BUILD_MODE_NAME), widgetset=$(WIDGETSET)) -> $(OUTDIR)"
	@# Note: avoid passing -B <outdir> to lazbuild — some lazbuild versions misinterpret it as a package name.
	@$(LAZBUILD) $(LAZBUILD_FLAGS) $(LPI)
	@echo "Build finished. Artifacts are in $(OUTDIR)"
	@# Ensure that if lazbuild produced the executable in the project dir we copy it into $(OUTDIR) (overwrite if present)
	@for f in "$(basename $(LPI))" "$(basename $(LPI)).exe" "$(basename $(LPI)).app"; do \
	  if [ -e "$$f" ]; then cp -r "$$f" "$(OUTDIR)/" && echo "Copied $$f to $(OUTDIR)"; fi; \
	done;
	@if [ -f "$(OUTDIR)/$(basename $(LPI))" ] || [ -f "$(OUTDIR)/$(basename $(LPI)).exe" ] || [ -d "$(OUTDIR)/$(basename $(LPI)).app" ]; then \
	  echo "Found in $(OUTDIR)"; \
	else \
	  echo "Warning: no executable found in project dir or $(OUTDIR)"; \
	fi; \
	# Copy translations into build dir for packaging/runtime
	if [ -d "lang" ]; then mkdir -p "$(OUTDIR)/lang" && cp -r lang/. "$(OUTDIR)/lang/" && echo "Copied translations to $(OUTDIR)/lang"; fi
	@# The extension engine links quickjs-ng and its ABI shim as shared libraries,
	@# which must sit beside the executable. Symlinks are not tracked in git, so
	@# recreate them here. Skipped for No Ext builds, which never load them.
	@if [ "$(BUILD_MODE_NAME)" != "No Ext" ]; then \
	  if [ -d "$(QJS_DIR)" ]; then \
	    for dest in "$(OUTDIR)" "$(OUTDIR)"/*.app/Contents/MacOS; do \
	      [ -d "$$dest" ] || continue; \
	      cp -P $(QJS_DIR)/$(QJS_LIBS) "$$dest/" 2>/dev/null || true; \
	      real=$$(cd "$$dest" && ls libqjs.so.[0-9]*.[0-9]*.[0-9]* 2>/dev/null | head -1); \
	      if [ -n "$$real" ]; then \
	        ( cd "$$dest" && ln -sf "$$real" libqjs.so.0 && ln -sf libqjs.so.0 libqjs.so ); \
	      fi; \
	      echo "Copied QuickJS libraries to $$dest"; \
	    done; \
	  else \
	    echo "Warning: $(QJS_DIR) missing; extensions will fail to start. Build it with externals/quickjs/build.sh"; \
	  fi; \
	fi
	@# Strip embedded debug info for smaller Release binaries (Linux default; override STRIP_RELEASE=0)
	@if [ "$(BUILD_MODE)" = "Release" ] && [ "$(STRIP_RELEASE)" = "1" ]; then \
	  if [ -f "$(OUTDIR)/Trndi" ]; then \
	    if command -v "$(STRIP)" >/dev/null 2>&1; then \
	      echo "Stripping $(OUTDIR)/Trndi"; \
	      "$(STRIP)" --strip-unneeded "$(OUTDIR)/Trndi" 2>/dev/null || "$(STRIP)" "$(OUTDIR)/Trndi" || true; \
	    else \
	      echo "strip not found; skipping strip step"; \
	    fi; \
	  fi; \
	fi

release: BUILD_MODE := Release
release: build

debug: BUILD_MODE := Debug
debug: build

test: check qjs-links
	@echo "Building console tests (tests/TrndiTestConsole.lpi)"
	@$(LAZBUILD) --widgetset=$(WIDGETSET) -B tests/TrndiTestConsole.lpi
	@# ext_js_tests links the QuickJS engine and its ABI shim; the test binary
	@# carries a runpath relative to itself, so put them beside it.
	@[ -d "$(QJS_DIR)" ] && cp -P $(QJS_DIR)/$(QJS_LIBS) tests/ || true
	@echo "Running console tests (embedded Pascal test server)"
	@./tests/TrndiTestConsole

noext-test: qjs-links
	@echo "Building console tests (tests/TrndiTestConsole.lpi) without extension support"
	@$(LAZBUILD) --widgetset=$(WIDGETSET) -B tests/TrndiTestConsole.lpi
	@# ext_js_tests links the QuickJS engine and its ABI shim; the test binary
	@# carries a runpath relative to itself, so put them beside it.
	@[ -d "$(QJS_DIR)" ] && cp -P $(QJS_DIR)/$(QJS_LIBS) tests/ || true
	@echo "Running console tests (embedded Pascal test server)"
	@./tests/TrndiTestConsole

test-noserver: check qjs-links
	@echo "Building console tests (tests/TrndiTestConsole.lpi)"
	@$(LAZBUILD) --widgetset=$(WIDGETSET) -B tests/TrndiTestConsole.lpi
	@# ext_js_tests links the QuickJS engine and its ABI shim; the test binary
	@# carries a runpath relative to itself, so put them beside it.
	@[ -d "$(QJS_DIR)" ] && cp -P $(QJS_DIR)/$(QJS_LIBS) tests/ || true
	@echo "Running tests without embedded test server (TRNDI_NO_TESTSERVER=1)"
	@TRNDI_NO_TESTSERVER=1 ./tests/TrndiTestConsole

noext-test-noserver: qjs-links
	@echo "Building console tests (tests/TrndiTestConsole.lpi) without extension support"
	@$(LAZBUILD) --widgetset=$(WIDGETSET) -B tests/TrndiTestConsole.lpi
	@# ext_js_tests links the QuickJS engine and its ABI shim; the test binary
	@# carries a runpath relative to itself, so put them beside it.
	@[ -d "$(QJS_DIR)" ] && cp -P $(QJS_DIR)/$(QJS_LIBS) tests/ || true
	@echo "Running tests without embedded test server (TRNDI_NO_TESTSERVER=1)"
	@TRNDI_NO_TESTSERVER=1 ./tests/TrndiTestConsole

# Regenerate compiled-in resource bundles (.lrs) from their source files, e.g.
# the CareLink login helper. Run after editing tools/carelink-login/* and commit
# the updated .lrs. Mirrors 'make.ps1 assets' on Windows.
assets:
	@if [ -z "$(LAZRES)" ]; then echo "lazres not found; set LAZRES=/path/to/lazres (it ships with Lazarus, e.g. /usr/lib/lazarus/<ver>/tools/lazres)"; exit 1; fi
	@echo "Regenerating $(CARELINK_ASSET_LRS) via $(LAZRES)"
	@$(LAZRES) $(CARELINK_ASSET_LRS) $(CARELINK_ASSET_SRCS)

# Fail if a committed .lrs is out of sync with its source files (CI guard). The
# resource content is keyed off the source file names, not the output path, so a
# temp regeneration diffs cleanly against the committed copy.
check-assets:
	@if [ -z "$(LAZRES)" ]; then echo "lazres not found; cannot verify $(CARELINK_ASSET_LRS) is current. Set LAZRES=/path/to/lazres."; exit 1; fi
	@tmp=$$(mktemp); \
	 $(LAZRES) $$tmp $(CARELINK_ASSET_SRCS) >/dev/null; \
	 if ! diff --strip-trailing-cr -q "$$tmp" "$(CARELINK_ASSET_LRS)" >/dev/null 2>&1; then \
	   rm -f "$$tmp"; \
	   echo "ERROR: $(CARELINK_ASSET_LRS) is stale — run 'make assets' and commit the result."; \
	   exit 1; \
	 fi; \
	 rm -f "$$tmp"; \
	 echo "$(CARELINK_ASSET_LRS) is up to date."

list-modes:
	@echo "Available build modes in $(LPI):"
	@perl -0777 -ne 'while (/<Item\s+Name\s*=\s*"([^"]+)"/g) { print "  - $$1\n" }' $(LPI) || true

list-modules:
	@echo "Modules (units) found under units/ (grouped by dot-separated names):"
	@find units -type f \( -name '*.pp' -o -name '*.pas' \) -print0 | xargs -0 -n1 perl -nle 'if (/^\s*unit\s+([A-Za-z0-9_.]+)/) { print "$$1\t$$ARGV"; close(ARGV) }' | sort -u | perl scripts/list-modules-tree.pl || echo "  (no modules found)"

check-module-names:
	@echo "Checking unit declarations vs filenames..."
	@find units -type f \( -name '*.pp' -o -name '*.pas' \) -print0 | xargs -0 -n1 scripts/check-module-names.pl || (echo "\nOne or more mismatches found; run 'make check-module-names' to show them" && exit 1)

show-mode:
	@echo "Resolved build mode: $(BUILD_MODE_NAME)"
	@echo lazbuild flags: $(LAZBUILD_FLAGS)
	@echo "(WIDGETSET=$(WIDGETSET), BUILD_MODE=$(BUILD_MODE))"

# Run the built binary (build first). Use RUN_ARGS to forward arguments to the program.
run: build
	@echo "Running Trndi from $(OUTDIR)"
	@set -e; \
	if [ -x "$(OUTDIR)/Trndi" ]; then "$(OUTDIR)/Trndi" $(RUN_ARGS); \
	elif [ -x "$(OUTDIR)/Trndi.app/Contents/MacOS/Trndi" ]; then "$(OUTDIR)/Trndi.app/Contents/MacOS/Trndi" $(RUN_ARGS); \
	elif [ -f "$(OUTDIR)/Trndi.exe" ]; then "$(OUTDIR)/Trndi.exe" $(RUN_ARGS); \
	elif [ -x "./Trndi" ]; then ./Trndi $(RUN_ARGS); \
	elif [ -f "./Trndi.exe" ]; then ./Trndi.exe $(RUN_ARGS); \
	else echo "Executable not found in $(OUTDIR) or project dir; build first"; exit 1; fi

# Build without JavaScript extension support. The "No Ext" build modes compile
# without TrndiExt, so nothing links QuickJS and no shared libraries are needed.
noext:
	@echo "Building without extensions (mode=$(NOEXT_BUILD_MODE_NAME), widgetset=$(WIDGETSET))"
	@set -e; \
	 $(LAZBUILD) $(NOEXT_LAZBUILD_FLAGS) $(LPI); \
	 { \
	  echo "Build complete (without extensions). Copying artifacts to $(OUTDIR)"; \
	  mkdir -p $(OUTDIR); \
	  for f in "$(basename $(LPI))" "$(basename $(LPI)).exe" "$(basename $(LPI)).app"; do \
	    if [ -e "$$f" ]; then cp -r "$$f" "$(OUTDIR)/" && echo "Copied $$f to $(OUTDIR)"; fi; \
	  done; \
	  if [ -f "$(OUTDIR)/$(basename $(LPI))" ] || [ -f "$(OUTDIR)/$(basename $(LPI)).exe" ] || [ -d "$(OUTDIR)/$(basename $(LPI)).app" ]; then \
	    echo "Found in $(OUTDIR)"; \
	  else \
	    echo "Warning: no executable found in project dir or $(OUTDIR)"; \
	  fi; \
	  if [ -d "lang" ]; then mkdir -p "$(OUTDIR)/lang" && cp -r lang/. "$(OUTDIR)/lang/" && echo "Copied translations to $(OUTDIR)/lang"; fi; \
	  if [ "$(BUILD_MODE)" = "Release" ] && [ "$(STRIP_RELEASE)" = "1" ]; then \
	    if [ -f "$(OUTDIR)/Trndi" ]; then \
	      if command -v "$(STRIP)" >/dev/null 2>&1; then \
	        echo "Stripping $(OUTDIR)/Trndi"; \
	        "$(STRIP)" --strip-unneeded "$(OUTDIR)/Trndi" 2>/dev/null || "$(STRIP)" "$(OUTDIR)/Trndi" || true; \
	      fi; \
	    fi; \
	  fi; \
	 }

noext-release: BUILD_MODE := Release
noext-release: noext

noext-debug: BUILD_MODE := Debug
noext-debug: noext

clean:
	@echo "Cleaning common products..."
	@find . -maxdepth 3 \( \
	  \( -type f \( -name '*.o' -o -name '*.ppu' -o -name '*.compiled' -o -name '*.a' -o -name '*.so' -o -name '*.dll' -o -name '*.exe' -o -name '*.app' -o -name '$(LPI).noext-*' -o -name '*.noext-*.lpi' -o -name '*.noext-*.res' -o -name '*.noext-*.ico' -o -name '*.noext-*.png' \) \) \
	  -o \( -type d -name '*.app' \) \
	\) -print0 | xargs -0 -r rm -rf || true
	@echo "(Note: Lazarus project files and sources are not removed, but temporary noext project files (e.g. $(LPI).noext-*) are cleaned.)"

dist: build
	@mkdir -p $(OUTDIR)
	@TSTAMP=$$(date +%Y%m%d%H%M%S); echo "Creating $(OUTDIR)/trndi-$${TSTAMP}.tar.gz"; tar czf "$(OUTDIR)/trndi-$${TSTAMP}.tar.gz" README.md Trndi.lpi || true

install: build
	@echo "Installing binary to /usr/local/bin (requires sudo)"
	@set -e; \
	NAME="$(basename $(LPI))"; \
	SRC=""; \
	if [ -x "$(OUTDIR)/$$NAME" ]; then SRC="$(OUTDIR)/$$NAME"; \
	elif [ -x "$(OUTDIR)/$$NAME.app/Contents/MacOS/$$NAME" ]; then SRC="$(OUTDIR)/$$NAME.app/Contents/MacOS/$$NAME"; \
	elif [ -f "$(OUTDIR)/$$NAME.exe" ]; then SRC="$(OUTDIR)/$$NAME.exe"; \
	elif [ -x "./$$NAME" ]; then SRC="./$$NAME"; \
	elif [ -f "./$$NAME.exe" ]; then SRC="./$$NAME.exe"; \
	else echo "Binary not found. Run 'make release' and ensure lazbuild produced an executable in the project dir."; exit 1; fi; \
	sudo install -m 755 "$$SRC" /usr/local/bin/
