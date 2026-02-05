# Makefile for Trndi — simple wrapper around lazbuild
# Usage examples:
#   make          -> release build (default)
#   make debug    -> debug build
#   make release  -> release build
#   make test     -> build the tests
#   make clean    -> remove build artifacts
#   make install  -> install binary to /usr/local/bin (requires sudo)
#   make list-modes -> list available build modes from Trndi.lpi

LAZBUILD ?= lazbuild
LPI ?= Trndi.lpi
TEST_LPI ?= tests/TrndiTest.lpi
OUTDIR ?= build
WIDGETSET ?= qt6
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
UNAME_S := $(shell uname -s)
IS_MINGW := $(findstring MINGW,$(UNAME_S))
IS_CYGWIN := $(findstring CYGWIN,$(UNAME_S))

# Linux: prefer Qt6 builds; when multiple Qt6 release modes exist prefer the Extensions variant
ifeq ($(UNAME_S),Linux)
  WIDGETSET ?= qt6
	# On Linux, prefer the generic project modes if they exist; --widgetset=qt6 controls the LCL backend.
	# This avoids having to maintain widgetset-specific modes just to get correct Release flags.
	STRIP_RELEASE ?= 1

# macOS: prefer native Release builds (Extensions)
else ifeq ($(UNAME_S),Darwin)

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

LAZBUILD_FLAGS = --widgetset=$(WIDGETSET) --build-mode="$(BUILD_MODE_NAME)" $(CPU_FLAG)

# Determine a build-mode suitable for 'noext' (prefer Qt6 No Extensions or No Ext)
NOEXT_BUILD_MODE_NAME = No Ext ($(BUILD_MODE))

NOEXT_LAZBUILD_FLAGS = --widgetset=$(WIDGETSET) --build-mode="$(NOEXT_BUILD_MODE_NAME)" $(CPU_FLAG)

.PHONY: all help check build release debug test clean dist install run list-modes list-modules

all: release

help:
	@echo "Trndi Makefile"
	@echo "  make [target]"
	@echo "Targets:"
	@echo "  release    Build release (default)"
	@echo "  debug      Build debug"
	@echo "  build      Generic build (honors BUILD_MODE and WIDGETSET)"
	@echo "  test       Build tests"
	@echo "  list-modes Show available project build modes from $(LPI)"
	@echo "  list-modules Show Pascal `unit` modules found under `units/`"
	@echo "  show-mode  Show resolved build-mode and lazbuild flags"
	@echo "  noext      Build without mORMot2 (temporary project) - use noext-release/noext-debug to override mode"
	@echo "  clean      Remove common build artifacts (*.o, *.ppu, *.compiled, executables)"
	@echo "  dist       Create a minimal tarball in $(OUTDIR)"
	@echo "  run        Build (if needed) and run the built binary (use RUN_ARGS to pass args)"
	@echo "  install    Install built binary to /usr/local/bin (requires sudo)"
	@echo "Variables:" 
	@echo "  LAZBUILD (default: lazbuild)"
	@echo "  WIDGETSET (default: $(WIDGETSET))"
	@echo "  BUILD_MODE (default: $(BUILD_MODE))"

check:
	@command -v $(LAZBUILD) >/dev/null 2>&1 || (echo "lazbuild not found; please install Lazarus build tools (lazarus_bin)" && exit 1)
	@echo "Using $(LAZBUILD)"

build: check
	@mkdir -p $(OUTDIR)
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

test: check
	@echo "Building tests ($(TEST_LPI))"
	@$(LAZBUILD) --widgetset=$(WIDGETSET) -B $(TEST_LPI)

list-modes:
	@echo "Available build modes in $(LPI):"
	@perl -0777 -ne 'while (/<Item\s+Name\s*=\s*"([^"]+)"/g) { print "  - $$1\n" }' $(LPI) || true

list-modules:
	@echo "Modules (units) found under units/ (grouped by dot-separated names):"
	@find units -type f \( -name '*.pp' -o -name '*.pas' \) -print0 | xargs -0 -n1 perl -nle 'if (/^\s*unit\s+([A-Za-z0-9_.]+)/) { print "$$1\t$$ARGV"; close(ARGV) }' | sort -u | perl scripts/list-modules-tree.pl || echo "  (no modules found)"

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

# Build without extensions (use a temporary copy of the .lpi so original is untouched)
noext: check
	@echo "Building without extensions (using temporary project, mormot2 removed)"
	@set -e; STAMP=$$(date +%s); \
	 cp $(LPI) $(LPI).noext-$$STAMP.lpi; \
	 perl -0777 -pe 's/\s*<Item>\s*<PackageName Value="mormot2"\/?>(\s*)<\/Item>\s*//s' $(LPI).noext-$$STAMP.lpi > $(LPI).noext-$$STAMP.lpi.tmp; \
	 mv $(LPI).noext-$$STAMP.lpi.tmp $(LPI).noext-$$STAMP.lpi; \
	 cp Trndi.res $(LPI).noext-$$STAMP.res; \
	 cp Trndi.ico $(LPI).noext-$$STAMP.ico; \
	 cp Trndi.png $(LPI).noext-$$STAMP.png; \
	 echo "Using temporary project: $(LPI).noext-$$STAMP.lpi"; \
	 set +e; \
	 $(LAZBUILD) $(NOEXT_LAZBUILD_FLAGS) $(LPI).noext-$$STAMP.lpi; \
	 RET=$$?; \
	 set -e; \
	 if [ $$RET -ne 0 ]; then \
	  echo "Build failed (status $$RET). The temporary project is kept at $(LPI).noext-$$STAMP.lpi for inspection."; \
	  exit $$RET; \
	 else \
	  echo "Build complete (without mORMot2). Copying artifacts to $(OUTDIR)"; \
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
	  rm -f $(LPI).noext-$$STAMP.lpi $(LPI).noext-$$STAMP.res $(LPI).noext-$$STAMP.ico $(LPI).noext-$$STAMP.png; \
	 fi

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
