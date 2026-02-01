# Makefile for Trndi — simple wrapper around lazbuild
# Usage examples:
#   make          -> release build (default)
#   make debug    -> debug build
#   make release  -> release build
#   make test     -> build the tests
#   make clean    -> remove build artifacts
#   make install  -> install binary to /usr/local/bin (requires sudo)

LAZBUILD ?= lazbuild
LPI ?= Trndi.lpi
TEST_LPI ?= tests/TrndiTest.lpi
OUTDIR ?= build
WIDGETSET ?= qt6
# BUILD_MODE is a short hint (Release/Debug) for backward compatibility
# BUILD_MODE_NAME is the actual project build-mode name as seen in Trndi.lpi
BUILD_MODE ?= Release
BUILD_MODE_NAME ?= No Ext (Release)
CPU_FLAG ?=

# Auto-detect OS and pick appropriate default build-mode
UNAME_S := $(shell uname -s)
IS_MINGW := $(findstring MINGW,$(UNAME_S))
IS_CYGWIN := $(findstring CYGWIN,$(UNAME_S))

# Linux: prefer Qt6 builds
ifeq ($(UNAME_S),Linux)
  WIDGETSET ?= qt6
  ifeq ($(BUILD_MODE),Debug)
    BUILD_MODE_NAME := Qt6 (Debug)
  else
    BUILD_MODE_NAME := Qt6 (Release)
  endif

# macOS: prefer native Release builds (Extensions)
else ifeq ($(UNAME_S),Darwin)
  ifeq ($(BUILD_MODE),Debug)
    BUILD_MODE_NAME := Extensions (Debug)
  else
    BUILD_MODE_NAME := Extensions (Release)
  endif

# Windows (MSYS/Cygwin): prefer native Release builds (Extensions)
else ifneq ($(IS_MINGW),)
  ifeq ($(BUILD_MODE),Debug)
    BUILD_MODE_NAME := Extensions (Debug)
  else
    BUILD_MODE_NAME := Extensions (Release)
  endif
else ifneq ($(IS_CYGWIN),)
  ifeq ($(BUILD_MODE),Debug)
    BUILD_MODE_NAME := Extensions (Debug)
  else
    BUILD_MODE_NAME := Extensions (Release)
  endif

# Fallback: use qt6 if requested, otherwise No Ext
else
  ifeq ($(WIDGETSET),qt6)
    ifeq ($(BUILD_MODE),Debug)
      BUILD_MODE_NAME := Qt6 (Debug)
    else
      BUILD_MODE_NAME := Qt6 (Release)
    endif
  else
    ifeq ($(BUILD_MODE),Debug)
      BUILD_MODE_NAME := No Ext (Debug)
    else
      BUILD_MODE_NAME := No Ext (Release)
    endif
  endif
endif

LAZBUILD_FLAGS ?= --widgetset=$(WIDGETSET) --build-mode="$(BUILD_MODE_NAME)" $(CPU_FLAG)

.PHONY: all help check build release debug test clean dist install

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
	@echo "  noext      Build without mORMot2 (temporary project) - use noext-release/noext-debug to override mode"
	@echo "  clean      Remove common build artifacts (*.o, *.ppu, *.compiled, executables)"
	@echo "  dist       Create a minimal tarball in $(OUTDIR)"
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
	@echo "Building $(LPI) (mode=$(BUILD_MODE), widgetset=$(WIDGETSET))"
	@$(LAZBUILD) $(LAZBUILD_FLAGS) -B $(LPI)
	@echo "Build finished. You should find the built binary next to the project or in Lazarus' default output location."
	@echo "(Optional) Copying any found executable to $(OUTDIR)"
	@for f in $(basename $(LPI)); do if [ -f "$$f" ]; then cp "$$f" "$(OUTDIR)/"; fi; done || true

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

# Build without extensions (use a temporary copy of the .lpi so original is untouched)
noext: check
	@echo "Building without extensions (using temporary project, mormot2 removed)"
	@STAMP=$$(date +%s) && \
	 cp $(LPI) $(LPI).noext-$$STAMP && \
	 # Remove the mormot2 package entry from the temporary project
	 perl -0777 -pe 's/\s*<Item>\s*<PackageName Value="mormot2"\/?>\s*<\/Item>\s*//s' $(LPI).noext-$$STAMP > $(LPI).noext-$$STAMP.tmp && \
	 mv $(LPI).noext-$$STAMP.tmp $(LPI).noext-$$STAMP && \
	 echo "Using temporary project: $(LPI).noext-$$STAMP" && \
	 $(LAZBUILD) $(LAZBUILD_FLAGS) -B $(LPI).noext-$$STAMP; \
	 RET=$$?; \
	 if [ $$RET -ne 0 ]; then \
	  echo "Build failed (status $$RET). The temporary project is kept at $(LPI).noext-$$STAMP for inspection."; \
	  exit $$RET; \
	 else \
	  echo "Build complete (without mORMot2). Removing temporary project $(LPI).noext-$$STAMP"; \
	  rm -f $(LPI).noext-$$STAMP; \
	 fi

noext-release: BUILD_MODE := Release
noext-release: noext

noext-debug: BUILD_MODE := Debug
noext-debug: noext

clean:
	@echo "Cleaning common products..."
	@find . -maxdepth 3 -type f \( -name '*.o' -o -name '*.ppu' -o -name '*.compiled' -o -name '*.a' -o -name '*.so' -o -name '*.dll' -o -name '*.exe' -o -name '*.app' \) -print0 | xargs -0 -r rm -f || true
	@echo "(Note: Lazarus project files, .lpi, and sources are not removed.)"

dist: build
	@mkdir -p $(OUTDIR)
	@TSTAMP=$$(date +%Y%m%d%H%M%S); echo "Creating $(OUTDIR)/trndi-$${TSTAMP}.tar.gz"; tar czf "$(OUTDIR)/trndi-$${TSTAMP}.tar.gz" README.md Trndi.lpi || true

install: build
	@echo "Installing binary to /usr/local/bin (requires sudo)"
	@if [ -f "$(basename $(LPI))" ]; then sudo install -m 755 "$(basename $(LPI))" /usr/local/bin/; else echo "Binary not found. Run 'make release' and ensure lazbuild produced an executable in the project dir."; fi
