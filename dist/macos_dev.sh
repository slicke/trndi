#!/bin/zsh

set -e

# If invoked with sh/bash (ignoring the shebang), re-exec under zsh so we can
# rely on consistent behavior.
if [ -z "${ZSH_VERSION:-}" ]; then
  exec /bin/zsh "$0" "$@"
fi

set -u
set -o pipefail

VERSION="${VERSION:-1.0.0}"

# Developer-friendly wrapper of macos.sh:
# - Can be invoked from any working directory
# - Avoids create-dmg --out (some create-dmg variants interpret it as an output dir)

SCRIPT_DIR="$(cd -- "$(dirname -- "$0")" && pwd)"
ROOT_DIR="$(cd -- "${SCRIPT_DIR}/.." && pwd)"
cd "${SCRIPT_DIR}"

die() {
  echo "ERROR: $*" >&2
  exit 1
}

need_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "Missing required command: $1"
}

need_file() {
  [ -e "$1" ] || die "Missing required file: $1"
}

need_cmd sips
need_cmd iconutil
need_cmd hdiutil
need_cmd osascript

# create-dmg is optional for dev builds (there are multiple incompatible
# implementations with the same name). We'll fall back to hdiutil if needed.
if ! command -v create-dmg >/dev/null 2>&1; then
  echo "WARN: 'create-dmg' not found; will use 'hdiutil create' fallback." >&2
fi

# Optional: create-dmg often uses SetFile (Xcode Command Line Tools)
if ! command -v SetFile >/dev/null 2>&1; then
  echo "WARN: 'SetFile' not found. Install Xcode Command Line Tools: xcode-select --install" >&2
fi

need_file "${ROOT_DIR}/Trndi.app"
need_file "${ROOT_DIR}/Trndi"
need_file "${ROOT_DIR}/Trndi.png"

rm -rf macos
mkdir macos

# Make App
cp -r "${ROOT_DIR}/Trndi.app" macos/.
rm -rf macos/Trndi.app/Contents/MacOS/Trndi
cp "${ROOT_DIR}/Trndi" macos/Trndi.app/Contents/MacOS/Trndi

# Setup languages
# Trndi currently looks for translations in "appdir/lang".
# In a .app bundle, appdir resolves to Contents/MacOS, so keep lang/ there.
mkdir -p macos/Trndi.app/Contents/MacOS
if [ -d "${ROOT_DIR}/lang" ]; then
  cp -r "${ROOT_DIR}/lang" macos/Trndi.app/Contents/MacOS/
fi

# Install AppleScript dictionary if provided (PoC)
if [ -f "${ROOT_DIR}/mac/Trndi.sdef" ]; then
  mkdir -p macos/Trndi.app/Contents/Resources
  cp "${ROOT_DIR}/mac/Trndi.sdef" macos/Trndi.app/Contents/Resources/Trndi.sdef
  chmod 644 macos/Trndi.app/Contents/Resources/Trndi.sdef
fi

# Create icons
mkdir macos/Trndi.iconset

# Use Apple-standard iconset names so iconutil reliably produces a correct .icns
sips -z 16 16     "${ROOT_DIR}/Trndi.png" --out macos/Trndi.iconset/icon_16x16.png
sips -z 32 32     "${ROOT_DIR}/Trndi.png" --out macos/Trndi.iconset/icon_16x16@2x.png
sips -z 32 32     "${ROOT_DIR}/Trndi.png" --out macos/Trndi.iconset/icon_32x32.png
sips -z 64 64     "${ROOT_DIR}/Trndi.png" --out macos/Trndi.iconset/icon_32x32@2x.png
sips -z 128 128   "${ROOT_DIR}/Trndi.png" --out macos/Trndi.iconset/icon_128x128.png
sips -z 256 256   "${ROOT_DIR}/Trndi.png" --out macos/Trndi.iconset/icon_128x128@2x.png
sips -z 256 256   "${ROOT_DIR}/Trndi.png" --out macos/Trndi.iconset/icon_256x256.png
sips -z 512 512   "${ROOT_DIR}/Trndi.png" --out macos/Trndi.iconset/icon_256x256@2x.png
sips -z 512 512   "${ROOT_DIR}/Trndi.png" --out macos/Trndi.iconset/icon_512x512.png
sips -z 1024 1024 "${ROOT_DIR}/Trndi.png" --out macos/Trndi.iconset/icon_512x512@2x.png

iconutil -c icns macos/Trndi.iconset
cp macos/Trndi.icns macos/Trndi.app/Contents/Resources/

# Create Info.plist
# Note: heredoc must be unquoted so ${VERSION} expands.
cat > "macos/Trndi.app/Contents/Info.plist" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
   "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key>
  <string>Trndi</string>
  <key>CFBundleIdentifier</key>
  <string>com.slicke.Trndi</string>
  <key>CFBundleExecutable</key>
  <string>Trndi</string>
  <key>CFBundleVersion</key>
  <string>${VERSION}</string>
  <key>CFBundleShortVersionString</key>
  <string>${VERSION}</string>
  <key>CFBundleIconFile</key>
  <string>Trndi.icns</string>
  <key>NSHighResolutionCapable</key>
  <true/>
  <key>CSResourcesFileMapped</key>
  <true/>
  <key>CFBundleDocumentTypes</key>
  <array>
    <dict>
      <key>CFBundleTypeRole</key>
      <string>Viewer</string>
      <key>CFBundleTypeExtensions</key>
      <array>
        <string>*</string>
      </array>
      <key>CFBundleTypeOSTypes</key>
      <array>
        <string>fold</string>
        <string>disk</string>
        <string>****</string>
      </array>
    </dict>
  </array>
  <key>CFBundleInfoDictionaryVersion</key>
  <string>6.0</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
</dict>
</plist>
EOF

chmod 644 "macos/Trndi.app/Contents/Info.plist"

# Create DMG
mkdir -p macos/stage
cp -R macos/Trndi.app macos/stage/
rm -f Trndi.dmg

# Track DMGs created during create-dmg run (some create-dmg variants may not
# write exactly ./Trndi.dmg even if they succeed).
MARKER_FILE="$(mktemp -t trndi_dmg_marker.XXXXXX)"
touch "${MARKER_FILE}"
CREATE_DMG_LOG="macos/create-dmg.log"
CREATE_DMG_HELP_LOG="macos/create-dmg.help.log"
rm -f "${CREATE_DMG_LOG}"

# Capture help text for variant detection / debugging (only if create-dmg exists).
if command -v create-dmg >/dev/null 2>&1; then
  {
    create-dmg --help
  } >"${CREATE_DMG_HELP_LOG}" 2>&1 || {
    {
      create-dmg -h
    } >"${CREATE_DMG_HELP_LOG}" 2>&1 || true
  }
else
  echo "create-dmg not installed" >"${CREATE_DMG_HELP_LOG}"
fi

run_create_dmg_attempt() {
  local attempt_name="$1"
  shift

  echo "Attempt: ${attempt_name}" >"${CREATE_DMG_LOG}"
  echo "Working dir: $(pwd)" >>"${CREATE_DMG_LOG}"
  echo "Command: $*" >>"${CREATE_DMG_LOG}"
  echo "---" >>"${CREATE_DMG_LOG}"

  set +e
  "$@" >>"${CREATE_DMG_LOG}" 2>&1
  local exit_code=$?
  set -e

  return ${exit_code}
}


# Try to accommodate different create-dmg variants (if installed).
CREATE_DMG_EXIT=127
if command -v create-dmg >/dev/null 2>&1; then
  CREATE_DMG_EXIT=0
  if grep -q -- "--out" "${CREATE_DMG_HELP_LOG}" 2>/dev/null; then
    # Matches CI script style.
    run_create_dmg_attempt "with --out (CI style)" \
      create-dmg Trndi.dmg "macos/stage" --volname "Trndi" --format UDZO --icon-size 128 --icon "Trndi.app" 150 200 --app-drop-link 250 200 --out "Trndi.dmg" || CREATE_DMG_EXIT=$?
  else
    # Common upstream style.
    run_create_dmg_attempt "without --out" \
      create-dmg "Trndi.dmg" "macos/stage" --volname "Trndi" --format UDZO --icon-size 128 --icon "Trndi.app" 150 200 --app-drop-link 250 200 || CREATE_DMG_EXIT=$?
  fi

  # If the first attempt failed, try the other style as a fallback.
  if [ ${CREATE_DMG_EXIT} -ne 0 ]; then
    if grep -q -- "--out" "${CREATE_DMG_HELP_LOG}" 2>/dev/null; then
      run_create_dmg_attempt "without --out (fallback)" \
        create-dmg "Trndi.dmg" "macos/stage" --volname "Trndi" --format UDZO --icon-size 128 --icon "Trndi.app" 150 200 --app-drop-link 250 200 || CREATE_DMG_EXIT=$?
    else
      run_create_dmg_attempt "with --out (fallback)" \
        create-dmg Trndi.dmg "macos/stage" --volname "Trndi" --format UDZO --icon-size 128 --icon "Trndi.app" 150 200 --app-drop-link 250 200 --out "Trndi.dmg" || CREATE_DMG_EXIT=$?
    fi
  fi
fi

if [ ${CREATE_DMG_EXIT} -ne 0 ]; then
  echo "WARN: create-dmg failed (exit ${CREATE_DMG_EXIT}); trying hdiutil fallback" >&2
fi

# If create-dmg is missing/incompatible OR it did not produce Trndi.dmg,
# create the DMG directly using macOS's built-in tooling.
if [ ${CREATE_DMG_EXIT} -ne 0 ] || [ ! -f "Trndi.dmg" ]; then
  echo "Attempt: hdiutil create" >"${CREATE_DMG_LOG}"
  echo "Working dir: $(pwd)" >>"${CREATE_DMG_LOG}"
  echo "Command: hdiutil create -volname Trndi -srcfolder macos/stage -ov -format UDZO Trndi.dmg" >>"${CREATE_DMG_LOG}"
  echo "---" >>"${CREATE_DMG_LOG}"

  set +e
  hdiutil create -volname "Trndi" -srcfolder "macos/stage" -ov -format UDZO "Trndi.dmg" >>"${CREATE_DMG_LOG}" 2>&1
  HDIUTIL_EXIT=$?
  set -e

  if [ ${HDIUTIL_EXIT} -ne 0 ] || [ ! -f "Trndi.dmg" ]; then
    echo "ERROR: DMG creation failed (create-dmg exit ${CREATE_DMG_EXIT}; hdiutil exit ${HDIUTIL_EXIT})" >&2
    echo "Working dir: $(pwd)" >&2
    echo "create-dmg: $(command -v create-dmg 2>/dev/null || echo 'not installed')" >&2
    echo "Help log: ${CREATE_DMG_HELP_LOG}" >&2
    echo "Log file: ${CREATE_DMG_LOG}" >&2
    echo "Stage dir contents:" >&2
    ls -la "macos/stage" >&2 || true
    echo "Stage app contents:" >&2
    ls -la "macos/stage/Trndi.app" >&2 || true
    echo "---- dmg tool output ----" >&2
    cat "${CREATE_DMG_LOG}" >&2 || true
    echo "-------------------------" >&2
    exit 1
  fi
fi

if [ ! -f "Trndi.dmg" ]; then
  # Look for any newly created DMGs near the working directory.
  dmg_candidates=("${(@0)$(find . macos -maxdepth 4 -type f -name "*.dmg" -newer "${MARKER_FILE}" -print0 2>/dev/null)}")

  # Prefer a DMG that contains "Trndi" in the name.
  best_candidate=""
  for f in "${dmg_candidates[@]}"; do
    case "${f}" in
      *Trndi*.dmg) best_candidate="${f}"; break ;;
    esac
  done

  # If no Trndi-named DMG found but exactly one DMG was produced, use it.
  if [ -z "${best_candidate}" ] && [ ${#dmg_candidates[@]} -eq 1 ]; then
    best_candidate="${dmg_candidates[1]}"
  fi

  if [ -n "${best_candidate}" ] && [ -f "${best_candidate}" ]; then
    mv -f "${best_candidate}" "Trndi.dmg"
  fi
fi

rm -f "${MARKER_FILE}" 2>/dev/null || true

if [ ! -f "Trndi.dmg" ]; then
  echo "ERROR: Could not find Trndi.dmg after create-dmg succeeded." >&2
  echo "Working dir: $(pwd)" >&2
  echo "DMG candidates in $(pwd):" >&2
  ls -la ./*.dmg 2>/dev/null || true
  echo "DMG candidates under $(pwd) (maxdepth 4):" >&2
  find "$(pwd)" -maxdepth 4 -name "*.dmg" -print 2>/dev/null || true
  echo "---- create-dmg output ----" >&2
  cat "${CREATE_DMG_LOG}" >&2 || true
  echo "--------------------------" >&2
  exit 1
fi

rm -f rw*Trndi*.dmg
rm -rf macos

echo "Created: $(pwd)/Trndi.dmg"
