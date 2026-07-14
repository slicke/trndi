#!/bin/zsh

set -e

VERSION="${VERSION:-1.0.0}"

rm -rf macos
mkdir macos

# Make App
cp -r ../Trndi.app macos/.
rm -rf macos/Trndi.app/Contents/MacOS/Trndi
cp ../Trndi macos/Trndi.app/Contents/MacOS/Trndi

# Setup languages
# Trndi currently looks for translations in "appdir/lang".
# In a .app bundle, appdir resolves to Contents/MacOS, so keep lang/ there.
mkdir -p macos/Trndi.app/Contents/MacOS
if [ -d "../lang" ]; then
  cp -r ../lang macos/Trndi.app/Contents/MacOS/
fi

# The CareLink login helper is compiled into the binary (see
# units/trndi/api/carelink_assets.lrs) and written to the user's writable
# settings folder on demand, so nothing is bundled into the .app here —
# writing into a signed bundle would break its signature anyway.

# Create icons
mkdir macos/Trndi.iconset

# macOS dock/app icon: prefer the macOS-specific artwork, fall back to Trndi.png.
ICON_SRC="../Trndi-macos.png"
[ -f "${ICON_SRC}" ] || ICON_SRC="../Trndi.png"

# Use Apple-standard iconset names so iconutil reliably produces a correct .icns
sips -z 16 16    "${ICON_SRC}" --out macos/Trndi.iconset/icon_16x16.png
sips -z 32 32    "${ICON_SRC}" --out macos/Trndi.iconset/icon_16x16@2x.png
sips -z 32 32    "${ICON_SRC}" --out macos/Trndi.iconset/icon_32x32.png
sips -z 64 64    "${ICON_SRC}" --out macos/Trndi.iconset/icon_32x32@2x.png
sips -z 128 128  "${ICON_SRC}" --out macos/Trndi.iconset/icon_128x128.png
sips -z 256 256  "${ICON_SRC}" --out macos/Trndi.iconset/icon_128x128@2x.png
sips -z 256 256  "${ICON_SRC}" --out macos/Trndi.iconset/icon_256x256.png
sips -z 512 512  "${ICON_SRC}" --out macos/Trndi.iconset/icon_256x256@2x.png
sips -z 512 512  "${ICON_SRC}" --out macos/Trndi.iconset/icon_512x512.png
sips -z 1024 1024 "${ICON_SRC}" --out macos/Trndi.iconset/icon_512x512@2x.png

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
if [ -f "macos_README.txt" ]; then
  cp macos_README.txt "macos/stage/README.txt"
fi
rm -f Trndi.dmg

DMG_BG_ARG=()
if [ -f "macos_dmg_background.swift" ] && command -v swift >/dev/null 2>&1; then
  if swift macos_dmg_background.swift macos/dmg-background.png; then
    if [ -f "macos/dmg-background.png" ]; then
      DMG_BG_ARG=(--background "macos/dmg-background.png")
      echo "DMG background generated: macos/dmg-background.png"
    else
      echo "WARN: swift exited 0 but macos/dmg-background.png is missing" >&2
    fi
  else
    echo "WARN: swift failed to generate DMG background; proceeding without it" >&2
  fi
else
  echo "WARN: swift not available or background script missing; no DMG background" >&2
fi

# Diagnostic: which create-dmg are we actually running?
echo "create-dmg path: $(command -v create-dmg)"
create-dmg --version 2>&1 | head -3 || true

# Pass options BEFORE positional args (Trndi.dmg + source dir). Some
# create-dmg variants stop option parsing at the first positional, so options
# after the .dmg path were being silently ignored — that's why earlier runs
# produced a DMG with no background and no Applications symlink.
create-dmg \
  --volname "Trndi" \
  --format UDZO \
  --window-size 600 500 \
  "${DMG_BG_ARG[@]}" \
  --icon-size 128 \
  --icon "Trndi.app" 150 200 \
  --icon "README.txt" 300 360 \
  --app-drop-link 450 200 \
  Trndi.dmg "macos/stage" 2>&1 | grep -v "internet-enable"

# Zsh's default nomatch behavior aborts the script when a glob has no matches,
# even with `|| true`. The (N) glob qualifier enables null_glob for this one
# pattern, so it expands to nothing instead of erroring.
rm -f rw*Trndi*.dmg(N)
rm -rf macos