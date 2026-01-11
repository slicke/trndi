#!/bin/zsh

set -e

VERSION="${VERSION:-1.0.0}"

# Developer-friendly wrapper of macos.sh:
# - Can be invoked from any working directory
# - Avoids create-dmg --out (some create-dmg variants interpret it as an output dir)

SCRIPT_DIR="${0:A:h}"
ROOT_DIR="${SCRIPT_DIR}/.."
cd "${SCRIPT_DIR}"

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

create-dmg "Trndi.dmg" "macos/stage" --volname "Trndi" --format UDZO --icon-size 128 --icon "Trndi.app" 150 200 --app-drop-link 250 200

if [ ! -f "Trndi.dmg" ]; then
  echo "ERROR: Trndi.dmg was not created (create-dmg failed)" >&2
  exit 1
fi

rm -f rw*Trndi*.dmg
rm -rf macos

echo "Created: $(pwd)/Trndi.dmg"
