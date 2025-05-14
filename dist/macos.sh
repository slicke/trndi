#!/bin/zsh

rm -r macos
mkdir macos

# Make App
cp -r ../Trndi.app macos/.
rm -rf macos/Trndi.app/Contents/MacOS/Trndi
cp ../Trndi macos/Trndi.app/Contents/MacOS/Trndi

# Setup languages
mkdir macos/Trndi.app/Contents/MacOS/lang
cp ../lang/* macos/Trndi.app/Contents/MacOS/lang/.

mkdir macos/Trndi.iconset
sips -z 16 16  ../Trndi.png --out macos/Trndi.iconset/icon_16x16.png
sips -z 24 24  ../Trndi.png --out macos/Trndi.iconset/icon_24x24.png
sips -z 64 64  ../Trndi.png --out macos/Trndi.iconset/icon_64x64.png
sips -z 128 128  ../Trndi.png --out macos/Trndi.iconset/icon_128x128.png
sips -z 256 256  ../Trndi.png --out macos/Trndi.iconset/icon_256x256.png

iconutil -c icns macos/Trndi.iconset

cp macos/Trndi.icns macos/Trndi.app/Contents/Resources/

cat > "macos/Trndi.app/Contents/Info.plist" << 'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
   "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key>
  <string>Trndi</string>
  <key>CFBundleIdentifier</key>
  <string>com.slicke.Trndi</string>
  <key>CFBundleVersion</key>
  <string>1.0</string>
  <key>CFBundleIconFile</key>
  <string>Trndi</string>
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

# DMG
# 1) Skapa staging- och mount-kataloger
mkdir -p macos/stage
cp -R macos/Trndi.app macos/stage/

rm Trndi.dmg
# 2) Kör create-dmg
create-dmg Trndi.dmg "macos/stage" --volname "Trndi" --format UDZO --icon-size 128 --icon "Trndi.app" 150 200 --app-drop-link 250 200 --out "Trndi.dmg"          
rm rw*Trndi*.dmg
# 3) Rensa upp
rm -rf macos