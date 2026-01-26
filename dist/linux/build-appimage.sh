#!/bin/bash
# Build AppImage for Trndi
# This script creates a portable AppImage package that works across Linux distributions
set -euo pipefail

# Variables
ARCH="${1:-x86_64}"  # x86_64 or aarch64
APP_NAME="Trndi"
APP_DIR="${APP_NAME}.AppDir"
VERSION="${VERSION:-1.0.0}"

echo "Building AppImage for ${APP_NAME} (${ARCH})..."

# Clean previous build
rm -rf "${APP_DIR}"
mkdir -p "${APP_DIR}"

# Create standard AppImage directory structure
mkdir -p "${APP_DIR}/usr/bin"
mkdir -p "${APP_DIR}/usr/share/applications"
mkdir -p "${APP_DIR}/usr/share/icons/hicolor/256x256/apps"
mkdir -p "${APP_DIR}/usr/share/trndi"

# Copy binary
echo "Copying binary..."
cp -v Trndi "${APP_DIR}/usr/bin/Trndi"
chmod +x "${APP_DIR}/usr/bin/Trndi"

# Copy language files if they exist
if [ -d "lang" ]; then
  echo "Copying language files..."
  cp -r lang "${APP_DIR}/usr/share/trndi/"
fi

# Copy GNOME Shell extension if present
if [ -d "gnome-shell-extension/trndi-current" ]; then
  echo "Copying GNOME Shell extension..."
  mkdir -p "${APP_DIR}/usr/share/trndi/gnome-shell-extension"
  cp -r "gnome-shell-extension/trndi-current" \
    "${APP_DIR}/usr/share/trndi/gnome-shell-extension/trndi-current@slicke.com"
fi

# Copy KDE Plasmoid if present
if [ -d "kde-plasmoid/trndi-current" ]; then
  echo "Copying KDE Plasmoid..."
  mkdir -p "${APP_DIR}/usr/share/trndi/kde-plasmoid"
  cp -r "kde-plasmoid/trndi-current" \
    "${APP_DIR}/usr/share/trndi/kde-plasmoid/com.slicke.trndi.current"
fi

# Copy icon
echo "Copying icon..."
if [ -f "Trndi.png" ]; then
  cp -v Trndi.png "${APP_DIR}/usr/share/icons/hicolor/256x256/apps/trndi.png"
  cp -v Trndi.png "${APP_DIR}/trndi.png"  # AppImage also needs icon in root
else
  echo "Warning: Trndi.png not found"
fi

# Create .desktop file
echo "Creating desktop entry..."
cat > "${APP_DIR}/usr/share/applications/trndi.desktop" <<'EOF'
[Desktop Entry]
Name=Trndi
GenericName=CGM Viewer
Comment=Blood glucose monitoring application
Exec=Trndi %U
Icon=trndi
Type=Application
Categories=Utility;
X-GNOME-FullName=Trndi
X-KDE-FullName=Trndi
X-DBUS-ServiceName=com.slicke.trndi
X-DBUS-StartupType=Multi
Keywords=trndi;cgm;glucose;monitoring;diabetes;
EOF

# Symlink .desktop file to AppDir root (required for AppImage)
ln -sf usr/share/applications/trndi.desktop "${APP_DIR}/trndi.desktop"

# Create AppRun script
echo "Creating AppRun launcher..."
cat > "${APP_DIR}/AppRun" <<'EOF'
#!/bin/bash
# AppImage launcher script for Trndi
SELF=$(readlink -f "$0")
HERE=${SELF%/*}

# Export paths for language files and extensions
export TRNDI_SHARE_DIR="${HERE}/usr/share/trndi"

# Launch Trndi
exec "${HERE}/usr/bin/Trndi" "$@"
EOF
chmod +x "${APP_DIR}/AppRun"

# Download appimagetool if not present
APPIMAGETOOL="appimagetool-${ARCH}.AppImage"
if [ ! -f "${APPIMAGETOOL}" ]; then
  echo "Downloading appimagetool..."
  wget -q "https://github.com/AppImage/AppImageKit/releases/download/continuous/${APPIMAGETOOL}"
  chmod +x "${APPIMAGETOOL}"
fi

# Build AppImage
# In CI environments without FUSE, extract and run appimagetool directly
echo "Building AppImage..."
if [ ! -d "squashfs-root" ]; then
  echo "Extracting appimagetool (FUSE not available in CI)..."
  ./"${APPIMAGETOOL}" --appimage-extract >/dev/null 2>&1
  echo "Contents of squashfs-root after extraction:"
  ls -l ./squashfs-root/
fi

# Run appimagetool directly from extracted directory
ARCH="${ARCH}" ./squashfs-root/AppRun "${APP_DIR}" "${APP_NAME}-${ARCH}.AppImage"

# Clean up extracted appimagetool
rm -rf squashfs-root

echo "✓ AppImage created: ${APP_NAME}-${ARCH}.AppImage"
ls -lh "${APP_NAME}-${ARCH}.AppImage"
