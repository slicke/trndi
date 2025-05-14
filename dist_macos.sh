#!/bin/bash

# Trndi macOS Package Builder
# Version: 2.0.0
# Beskrivning: Skapar en DMG-fil med Trndi-appen för macOS-distribution med integrerad versionshantering

## --------------------------
## KONFIGURATIONSBARA VARIABLER
## --------------------------

# Företagsinformation (MÅSTE anpassas)
COMPANY_ID_PREFIX="com.slicke"       # Ersätt med din domän (t.ex. com.eriktornkvist)
COMPANY_NAME="Slicke"     # Ersätt med ditt företagsnamn

# Sökvägar (anpassa efter behov)
APP_NAME="Trndi"
BUILD_DIR="build/macos"
RESOURCES_DIR="resources"
ICON_PATH="${RESOURCES_DIR}/app_icon.icns"  # Standardikon-sökväg
LAZ_PROJECT_FILE="${APP_NAME}.lpi"    # Lazarus-projektfil
VERSION_FILE="version.env"            # Versionsfil som genereras av version_manager.sh

# DMG-inställningar
DMG_BACKGROUND="${RESOURCES_DIR}/dmg_background.png"  # Valfri bakgrundsbild
DMG_SIZE="256M"                      # DMG-filstorlek (kan vara "auto")

## --------------------------
## HUVUDFUNKTIONER
## --------------------------

# Funktion för felhantering
handle_error() {
    echo "Fel: $1" >&2
    echo "Skript avbryts." >&2
    cleanup
    exit 1
}

# Funktion för rensning av temporära filer
cleanup() {
    if [[ -n "$TMP_DIR" && -d "$TMP_DIR" ]]; then
        echo "Rensar temporära filer..."
        rm -rf "$TMP_DIR"
    fi
}

# Funktion för att extrahera XML-värden
get_xml_value() {
    local element="$1"
    local file="$2"
    
    # Försök med xmllint först
    local value=$(xmllint --xpath "string(//$element/@Value)" "$file" 2>/dev/null)
    
    # Fallback till grep om xmllint misslyckas eller inte finns
    if [[ -z "$value" ]]; then
        value=$(grep -oP "<$element Value=\"\K[0-9]+" "$file" 2>/dev/null || echo "0")
    fi
    
    echo "$value"
}

# Funktion för att läsa versionsnummer
read_version() {
    # Generera version.env om den inte finns
    if [[ ! -f "$VERSION_FILE" ]]; then
        echo "Genererar versionsfil..."
        
        # Validera att .lpi-filen finns
        if [[ ! -f "$LAZ_PROJECT_FILE" ]]; then
            handle_error "Lazarus-projektfil $LAZ_PROJECT_FILE saknas"
        fi

        # Extrahera versionskomponenter
        local major=$(get_xml_value "MajorVersionNr" "$LAZ_PROJECT_FILE")
        local minor=$(get_xml_value "MinorVersionNr" "$LAZ_PROJECT_FILE")
        local revision=$(get_xml_value "RevisionNr" "$LAZ_PROJECT_FILE")
        local build=$(get_xml_value "BuildNr" "$LAZ_PROJECT_FILE")

        # Skapa versionssträng (format: Major.Minor.Revision.Build)
        local binary_version="${major:-0}.${minor:-0}.${revision:-0}.${build:-0}"

        # Validera versionsformat
        if [[ ! "$binary_version" =~ ^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
            handle_error "Ogiltigt versionsformat: $binary_version"
        fi

        # Skriv till versionsfil
        echo "BINARY_VERSION=$binary_version" > "$VERSION_FILE"
        echo "PROJECT_VERSION=$binary_version" >> "$VERSION_FILE"
    fi

    # Läs version från fil
    source "$VERSION_FILE"
    
    # Använd BINARY_VERSION om den finns, annars PROJECT_VERSION
    local version="${BINARY_VERSION:-$PROJECT_VERSION}"
    
    if [[ -z "$version" ]]; then
        handle_error "Kunde inte läsa versionsnummer från $VERSION_FILE"
    fi

    echo "$version"
}

# Funktion för att skapa applikationsmapp
create_app_bundle() {
    local version="$1"
    local bundle_path="$TMP_DIR/$APP_NAME.app"

    echo "Skapar applikationsbundle för version $version..."

    # Skappar katalogstruktur
    mkdir -p "$bundle_path/Contents/MacOS"
    mkdir -p "$bundle_path/Contents/Resources"

    # Extrahera versionskomponenter för Info.plist
    IFS='.' read -ra ver_components <<< "$version"
    local short_version="${ver_components[0]}.${ver_components[1]}.${ver_components[2]}"

    # Skappar Info.plist
    cat > "$bundle_path/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleExecutable</key>
    <string>$APP_NAME</string>
    <key>CFBundleIdentifier</key>
    <string>$COMPANY_ID_PREFIX.$APP_NAME</string>
    <key>CFBundleName</key>
    <string>$APP_NAME</string>
    <key>CFBundleVersion</key>
    <string>$version</string>
    <key>CFBundleShortVersionString</key>
    <string>$short_version</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleSignature</key>
    <string>????</string>
    <key>CFBundleIconFile</key>
    <string>app_icon.icns</string>
    <key>LSMinimumSystemVersion</key>
    <string>10.13</string>
    <key>NSHumanReadableCopyright</key>
    <string>© $(date +%Y) $COMPANY_NAME. Alla rättigheter förbehållna.</string>
</dict>
</plist>
EOF

    # Kopierar ikon om den finns
    if [[ -f "$ICON_PATH" ]]; then
        cp "$ICON_PATH" "$bundle_path/Contents/Resources/app_icon.icns"
    else
        echo "Varning: Applikationsikon saknas ($ICON_PATH)"
    fi

    # Kopierar binärfil
    if [[ -f "$BUILD_DIR/$APP_NAME" ]]; then
        cp "$BUILD_DIR/$APP_NAME" "$bundle_path/Contents/MacOS/"
        chmod +x "$bundle_path/Contents/MacOS/$APP_NAME"
    else
        handle_error "Binärfil $BUILD_DIR/$APP_NAME saknas"
    fi
}

# Funktion för att skapa DMG-fil
create_dmg() {
    local version="$1"
    local volume_name="${APP_NAME}-${version}"
    local dmg_name="${APP_NAME}-${version}-macos.dmg"
    local temp_dmg="$TMP_DIR/temp.dmg"

    echo "Skapar DMG-fil för version $version..."

    # Beräkna storlek automatiskt om DMG_SIZE är "auto"
    if [[ "$DMG_SIZE" == "auto" ]]; then
        local app_size=$(du -sm "$TMP_DIR/$APP_NAME.app" | cut -f1)
        DMG_SIZE=$(($app_size + 20))M  # Lägg till lite marginal
        echo "Automatisk storleksberäkning: $DMG_SIZE"
    fi

    # Skapar tom DMG
    hdiutil create -size "$DMG_SIZE" -fs HFS+ -volname "$volume_name" -ov "$temp_dmg" || 
        handle_error "Kunde inte skapa DMG-fil"

    # Monterar DMG
    local device=$(hdiutil attach -nobrowse -noautoopen "$temp_dmg" | awk 'NR==1{print $1}')
    local mount_point=$(hdiutil info | grep "$device" | awk '{print $3}')

    # Kopierar app till DMG
    cp -R "$TMP_DIR/$APP_NAME.app" "$mount_point/"

    # Konfigurerar DMG-layout (valfritt)
    if [[ -f "$DMG_BACKGROUND" ]]; then
        echo "Konfigurerar DMG-layout med bakgrundsbild..."

        # Skapar .background-katalog och kopierar bakgrundsbild
        mkdir -p "$mount_point/.background"
        cp "$DMG_BACKGROUND" "$mount_point/.background/background.png"

        # Använder AppleScript för att konfigurera vyn
        echo '
        tell application "Finder"
            tell disk "'$volume_name'"
                open
                set current view of container window to icon view
                set toolbar visible of container window to false
                set statusbar visible of container window to false
                set the bounds of container window to {400, 100, 900, 400}
                set viewOptions to the icon view options of container window
                set arrangement of viewOptions to not arranged
                set icon size of viewOptions to 72
                set background picture of viewOptions to file ".background:background.png"
                set position of item "'$APP_NAME.app'" of container window to {150, 100}
                update without registering applications
                close
            end tell
        end tell
        ' | osascript
    fi

    # Skapar Applications-länk
    ln -s /Applications "$mount_point/Applications"

    # Avmonterar DMG
    hdiutil detach "$device"

    # Konverterar till komprimerad DMG
    echo "Slutför DMG-skapande..."
    hdiutil convert "$temp_dmg" -format UDZO -imagekey zlib-level=9 -ov -o "$PWD/$dmg_name" || 
        handle_error "Kunde inte konvertera DMG till slutligt format"

    # Kontrollerar den skapade filen
    if [[ ! -f "$PWD/$dmg_name" ]]; then
        handle_error "DMG-filen skapades inte som förväntat"
    fi

    echo "DMG skapad: $PWD/$dmg_name"
}

## --------------------------
## HUVUDPROGRAM
## --------------------------

# Kopiera språkfiler till Resources-mappen
LANG_DIR="build/macos/Trndi.app/Contents/Resources/lang"
mkdir -p "$LANG_DIR"
cp -R lang/*.mo "$LANG_DIR/"

# Verifiera att filerna kopierats
echo "Verifierar språkfiler:"
ls -la "$LANG_DIR"

# Skapar temporär katalog
TMP_DIR=$(mktemp -d /tmp/${APP_NAME}_package.XXXXXX) || 
    handle_error "Kunde inte skapa temporär katalog"

# Hämta version
VERSION=$(read_version) || exit 1

if [[ -f "./Trndi.png" && ! -f "$ICON_PATH" ]]; then
    echo "Konverterar Trndi.png till app_icon.icns..."
    mkdir -p "$RESOURCES_DIR"
    sips -s format icns "./Trndi.png" --out "$ICON_PATH" || 
        echo "Varning: Kunde inte konvertera ikonfilen. Standardikon kommer användas."
fi

if [[ ! -f "$BUILD_DIR/$APP_NAME" && -f "./$APP_NAME" ]]; then
    echo "Kopierar binärfil från ./$APP_NAME till build-katalogen..."
    mkdir -p "$BUILD_DIR"
    cp "./$APP_NAME" "$BUILD_DIR/$APP_NAME" || 
        handle_error "Kunde inte kopiera binärfilen"
fi

# Skapa app bundle
create_app_bundle "$VERSION"

# Skapa DMG
#create_dmg "$VERSION"

# Rensa upp
#cleanup

echo "Paketering av $APP_NAME version $VERSION slutförd!"
