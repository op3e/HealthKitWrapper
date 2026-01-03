#!/bin/bash

# Embed Watch App into Delphi App Bundle and Deploy
# Run this AFTER building in Delphi

set -e

# Default values
XCODE_PROJECT="/Users/alexwallace/Src/HealthKitWrapper/HealthKitWrapper.xcodeproj"
WATCH_TARGET="TestWatchApp"
DELPHI_APP=""
DERIVED_DATA="/Users/alexwallace/Library/Developer/Xcode/DerivedData"
DEVICE_ID=""

# Usage function
usage() {
    echo "Usage: $0 -d <delphi_app_path> [options]"
    echo ""
    echo "Required:"
    echo "  -d, --delphi-app PATH    Path to the Delphi .app bundle"
    echo ""
    echo "Optional:"
    echo "  -p, --project PATH       Xcode project path (default: $XCODE_PROJECT)"
    echo "  -t, --target NAME        Watch target name (default: $WATCH_TARGET)"
    echo "  -D, --derived-data PATH  Derived data path (default: $DERIVED_DATA)"
    echo "  -i, --device-id ID       Device ID for deployment (auto-detected if not specified)"
    echo "  -h, --help               Show this help"
    echo ""
    echo "Example:"
    echo "  $0 -d /Users/me/PAServer/scratch-dir/myproject/MyApp.app"
    echo ""
    exit 1
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--delphi-app)
            DELPHI_APP="$2"
            shift 2
            ;;
        -p|--project)
            XCODE_PROJECT="$2"
            shift 2
            ;;
        -t|--target)
            WATCH_TARGET="$2"
            shift 2
            ;;
        -D|--derived-data)
            DERIVED_DATA="$2"
            shift 2
            ;;
        -i|--device-id)
            DEVICE_ID="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

# Validate required arguments
if [ -z "$DELPHI_APP" ]; then
    echo "ERROR: Delphi app path is required"
    echo ""
    usage
fi

echo "=== Embed Watch App into Delphi App ==="
echo ""
echo "Configuration:"
echo "  Xcode Project: $XCODE_PROJECT"
echo "  Watch Target:  $WATCH_TARGET"
echo "  Delphi App:    $DELPHI_APP"
echo "  Derived Data:  $DERIVED_DATA"

# Step 1: Build the watch app
echo ""
echo "Step 1: Building watch app..."
xcodebuild -project "$XCODE_PROJECT" \
    -scheme "$WATCH_TARGET" \
    -configuration Debug \
    -destination 'generic/platform=watchOS' \
    -derivedDataPath "$DERIVED_DATA" \
    CODE_SIGN_IDENTITY="Apple Development" \
    -quiet

if [ $? -ne 0 ]; then
    echo "ERROR: Failed to build watch app"
    exit 1
fi

# Find the watch app - could be in different locations
WATCH_APP=$(find "$DERIVED_DATA/Build/Products" -name "${WATCH_TARGET}.app" -type d 2>/dev/null | grep -v "\.app/" | head -1)

if [ -z "$WATCH_APP" ]; then
    WATCH_APP="$DERIVED_DATA/Build/Products/Debug-watchos/${WATCH_TARGET}.app"
fi

if [ ! -d "$WATCH_APP" ]; then
    echo "ERROR: Watch app not found at $WATCH_APP"
    exit 1
fi

echo "Watch app built: $WATCH_APP"

# Step 2: Check Delphi app exists
echo ""
echo "Step 2: Checking Delphi app..."

if [ ! -d "$DELPHI_APP" ]; then
    echo "ERROR: Delphi app not found at $DELPHI_APP"
    echo ""
    echo "Make sure to BUILD the Delphi app first:"
    echo "  1. In RAD Studio, Project -> Build (or Shift+F9)"
    echo "  2. Do NOT run/deploy yet"
    echo ""
    exit 1
fi

echo "Delphi app found"

# Step 3: Create Watch folder and copy watch app
echo ""
echo "Step 3: Embedding watch app..."

WATCH_DEST="$DELPHI_APP/Watch"
mkdir -p "$WATCH_DEST"

# Remove old watch app if exists
rm -rf "$WATCH_DEST/${WATCH_TARGET}.app" 2>/dev/null || true

# Copy watch app
cp -R "$WATCH_APP" "$WATCH_DEST/"

echo "Watch app embedded"

# Step 4: Re-sign everything
echo ""
echo "Step 4: Re-signing app bundle..."

# Get the signing identity from the Delphi app
SIGNING_IDENTITY=$(codesign -d -vvv "$DELPHI_APP" 2>&1 | grep "Authority=" | head -1 | sed 's/Authority=//')

if [ -z "$SIGNING_IDENTITY" ]; then
    # Fallback to finding a development identity
    SIGNING_IDENTITY=$(security find-identity -v -p codesigning | grep "Apple Development" | head -1 | sed 's/.*"\(.*\)".*/\1/')
fi

echo "Using identity: $SIGNING_IDENTITY"

# Get entitlements from watch app
WATCH_ENTITLEMENTS=$(mktemp)
codesign -d --entitlements - "$WATCH_APP" --xml > "$WATCH_ENTITLEMENTS" 2>/dev/null || true

# Sign the watch app
codesign --force --sign "$SIGNING_IDENTITY" \
    --entitlements "$WATCH_ENTITLEMENTS" \
    "$WATCH_DEST/${WATCH_TARGET}.app"

rm -f "$WATCH_ENTITLEMENTS"

# Get entitlements from Delphi app and re-sign
DELPHI_ENTITLEMENTS=$(mktemp)
codesign -d --entitlements - "$DELPHI_APP" --xml > "$DELPHI_ENTITLEMENTS" 2>/dev/null || true

codesign --force --sign "$SIGNING_IDENTITY" \
    --entitlements "$DELPHI_ENTITLEMENTS" \
    "$DELPHI_APP"

rm -f "$DELPHI_ENTITLEMENTS"

echo "Signing complete"

# Step 5: Deploy to device
echo ""
echo "Step 5: Deploying to device..."

# Auto-detect device if not specified
if [ -z "$DEVICE_ID" ]; then
    DEVICE_ID=$(xcrun devicectl list devices 2>/dev/null | grep "connected" | grep -E "iPhone|iPad" | head -1 | awk '{print $3}')
fi

if [ -z "$DEVICE_ID" ]; then
    echo "ERROR: No connected iOS device found."
    echo ""
    echo "The app bundle is ready at: $DELPHI_APP"
    echo "You can deploy manually via Xcode's Devices window."
    exit 1
fi

echo "Deploying to device $DEVICE_ID..."
xcrun devicectl device install app --device "$DEVICE_ID" "$DELPHI_APP"

if [ $? -ne 0 ]; then
    echo ""
    echo "ERROR: Deployment failed."
    echo "The app bundle is ready at: $DELPHI_APP"
    echo ""
    echo "You can try manual deployment via Xcode's Devices window."
    exit 1
fi

echo ""
echo "=== Done! ==="
echo "The Delphi app with embedded watch app has been deployed."
echo ""
echo "Next steps:"
echo "  1. Open the Watch app on your iPhone"
echo "  2. Scroll to 'Available Apps'"
echo "  3. Find your app and tap 'Install'"
echo ""
