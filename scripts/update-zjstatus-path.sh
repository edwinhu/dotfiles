#!/usr/bin/env bash

# Script to update the zjstatus path in the zellij layout after nix rebuild

set -e

LAYOUT_FILE="$HOME/dotfiles/.config/zellij/layouts/default.kdl"
ZJSTATUS_WASM=""

# Find zjstatus.wasm in the nix store
echo "Looking for zjstatus.wasm in nix store..."

# First try to find it via the `which` command
if command -v zjstatus >/dev/null 2>&1; then
    ZJSTATUS_DIR=$(dirname "$(which zjstatus)")
    if [[ -f "$ZJSTATUS_DIR/zjstatus.wasm" ]]; then
        ZJSTATUS_WASM="$ZJSTATUS_DIR/zjstatus.wasm"
    fi
fi

# If not found via which, search the nix store directly
if [[ -z "$ZJSTATUS_WASM" ]]; then
    ZJSTATUS_WASM=$(find /nix/store -name "zjstatus.wasm" 2>/dev/null | head -1)
fi

if [[ -z "$ZJSTATUS_WASM" ]]; then
    echo "❌ zjstatus.wasm not found. Make sure to rebuild nix first:"
    echo "   cd ~/nix && nix run .#build-switch"
    exit 1
fi

echo "✅ Found zjstatus.wasm at: $ZJSTATUS_WASM"

# Update the layout file
if grep -q "ZJSTATUS_PATH" "$LAYOUT_FILE"; then
    echo "🔄 Updating layout file..."
    sed -i.bak "s|file:ZJSTATUS_PATH|file:$ZJSTATUS_WASM|g" "$LAYOUT_FILE"
    echo "✅ Layout file updated successfully!"
    echo "   Old version backed up as: ${LAYOUT_FILE}.bak"
else
    echo "ℹ️  Layout file already updated or doesn't contain placeholder"
fi

# Verify the change
if grep -q "file:$ZJSTATUS_WASM" "$LAYOUT_FILE"; then
    echo "🎉 zjstatus is now configured and ready to use!"
    
    # Update the config to use the default layout
    CONFIG_FILE="$HOME/dotfiles/.config/zellij/config.kdl"
    if grep -q 'default_layout "fallback"' "$CONFIG_FILE"; then
        echo "🔄 Switching to zjstatus layout..."
        sed -i.bak 's/default_layout "fallback"/default_layout "default"/' "$CONFIG_FILE"
        echo "✅ Config updated to use zjstatus layout!"
    fi
    
    echo "   Start zellij to see the beautiful status bar"
else
    echo "⚠️  Something went wrong with the update"
    exit 1
fi