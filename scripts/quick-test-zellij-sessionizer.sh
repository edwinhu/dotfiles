#!/usr/bin/env bash

# Quick test for the enhanced Zellij sessionizer
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SESSIONIZER="$SCRIPT_DIR/zellij-sessionizer-improved.sh"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}=== Quick Zellij Sessionizer Test ===${NC}"

# Test 1: Script exists and is executable
if [[ -x "$SESSIONIZER" ]]; then
    echo -e "${GREEN}✓${NC} Script exists and is executable"
else
    echo -e "${RED}✗${NC} Script not found or not executable"
    exit 1
fi

# Test 2: Help works
if "$SESSIONIZER" --help | grep -q "Enhanced Zellij Sessionizer"; then
    echo -e "${GREEN}✓${NC} Help functionality works"
else
    echo -e "${RED}✗${NC} Help functionality broken"
    exit 1
fi

# Test 3: Dependencies
missing_deps=()
for dep in zellij fzf; do
    if ! command -v "$dep" >/dev/null 2>&1; then
        missing_deps+=("$dep")
    fi
done

if [[ ${#missing_deps[@]} -eq 0 ]]; then
    echo -e "${GREEN}✓${NC} All dependencies available"
else
    echo -e "${RED}✗${NC} Missing dependencies: ${missing_deps[*]}"
    exit 1
fi

# Test 4: Configuration
if grep -q "zellij-sessionizer-improved.sh" "$HOME/.config/zellij/config.kdl" 2>/dev/null; then
    echo -e "${GREEN}✓${NC} Keybinding configured in Zellij"
else
    echo -e "${YELLOW}⚠${NC} Keybinding not found in config (may need manual setup)"
fi

# Test 5: Zoxide integration (optional)
if command -v zoxide >/dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Zoxide available for frecent directories"
else
    echo -e "${YELLOW}⚠${NC} Zoxide not available (frecent directories disabled)"
fi

echo ""
echo -e "${GREEN}Basic tests passed! 🎉${NC}"
echo ""
echo -e "${BLUE}Usage:${NC}"
echo "  Interactive mode: $SESSIONIZER"
echo "  Direct mode:      $SESSIONIZER /path/to/project"
echo ""
echo -e "${BLUE}Keybinding:${NC}"
echo "  In Zellij: Ctrl-s Ctrl-w"
echo ""
echo -e "${BLUE}Manual test:${NC}"
echo "  Try running: $SESSIONIZER --help"
echo "  Or:          $SESSIONIZER ~/dotfiles"