#!/bin/bash

# Simple sessionizer for Zellij floating panes
set -e

# Get zoxide directories and pipe to fzf
selected=$(zoxide query -l 2>/dev/null | fzf --prompt="Select directory: " --height=60% --layout=reverse --border 2>/dev/null || echo "")

if [[ -n $selected ]]; then
    # Create new tab with selected directory
    zellij action new-tab --cwd "$selected"
fi