#!/usr/bin/env bash

# Zellij Sessionizer - similar to tmux-sessionizer
# Quickly switch between project directories using fzf

set -e

# Default directories to search for projects
PROJECT_DIRS=(
    "$HOME/projects"
    "$HOME/work" 
    "$HOME/dotfiles"
    "$HOME/.config"
    "$HOME/Documents"
    "$HOME/Documents/Notes"
)

# Additional single directories
SINGLE_DIRS=(
    "$HOME"
    "/tmp"
)

# Function to find all project directories
find_projects() {
    local all_dirs=()
    
    # Find subdirectories in project directories
    for dir in "${PROJECT_DIRS[@]}"; do
        if [[ -d "$dir" ]]; then
            while IFS= read -r -d '' project_dir; do
                all_dirs+=("$project_dir")
            done < <(find "$dir" -mindepth 1 -maxdepth 2 -type d -print0 2>/dev/null)
        fi
    done
    
    # Add single directories
    for dir in "${SINGLE_DIRS[@]}"; do
        if [[ -d "$dir" ]]; then
            all_dirs+=("$dir")
        fi
    done
    
    # Remove duplicates and sort
    printf '%s\n' "${all_dirs[@]}" | sort -u
}

# Function to get existing sessions
get_existing_sessions() {
    if command -v zellij >/dev/null 2>&1; then
        zellij list-sessions 2>/dev/null | grep -E "^[a-zA-Z0-9_-]+" | awk '{print $1}' | sort
    fi
}

# Get selected directory or session
if [[ $# -eq 1 ]]; then
    selected=$1
else
    # Combine existing sessions and new project directories
    {
        echo "=== EXISTING SESSIONS ==="
        get_existing_sessions
        echo "=== NEW PROJECTS ==="
        find_projects
    } | fzf --prompt="Select session/project: " --height=60% --layout=reverse --border --preview='
        if [[ {} == "=== EXISTING SESSIONS ===" ]] || [[ {} == "=== NEW PROJECTS ===" ]]; then
            echo "Header"
        elif zellij list-sessions 2>/dev/null | grep -q "^{}"; then
            echo "Existing session: {}"
            echo "Press Enter to switch to this session"
        else
            echo "New project: {}"
            echo "Press Enter to create session for this directory"
            [[ -d {} ]] && ls -la {}
        fi
    ' --preview-window=right:40% | grep -v "=== "
fi

if [[ -z $selected ]]; then
    exit 0
fi

# Check if selected item is an existing session or a new project
if zellij list-sessions 2>/dev/null | grep -q "^$selected "; then
    # It's an existing session
    selected_name="$selected"
    echo "Switching to existing session: $selected_name"
else
    # It's a new project directory
    selected_name=$(basename "$selected" | tr . _)
    echo "Creating new session: $selected_name for $selected"
fi

# Check if we're already in a zellij session
if [[ -z $ZELLIJ ]]; then
    # Not in zellij, start or attach to session
    if zellij list-sessions 2>/dev/null | grep -q "^$selected_name "; then
        # Session exists, attach to it
        exec zellij attach "$selected_name"
    else
        # Session doesn't exist, create it
        cd "$selected" 2>/dev/null || { echo "Directory $selected not found"; exit 1; }
        exec zellij --session "$selected_name"
    fi
else
    # Already in zellij, try to switch to session
    if zellij list-sessions | grep -q "^$selected_name "; then
        # Session exists, try to switch
        zellij action switch-session "$selected_name" 2>/dev/null || {
            echo "Could not switch to session: $selected_name"
            echo "You may need to detach and reattach manually"
            exit 1
        }
    else
        # Session doesn't exist, create new tab or session
        if [[ -d "$selected" ]]; then
            zellij action new-tab --layout default --name "$selected_name" --cwd "$selected"
            sleep 0.1  # Small delay to ensure tab is created
            zellij action go-to-tab-name "$selected_name"
        else
            echo "Directory $selected not found"
            exit 1
        fi
    fi
fi