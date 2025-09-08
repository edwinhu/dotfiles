#!/usr/bin/env bash

# Enhanced Zellij Sessionizer
# Integrates zoxide, existing sessions, and project directories
# Handles session switching using detach+attach method for proper Zellij session switching

set -euo pipefail

# Configuration
LOG_FILE="$HOME/.cache/zellij-sessionizer.log"
CACHE_DIR="$HOME/.cache"
SESSION_CACHE="$CACHE_DIR/zellij-sessions.cache"

# Ensure directories exist
mkdir -p "$(dirname "$LOG_FILE")"
mkdir -p "$CACHE_DIR"

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
)

# Directories mentioned in documentation
ADDITIONAL_DIRS=(
    "$HOME/.projects"
    "$HOME/.areas"
)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" >> "$LOG_FILE"
    
    # Also print to stderr for debugging when in terminal
    if [[ "${DEBUG:-}" == "1" ]]; then
        echo -e "${BLUE}[$timestamp] [$level] $message${NC}" >&2
    fi
}

# Error handling
error_exit() {
    log "ERROR" "$1"
    echo -e "${RED}Error: $1${NC}" >&2
    exit 1
}

# Success message
success() {
    log "INFO" "$1"
    echo -e "${GREEN}✓ $1${NC}" >&2
}

# Warning message
warn() {
    log "WARN" "$1"
    echo -e "${YELLOW}⚠ $1${NC}" >&2
}

# Check dependencies
check_dependencies() {
    local deps=("zellij" "fzf")
    for dep in "${deps[@]}"; do
        if ! command -v "$dep" >/dev/null 2>&1; then
            error_exit "Required dependency '$dep' not found in PATH"
        fi
    done
    
    # Check if zoxide is available (optional)
    if command -v zoxide >/dev/null 2>&1; then
        log "INFO" "zoxide found - frecent directories will be included"
        return 0
    else
        log "WARN" "zoxide not found - only scanning project directories"
        return 1
    fi
}

# Get existing Zellij sessions
get_existing_sessions() {
    if ! command -v zellij >/dev/null 2>&1; then
        log "ERROR" "zellij command not found"
        return 1
    fi
    
    # Use --short flag for cleaner session name extraction
    zellij list-sessions --short 2>/dev/null | sort || true
}

# Check if a name is an existing session (helper for preview)
is_existing_session() {
    local name="$1"
    get_existing_sessions | grep -q "^${name}$"
}

# Get zoxide directories (frecent directories)
get_zoxide_dirs() {
    if command -v zoxide >/dev/null 2>&1; then
        # Get directories from zoxide, filtering out temp/system directories and limiting to 10
        zoxide query -l 2>/dev/null | while read -r dir; do
            # Skip if directory doesn't exist
            [[ ! -d "$dir" ]] && continue
            
            # Filter out temp and system directories
            case "$dir" in
                /tmp | /tmp/* | /var/tmp | /var/tmp/* | /private/tmp | /private/tmp/* | \
                /System | /System/* | /Library | /Library/* | /usr | /usr/* | \
                /bin | /bin/* | /sbin | /sbin/* | /Applications | /Applications/* | \
                /Volumes | /Volumes/* | /dev | /dev/* | /proc | /proc/* | \
                *.Trash* | */.Trash* | */Trash/* )
                    log "DEBUG" "Filtering out system/temp directory: $dir"
                    continue
                    ;;
            esac
            
            echo "$dir"
        done | head -10  # Limit to top 10 frecent directories after filtering
    fi
}

# Find project directories (prioritize main project directories, not every subdirectory)
find_projects() {
    local all_dirs=()
    
    # First add the main project directories themselves
    for dir in "${PROJECT_DIRS[@]}"; do
        if [[ -d "$dir" ]]; then
            all_dirs+=("$dir")
        fi
    done
    
    # Then find immediate subdirectories in project directories (depth 1 only)
    for dir in "${PROJECT_DIRS[@]}" "${ADDITIONAL_DIRS[@]}"; do
        if [[ -d "$dir" ]]; then
            log "DEBUG" "Scanning main projects in: $dir"
            while IFS= read -r -d '' project_dir; do
                # Only add if it looks like a main project (has common project files)
                if [[ -f "$project_dir/.git/config" ]] || [[ -f "$project_dir/Cargo.toml" ]] || [[ -f "$project_dir/package.json" ]] || [[ -f "$project_dir/pyproject.toml" ]] || [[ -f "$project_dir/requirements.txt" ]] || [[ -f "$project_dir/Makefile" ]] || [[ -f "$project_dir/README.md" ]] || [[ -f "$project_dir/pixi.toml" ]]; then
                    all_dirs+=("$project_dir")
                    log "DEBUG" "Added project directory: $project_dir"
                else
                    log "DEBUG" "Skipped non-project directory: $project_dir"
                fi
            done < <(find "$dir" -mindepth 1 -maxdepth 1 -type d -print0 2>/dev/null)
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

# Get all available directories (zoxide first for frecency, then projects) excluding those with matching session names
get_all_directories() {
    local existing_sessions
    existing_sessions=$(get_existing_sessions)
    
    local seen_dirs=()
    local result_dirs=()
    local max_zoxide_dirs=8  # Reserve slots for project directories
    local max_total_dirs=15  # Increase total limit for better coverage
    
    # Helper function to check if directory already seen
    is_seen() {
        local dir="$1"
        local seen_dir
        for seen_dir in "${seen_dirs[@]}"; do
            if [[ "$seen_dir" == "$dir" ]]; then
                return 0
            fi
        done
        return 1
    }
    
    # Helper function to check and add directory
    check_and_add_dir() {
        local dir="$1"
        local dirname
        dirname=$(basename "$dir")
        
        # Stop if we've reached the total limit
        if [[ ${#result_dirs[@]} -ge $max_total_dirs ]]; then
            log "DEBUG" "Reached total directory limit ($max_total_dirs), stopping"
            return 1
        fi
        
        # Skip if already seen (avoid duplicates)
        if is_seen "$dir"; then
            log "DEBUG" "Skipping duplicate directory: $dir"
            return
        fi
        
        # Skip if a session exists with the same name as the directory basename
        if echo "$existing_sessions" | grep -q "^${dirname}$"; then
            log "DEBUG" "Skipping directory $dir - session '$dirname' already exists"
            seen_dirs+=("$dir")  # Mark as seen to avoid duplicate checks
            return
        fi
        
        log "DEBUG" "Adding directory: $dir ($((${#result_dirs[@]} + 1))/$max_total_dirs)"
        seen_dirs+=("$dir")
        result_dirs+=("$dir")
    }
    
    # First, add zoxide directories in frecency order (but limit them to leave room for projects)
    if command -v zoxide >/dev/null 2>&1; then
        log "DEBUG" "Processing zoxide directories in frecency order (max $max_zoxide_dirs)..."
        local zoxide_count=0
        while IFS= read -r dir; do
            if [[ -d "$dir" ]]; then
                if [[ $zoxide_count -ge $max_zoxide_dirs ]]; then
                    log "DEBUG" "Reached zoxide limit ($max_zoxide_dirs), stopping zoxide processing"
                    break
                fi
                check_and_add_dir "$dir" && ((zoxide_count++))
            fi
        done < <(get_zoxide_dirs)
    fi
    
    # Always try to add project directories (these are important)
    log "DEBUG" "Processing project directories (remaining slots: $((max_total_dirs - ${#result_dirs[@]})))..."
    while IFS= read -r dir; do
        if [[ -n "$dir" ]]; then
            check_and_add_dir "$dir" || break  # Stop if total limit reached
        fi
    done < <(find_projects)
    
    log "DEBUG" "Final directory count: ${#result_dirs[@]}"
    # Output results in order (zoxide first, then projects)
    printf '%s\n' "${result_dirs[@]}"
}

# Create session name from directory path
create_session_name() {
    local dir="$1"
    # Use basename and replace dots/spaces with underscores
    # Then replace any remaining special characters with underscores
    basename "$dir" | tr '. ' '__' | sed 's/[^[:alnum:]_-]/_/g'
}

# Check if session exists (use the helper function for consistency)
session_exists() {
    local session_name="$1"
    is_existing_session "$session_name"
}

# Switch to existing session using proper detach+attach method
switch_to_session() {
    local session_name="$1"
    
    log "INFO" "Switching to existing session: $session_name"
    
    if [[ "${DRY_RUN:-}" == "1" ]]; then
        log "INFO" "DRY_RUN mode: Would switch to existing session: $session_name"
        success "Would switch to session: $session_name"
        return
    fi
    
    if [[ -n "${ZELLIJ:-}" ]]; then
        # We're inside Zellij - must detach first, then attach to target
        log "DEBUG" "Inside Zellij session, using detach+attach method for: $session_name"
        
        # Show user feedback
        echo -e "\n${GREEN}✓ Switching to existing session: $session_name${NC}"
        echo -e "${BLUE}🔄 Detaching from current session and attaching to target...${NC}\n"
        
        # Detach from current session first
        log "DEBUG" "Detaching from current session"
        zellij action detach
        
        # Small delay to ensure detach completes
        sleep 0.2
        
        # Now attach to target session
        log "DEBUG" "Attaching to target session: $session_name"
        exec zellij attach "$session_name"
    else
        # Not in Zellij, simple attach
        log "DEBUG" "Not in Zellij session, attaching to: $session_name"
        exec zellij attach "$session_name"
    fi
}

# Create new session for directory
create_session() {
    local dir="$1"
    local session_name="$2"
    
    if [[ ! -d "$dir" ]]; then
        error_exit "Directory does not exist: $dir"
    fi
    
    log "INFO" "Creating new session: $session_name for directory: $dir"
    log "DEBUG" "Current directory before session creation: $(pwd)"
    
    if [[ -n "${ZELLIJ:-}" ]]; then
        # Inside Zellij - create new session and switch using detach+attach method
        log "DEBUG" "Creating new session and switching: $session_name"
        
        # Create detached session with working directory set
        (cd "$dir" && zellij attach --create-background "$session_name") || error_exit "Failed to create detached session"
        
        log "DEBUG" "Detached session created: $session_name"
        
        if [[ "${DRY_RUN:-}" == "1" ]]; then
            log "INFO" "DRY_RUN mode: Would switch to new session $session_name"
            success "Session created: $session_name in directory: $dir"
        else
            # Notify user and switch to the new session
            echo -e "\n${GREEN}✓ Created new session: $session_name${NC}"
            echo -e "${BLUE}📌 Directory: $dir${NC}"
            echo -e "${BLUE}🔄 Detaching and switching to new session...${NC}\n"
            
            # Detach from current session first
            log "DEBUG" "Detaching from current session before switching to new session"
            zellij action detach
            
            # Small delay to ensure detach completes
            sleep 0.2
            
            # Now attach to the newly created session
            log "DEBUG" "Attaching to new session: $session_name"
            exec zellij attach "$session_name"
        fi
    else
        # Not in Zellij - create and attach directly
        log "DEBUG" "Creating and attaching to session: $session_name"
        log "DEBUG" "Changing to directory: $dir"
        
        # Change to directory and start session
        cd "$dir" || error_exit "Cannot change to directory: $dir"
        log "DEBUG" "Changed to directory: $(pwd)"
        
        if [[ "${DRY_RUN:-}" == "1" ]]; then
            log "INFO" "DRY_RUN mode: Would exec 'zellij attach --create $session_name'"
            success "Session would be created: $session_name in directory: $dir"
        else
            # Use zellij --session for new session creation with working directory
            exec zellij --session "$session_name"
        fi
    fi
}

# Main selection interface
select_target() {
    log "INFO" "Starting target selection interface"
    
    local existing_sessions
    local all_dirs
    
    # Cache data to improve performance
    log "DEBUG" "Gathering existing sessions..."
    existing_sessions=$(get_existing_sessions)
    log "DEBUG" "Found existing sessions: $existing_sessions"
    
    log "DEBUG" "Gathering directories..."
    all_dirs=$(get_all_directories)
    log "DEBUG" "Found directories (first 10):"
    echo "$all_dirs" | head -10 | while IFS= read -r dir; do
        log "DEBUG" "  $dir"
    done
    
    # Create selection list with counts for better UX
    local session_count
    local dir_count
    session_count=$(echo "$existing_sessions" | wc -l | tr -d ' ')
    dir_count=$(echo "$all_dirs" | wc -l | tr -d ' ')
    
    # Adjust counts for empty results
    [[ -z "$existing_sessions" ]] && session_count=0
    [[ -z "$all_dirs" ]] && dir_count=0
    
    {
        if [[ -n "$existing_sessions" ]]; then
            echo "=== EXISTING SESSIONS ($session_count) ==="
            echo "$existing_sessions"
        fi
        echo "=== DIRECTORIES ($dir_count) ==="
        echo "$all_dirs"
    } | fzf \
        --prompt="Select session/directory: " \
        --layout=reverse \
        --border=rounded \
        --info=inline \
        --header="🔍 Sessions: Switch | Directories: New Session | ↑↓: navigate, Enter: select, Esc: cancel" \
        --preview='
            if [[ {} == "=== EXISTING SESSIONS ===" ]] || [[ {} == "=== DIRECTORIES ===" ]]; then
                echo "Section header - navigate to items below"
            elif zellij list-sessions 2>/dev/null | sed "s/\x1b\[[0-9;]*m//g" | grep -q "^{} "; then
                echo "🏃 EXISTING SESSION: {}"
                echo ""
                echo "This will switch to the existing Zellij session."
                echo "Session info:"
                zellij list-sessions 2>/dev/null | sed "s/\x1b\[[0-9;]*m//g" | grep "^{} "
            else
                echo "📁 DIRECTORY: {}"
                echo ""
                echo "This will create a new Zellij session for this directory."
                if [[ -d {} ]]; then
                    echo ""
                    echo "Session name: $(basename {} | tr ". " "__" | tr -cd "[:alnum:]_-")"
                    echo ""
                    echo "Directory contents:"
                    ls -la {} 2>/dev/null | head -10
                else
                    echo ""
                    echo "⚠️  Directory does not exist"
                fi
            fi
        ' \
        --preview-window=right:45% \
        --bind='ctrl-c:abort' \
        --bind='esc:abort' | grep -v "=== "
}

# Show help
show_help() {
    cat << EOF
Enhanced Zellij Sessionizer

USAGE:
    $(basename "$0") [TARGET]

ARGUMENTS:
    TARGET    Directory path or session name (optional)

DESCRIPTION:
    Interactive session manager for Zellij that integrates:
    - Existing Zellij sessions
    - Project directories from ~/projects, ~/dotfiles, etc.
    - Frecent directories from zoxide (if available)
    
    When run without arguments, opens an interactive fzf selection menu.
    When run with a directory path, creates a new session for that directory.
    When run with an existing session name, switches to that session.
    
    Note: When inside Zellij, session switching uses the detach+attach method
    which properly switches sessions without opening floating panes.

KEYBINDINGS (when in Zellij):
    Ctrl-s Ctrl-w    Launch sessionizer

EXAMPLES:
    $(basename "$0")                    # Interactive mode
    $(basename "$0") ~/my-project       # Create session for directory
    $(basename "$0") my-session         # Switch to existing session

ENVIRONMENT:
    DEBUG=1         Enable debug logging
    
LOG FILE:
    ~/.cache/zellij-sessionizer.log
EOF
}

# Main function
main() {
    # Handle help and special arguments
    case "${1:-}" in
        -h|--help|help)
            show_help
            exit 0
            ;;
    esac
    
    log "INFO" "=== Starting Zellij Sessionizer ==="
    log "DEBUG" "Arguments: $*"
    log "DEBUG" "ZELLIJ environment: ${ZELLIJ:-'not set'}"
    
    # Check dependencies
    local has_zoxide
    if check_dependencies; then
        has_zoxide=1
    else
        has_zoxide=0
    fi
    
    local selected
    
    if [[ $# -eq 1 ]]; then
        selected="$1"
        log "INFO" "Target provided as argument: $selected"
    else
        log "DEBUG" "Starting interactive selection..."
        selected=$(select_target)
        if [[ -z "$selected" ]]; then
            log "INFO" "Selection cancelled by user"
            exit 0
        fi
    fi
    
    log "INFO" "Selected target: $selected"
    
    # Check if selected item is an existing session
    if session_exists "$selected"; then
        # Double-check that the session still exists (avoid race conditions)
        if get_existing_sessions | grep -q "^${selected}$"; then
            switch_to_session "$selected"
        else
            warn "Session '$selected' no longer exists. Creating new session instead."
            # Treat as a directory and try to create new session
            if [[ -d "$selected" ]]; then
                local session_name
                session_name=$(create_session_name "$selected")
                create_session "$selected" "$session_name"
            else
                error_exit "Selected session '$selected' no longer exists and no matching directory found"
            fi
        fi
    else
        # It's a directory - create new session
        if [[ ! -d "$selected" ]]; then
            error_exit "Selected path is not a directory: $selected"
        fi
        
        local session_name
        session_name=$(create_session_name "$selected")
        
        log "DEBUG" "Generated session name: $session_name for directory: $selected"
        
        # Check if session name already exists
        if session_exists "$session_name"; then
            log "INFO" "Session with name $session_name already exists, switching to it"
            switch_to_session "$session_name"
        else
            create_session "$selected" "$session_name"
        fi
    fi
    
    log "INFO" "=== Sessionizer completed ==="
}

# Handle script termination (cleanup no longer needed with detach+attach method)
cleanup() {
    log "DEBUG" "Script termination cleanup"
}

trap cleanup EXIT

# Run main function
main "$@"