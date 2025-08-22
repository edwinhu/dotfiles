# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a dotfiles repository for a macOS development environment. The configuration follows a modular approach with settings organized by tool, using symlinks from the dotfiles directory to the home directory.

## Key Commands

### System Package Management

- **Rebuild system configuration**: `cd ~/nix && nix run .#build-switch`
- System packages are managed via **nix-darwin** - always make system-level changes through the nix configuration

### Development Tools

- **Search files**: Use `rg` (ripgrep) instead of grep
- **Find files**: Use `fd` instead of find
- **GitHub operations**: Use `gh` CLI
- **Directory navigation**: `cd` is aliased to `zoxide` for smart navigation

### Important Aliases

- `yolo`: Claude AI with bypass permissions
- `y`: Launch yazi file manager
- `search`: Ripgrep excluding node_modules
- `lg`: LazyGit

## Architecture & Structure

### Configuration Organization

```
dotfiles/
├── .shell_common        # Core shell config (sourced by bash/zsh)
├── .shell_aliases       # Command aliases and functions
├── .shell_env          # Environment variables and PATH
├── .config/            # XDG-compliant configurations
│   ├── ghostty/        # Primary terminal (GPU-accelerated)
│   ├── nvim/           # Neovim (LazyVim-based)
│   └── wezterm/        # Alternative terminal
└── .doom.d/            # Doom Emacs configuration
```

### Key Design Patterns

1. **Modular shell configuration**: Separate files for env, aliases, and common settings
2. **No installation scripts**: Manual symlink approach for explicit control
3. **XDG compliance**: Configurations in `.config/` directory
4. **Consistent theming**: Catppuccin Mocha color scheme throughout

### Nix-Managed Configurations

- **Sketchybar**: Fully managed by nix-darwin in `~/nix/modules/darwin/sketchybar/`
- **Git**: Configuration managed by nix home-manager
- **SSH**: Configuration managed by nix home-manager
- **tmux**: Configuration managed by nix home-manager
- **Starship**: Prompt configuration managed by nix home-manager
- **Zoxide**: Shell navigation managed by nix home-manager

## Important Context

### Package Management

- **Python/R projects**: Use `pixi` - always check `pixi.toml` before suggesting package installations
- **System packages**: Use nix-darwin - never use homebrew or system package managers directly

### Terminal Configuration

- **Leader key**: Ctrl+S in Ghostty
- **Special keybinding**: Shift+Enter for newlines in Claude Code
- Default editor: `nvim`

### Shell Environment

- Sources nix-darwin profiles for package availability
- Works with both bash and zsh
- Custom PATH additions for user-specific directories remain in `.shell_env`

### Marimo Notebooks

- When working with marimo notebooks, check the `__marimo__` folder for the `.ipynb` file with the same filename
- These `.ipynb` files contain the inputs/outputs for debugging purposes and any relevant images

### Claude Code Integration

- **Screenshots**: Always resize screenshots to below 2000 pixels before uploading to Claude to avoid API errors
- When taking screenshots for debugging/verification, use tools like `convert` or `sips` to resize: `sips -Z 1900 screenshot.png`

### Screenshots and Screen Capture

- **Emacs window screenshots**: Use the window ID approach for precise captures:
  ```bash
  # Method 1: Get window ID and capture
  screencapture -l $(osascript -e 'tell application "Emacs" to get the id of the front window') screenshot.png
  
  # Method 2: Activate Emacs first, then capture window
  osascript -e 'tell application "Emacs" to activate'
  screencapture -w screenshot.png
  ```
- The window ID method (`-l`) is more reliable for automated screenshots
- The window selection method (`-w`) is better for interactive use

## Emacs Configuration Protocol (CRITICAL)

When configuring Emacs/Doom, follow this **mandatory workflow** in order:

### 1. ⚠️ ALWAYS Check for Lisp Errors

```bash
# Check ALL modified files for syntax errors
cd ~/.doom.d
emacs --batch --eval "(progn
  (dolist (file '(\"config.el\" \"other-file.el\"))
    (condition-case err
        (progn (check-parens) (message \"✓ %s: syntax OK\" file))
      (error (message \"✗ %s: %s\" file err)))))"
```

**Never skip this step** - syntax errors will break the entire Doom config.

### 2. ⚠️ ALWAYS Test Lisp Commands

```bash
# Test individual modules load correctly
emacs --batch --eval "(progn
  (add-to-list 'load-path \"~/.doom.d/\")
  (require 'your-module)
  (message \"✓ Module loaded successfully\"))"
```

### 3. ⚠️ ALWAYS Test with Full Doom Config

- **Cannot use batch mode** for full Doom testing (macros like `after!`, `map!` not available)
- Create test functions in live Emacs session
- Test actual user workflows (C-c C-c, keybindings, etc.)
- Verify advice installation and function overrides work correctly

### 4. ⚠️ ALWAYS Implement File-Based Logging

```elisp
;; Add to all new Emacs modules
(defvar module-debug-log-file (expand-file-name "module-debug.log" "~/"))

(defun module-debug-log (level format-string &rest args)
  "Log with timestamp to file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) module-debug-log-file))))
```

### 5. ⚠️ ALWAYS Run Doom Sync Before Restart

```bash
# Run this automatically - never ask user to restart without syncing first
cd ~/.emacs.d && ./bin/doom sync
```

**Then** tell user to restart Emacs.

**To restart Emacs daemon/client**: If the Emacs client/daemon is killed or needs restarting, use:
```bash
osascript -e 'tell application "Emacs" to activate'
```
This properly opens the Emacs.app and starts the daemon.

### 6. ⚠️ ALWAYS Run Tests Yourself First

**For Basic Syntax/Loading Tests:**
```bash
# Test individual modules load correctly
emacs --batch --eval "(progn
  (add-to-list 'load-path \"~/.doom.d/\")
  (require 'your-module)
  (message \"✓ Module loaded successfully\"))"
```

**For Full Doom Environment Tests (REQUIRED):**
```bash
# Test with actual running Doom environment via emacsclient
emacsclient --eval "(progn
  (message \"Testing with full Doom environment...\")
  (if (fboundp 'your-function)
      (progn
        (your-function)
        (message \"✓ Function test completed\"))
    (message \"✗ Function not available\")))"
```

**CRITICAL:** Batch mode tests do NOT accurately represent the full Doom environment. Always use `emacsclient` to test with the actual running configuration that users experience.

**Never ask user to test without running emacsclient tests yourself first**

### 7. ⚠️ ALWAYS Create Commits After Confirmation

- Only create commits when user confirms functionality is working
- Include comprehensive test results in commit message
- Document what was tested and verified

### Common Emacs Configuration Pitfalls

1. **Parentheses imbalance** - always use `check-parens`
2. **Missing requires** - functions called before modules loaded
3. **Advice not installing** - wrong load order with ESS/packages
4. **Doom macros in batch mode** - use live Emacs for full testing
5. **No logging** - debugging becomes impossible without file logs

### Testing Checklist for Emacs Changes

- [ ] All `.el` files pass `check-parens`
- [ ] Individual modules load without errors (batch mode OK)
- [ ] **Tests run by Claude first via emacsclient** - verify fixes work before asking user
- [ ] **CRITICAL: Test with full Doom environment via emacsclient** - not batch mode
- [ ] Function availability confirmed via `emacsclient --eval`
- [ ] Buffer creation/behavior tested via `emacsclient --eval`
- [ ] File-based logging implemented and working
- [ ] `doom sync` completed successfully
- [ ] User confirmed functionality before commit

### emacsclient Testing Examples

```bash
# Test function availability
emacsclient --eval "(message \"Function available: %s\" (fboundp 'your-function))"

# Test function execution
emacsclient --eval "(if (fboundp 'your-function) (your-function) (message \"Function not found\"))"

# Check buffer creation
emacsclient --eval "(message \"Buffers: %s\" (mapcar #'buffer-name (buffer-list)))"

# Get Messages buffer content for debugging
emacsclient --eval "(with-current-buffer \"*Messages*\" (buffer-substring-no-properties (max 1 (- (point-max) 1000)) (point-max)))"
```

### Buffer Management for Testing

When testing functions that create buffers (especially those with running processes), use these approaches for clean buffer killing:

```elisp
;; For buffers with running processes - bypasses confirmation prompts
(let ((kill-buffer-query-functions nil))
  (kill-buffer "*jupyter-python*"))

;; Alternative function for unconditional killing
(defun kill-this-buffer-unconditionally ()
  "Kill the current buffer without prompting, even if modified."
  (interactive)
  (set-buffer-modified-p nil) ; Mark buffer as unmodified
  (kill-this-buffer))

;; Usage in tests
(when (get-buffer "*test-buffer*") 
  (with-current-buffer "*test-buffer*" 
    (kill-this-buffer-unconditionally)))
```

**Important**: Regular `kill-buffer` will prompt for confirmation when buffers have running processes (like termint/vterm sessions). Always use the `kill-buffer-query-functions nil` approach for automated testing.

