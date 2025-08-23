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

### Jupyter Console Integration Architecture

#### Technical Stack

- **Process Management**: termint.el (not comint) with bracketed paste support for multi-line code blocks
- **Terminal Backend**: eat (not vterm) - vterm does not support sixel graphics in Emacs
- **Graphics Display**: img2sixel for inline sixel display by overloading plot functions (plt.show, ggplot2::print.ggplot, etc.)
- **Console Mode**: Full jupyter console (no --simple-prompt flag) as it interferes with graphics display
- **Environment**: TERM=xterm-kitty COLORTERM=truecolor for proper sixel support

#### Key Files

- **Primary**: `jupyter-termint.el` - Main implementation for both Python and R jupyter integration
- **Deprecated**: `jupyter-console.el` - Should be removed, replaced by termint approach
- **Language-specific**: `jupyter-r-sixel.el` - R-specific termint integration with ggplot2 overrides

#### Why These Architectural Choices

- **termint over comint**: Better multi-line handling with bracketed paste support for code blocks
- **eat over vterm**: vterm in Emacs cannot display sixel graphics inline - eat provides native sixel support
- **img2sixel**: Provides true inline graphics display in terminal buffers, essential for data visualization
- **No --simple-prompt**: Required for proper graphics display and IRdisplay functionality in R/Python
- **Function overloading**: Automatic sixel conversion by intercepting plot display functions

#### Implementation Pattern

Both Python and R use the unified approach:

1. `termint-define` with eat backend and bracketed paste enabled
2. `pixi run jupyter console --kernel {python3|ir}` (no simple-prompt flag)
3. Environment variables: `TERM=xterm-kitty COLORTERM=truecolor` for sixel support
4. Function overloading for automatic sixel graphics:
   - Python: `plt.show()` → sixel display
   - R: `print.ggplot()` → sixel display
   - Automatic plot interception and conversion

#### Buffer Management

- Buffer names: `*jupyter-python*`, `*jupyter-r*`
- Process management through termint with eat backend
- Split-window display with automatic plot rendering in same buffer

### Claude Code Integration

- **Screenshots**: Always resize screenshots to below 2000 pixels before uploading to Claude to avoid API errors
- When taking screenshots for debugging/verification, use tools like `convert` or `sips` to resize: `sips -Z 1900 screenshot.png`

### Testing and Verification Protocol

**IMPORTANT**: Testing workflows with `Bash(emacsclient ...)` and screenshots consume excessive context. Instead:

1. **Use the Task tool with general-purpose agent** for all testing workflows that involve:
   - Multiple `emacsclient --eval` commands
   - Screenshot verification
   - Interactive testing of Emacs functionality
   - Buffer management and verification

2. **Testing subagent should handle**:
   - All `emacsclient --eval` testing commands
   - Screenshot capture and verification
   - Function availability checking
   - Buffer content verification
   - Keybinding testing

3. **Main Claude should only**:
   - Plan the implementation
   - Write/edit configuration files
   - Provide final summary of results from subagent

**Example**: "Use the Task tool to create a testing subagent that verifies the jupyter-termint.el sixel integration works by testing C-RET keybindings, checking *jupyter-python*/*jupyter-r* buffer creation, sending test plots with automatic plt.show()/print.ggplot() sixel conversion, and taking screenshots to confirm split window + inline sixel display."

### Screenshots and Screen Capture

- **Emacs window screenshots**: Use the streamlined osascript approach:

  ```bash
  # Recommended method: Focus Emacs then capture with delay
  osascript -e 'tell application "Emacs" to activate'
  screencapture -T 0.5 screenshot.png
  ```

- This approach eliminates the need for user clicking or elisp delays
- The `-T 0.5` flag adds a half-second delay after focusing (CRITICAL)
- More reliable than window ID methods for automated testing

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

**CRITICAL:** Batch mode tests do NOT accurately represent the full Doom environment. Always use `Bash(emacsclient ...)` to test with the actual running configuration that users experience.

**Never ask user to test without running emacsclient tests yourself first**

### 7. ⚠️ ALWAYS Create Commits After Confirmation

- Only create commits when user confirms functionality is working
- Include comprehensive test results in commit message
- Document what was tested and verified

## Emacs Startup Debugging Protocol

When starting a fresh Emacs instance or encountering configuration errors, always follow this debugging protocol:

1. **Check Warnings Buffer First**: Immediately after Emacs starts, check the *Warnings* buffer using:

   ```bash
   emacsclient --eval "(with-current-buffer \"*Warnings*\" (buffer-substring-no-properties (max 1 (- (point-max) 2000)) (point-max)))"
   ```

2. **Look for Common Issues**:
   - File missing errors (e.g., "Cannot open load file")  
   - Void function errors (e.g., "(void-function some-function)")
   - Syntax errors in .el files
   - Package loading failures

3. **Fix Configuration Errors**:
   - Remove references to deleted files in config.el, bindings.el
   - Comment out calls to undefined functions
   - Check for missing package dependencies
   - Verify file paths are correct

4. **Clean Restart Process**:
   - Kill all Emacs processes: First find processes with `ps aux | grep -i emacs`, then use `kill -9 <PID>` for each Emacs process ID

   ```bash
   # Find Emacs processes
   ps aux | grep -i emacs
   # Kill each Emacs process (replace PID with actual process ID)
   kill -9 <PID>
   # Or use this one-liner to kill all at once
   ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9
   ```

   - Delete compiled files: `find ~/.doom.d -name "*.elc" -delete`  
   - Start fresh: `osascript -e 'tell application "Emacs" to activate'`
   - Wait for full startup before testing functionality

5. **Verify Fixes**: After fixing errors, always check the Warnings buffer again to ensure clean startup.

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
- [ ] **CRITICAL: Test with full Doom environment via Bash(emacsclient)** - not batch mode
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

# Get Warnings buffer content for debugging
emacsclient --eval "(with-current-buffer \"*Warnings*\" (buffer-substring-no-properties (max 1 (- (point-max) 1000)) (point-max)))"
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

;; Usage in tests for jupyter buffers
(when (get-buffer "*jupyter-python*") 
  (with-current-buffer "*jupyter-python*" 
    (kill-this-buffer-unconditionally)))

;; Clean up both Python and R jupyter buffers
(dolist (buf '("*jupyter-python*" "*jupyter-r*"))
  (when (get-buffer buf)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buf))))
```

**Important**: Regular `kill-buffer` will prompt for confirmation when buffers have running processes (like termint sessions with jupyter console). Always use the `kill-buffer-query-functions nil` approach for automated testing. This is especially important for jupyter-termint.el buffers which maintain persistent jupyter console processes.
