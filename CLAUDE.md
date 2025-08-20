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

## Jupyter Integration (CRITICAL)

### ZMQ Elimination Strategy
Jupyter has been configured to work **WITHOUT ZMQ** to avoid persistent binary module conflicts. This is a hard requirement.

**NEVER re-enable ZMQ** - it causes binary compatibility issues between nix and system packages.

### Current Configuration
1. **Nix packages**: ZMQ removed from `~/nix/modules/darwin/packages.nix`
   - Line should be: `((emacsPackagesFor emacs-macport).emacsWithPackages (epkgs: [ epkgs.vterm ]))`
   - NO `epkgs.zmq` in the list

2. **Doom packages**: ZMQ disabled in `.doom.d/packages.el`
   - `(package! zmq :disable t)` must remain

3. **Config settings**: ZMQ forced off in `.doom.d/config.el`
   - `jupyter-use-zmq nil` in the jupyter configuration
   - Direct kernel startup using `jupyter kernel` command

### Verification Commands
**Quick check**: Run the verification script:
```bash
~/dotfiles/scripts/check-zmq-status.sh
```

**Manual checks** (if needed):
```bash
# Should return nil (not found)
emacs --batch --eval "(message \"ZMQ: %s\" (locate-library \"zmq\"))"

# Should show no ZMQ build directories
ls ~/.emacs.d/.local/straight/build*/zmq* 2>/dev/null

# Should show no ZMQ compiled files
find ~/.emacs.d/.local -name "*zmq*.elc" 2>/dev/null
```

### If ZMQ Issues Return
1. **Check nix config**: Ensure `epkgs.zmq` not in packages.nix
2. **Rebuild nix**: `cd ~/nix && nix run .#build-switch`  
3. **Remove compiled files**: `rm -rf ~/.emacs.d/.local/straight/build*/zmq/`
4. **Remove jupyter compiled files**: `rm -f ~/.emacs.d/.local/straight/build*/jupyter/*.elc`
5. **Restart Emacs completely**

### Working Kernel Types
- **Python**: `jupyter-python` source blocks with `python3` kernel
- **R**: `jupyter-R` source blocks with `ir` kernel  
- **Stata**: `jupyter-stata` source blocks with `nbstata` kernel

All use websocket/HTTP communication instead of ZMQ sockets.

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

### 6. ⚠️ ALWAYS Run Tests Yourself First
```bash
# Test your fixes before asking user to test
cd ~/.doom.d
# Load and run your test functions
emacs --batch -l test-file.el --eval "(test-function)"
```
**Never ask user to test without running tests yourself first**

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
- [ ] Individual modules load without errors
- [ ] **Tests run by Claude first** - verify fixes work before asking user
- [ ] Full functionality tested in live Emacs session
- [ ] File-based logging implemented and working
- [ ] `doom sync` completed successfully
- [ ] User confirmed functionality before commit