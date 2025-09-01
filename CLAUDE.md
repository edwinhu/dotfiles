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

### Euporie Console Integration Architecture (CURRENT FOCUS: SAS Integration)

#### Current Priority: SAS Remote Execution with TRAMP

**ACTIVE ISSUE**: Complete SAS integration with euporie, org-babel, and remote TRAMP execution.
**STATUS**: ✅ **COMPLETE** - SAS kernel integration with local/remote execution achieved.

#### Previous Achievement: Stata Inline Graphics Issue Resolution

**RESOLVED ISSUE**: Stata kernel graphics display console cleanliness in euporie environments.
**STATUS**: ✅ **RESOLVED** - Graph counter messages eliminated, clean console output achieved.

#### Technical Stack

- **Process Management**: 
  - Local: termint.el with bracketed paste support for multi-line code blocks
  - Remote: TRAMP-aware process execution with comint for remote directories
- **Terminal Backend**: eat (NEVER vterm) - vterm cannot display sixel graphics in Emacs
- **Graphics Display**: Native euporie graphics display with IPython-compatible MIME display system
- **Console Modes**: 
  - Python: `euporie-console --kernel-name=python3`
  - R: `euporie-console --kernel-name=ir`
  - Stata: `euporie-console --kernel-name=stata` 
  - **SAS: `euporie-console --kernel-name=sas` (NEW)**
- **Environment**: TERM=eat-truecolor COLORTERM=truecolor EUPORIE_GRAPHICS=sixel
- **CRITICAL**: Always use eat backend - vterm will break inline graphics display
- **TRAMP Support**: Remote execution via `:dir /sshx:host:/path` in org-babel blocks

#### SAS Kernel Integration (NEW - COMPLETE)

**Achievement**: Complete SAS integration with local and remote execution capabilities.

**Current Architecture**:
1. **SAS Command** → `proc print data=sashelp.cars; run;` (user input)
2. **Local Execution** → termint.el + eat backend for `pixi run euporie-console --kernel-name=sas`
3. **Remote Execution** → TRAMP + comint with `start-file-process` for remote directories
4. **Graphics Display** → Native euporie sixel graphics display
5. **Org-babel Integration** → Automatic detection of `:dir` parameter for remote vs local

**Key Files**:
- `euporie-termint.el` - Unified SAS integration with TRAMP support
- `ob-sas.el` - SAS-mode integration and language alias mapping
- `config.el` - Language mode mapping: "sas" → `SAS-mode`

**TRAMP Remote Execution**:
- Path detection: `(file-remote-p dir)` for any TRAMP path
- Process creation: `start-file-process` with `default-directory` set to TRAMP path
- Communication: `comint-send-string` for remote process interaction
- Automatic routing: Local uses termint, remote uses TRAMP + comint

#### Stata Kernel Graphics Pipeline (Recently Fixed)

**Issue Resolved**: Graph counter messages `global stata_kernel_graph_counter = $stata_kernel_graph_counter + 1` eliminated from console output.

**Current Architecture**:
1. **Stata Command** → `scatter price mpg` (user input)
2. **Graph Generation** → PNG files created in `.stata_kernel_cache/`  
3. **Detection System** → `_check_and_display_graphs()` finds new graphics
4. **MIME Display** → IPython-compatible `display_data` messages via `_send_euporie_graphics()`
5. **Console Output** → Clean interface without system messages (like Python/R kernels)

**Files Modified**:
- `stata_kernel/kernel.py` - IPython MIME compatibility, streamlined graphics detection
- `stata_kernel/stata_session.py` - Fixed infinite loops, eliminated duplicate displays  
- `stata_kernel/code_manager.py` - Suppressed counter messages with `quietly` prefix
- `stata_kernel/graphics.py` - Enhanced environment detection

#### Key Files

- **Primary**: `euporie-termint.el` - Main implementation for Python, R, Stata, and **SAS** euporie integration
- **SAS Core**: `ob-sas.el` - SAS-mode definition and org-babel integration with TRAMP support
- **Stata Graphics Core**: Modified `stata_kernel/kernel.py` with IPython-compatible MIME display
- **Configuration**: `config.el` - Language aliases and kernel integrations
- **TRAMP Integration**: Remote execution via `file-remote-p` detection and `start-file-process`

#### Implementation Pattern

**SAS Kernel (NEW - COMPLETE)**:
1. **Local**: `termint-define` with eat backend and bracketed paste enabled
2. **Remote**: TRAMP-aware `start-file-process` with comint integration
3. **Command**: `pixi run euporie-console --kernel-name=sas`
4. **Detection**: Automatic local/remote routing via `(file-remote-p dir)`
5. **Graphics**: Native euporie sixel display for both local and remote
6. **Usage**: 
   - Local: `#+begin_src sas`
   - Remote: `#+begin_src sas :dir /sshx:wrds:/path`
7. **Result**: Unified SAS experience with seamless TRAMP integration

**Stata Kernel (Recently Fixed)**:
1. `termint-define` with eat backend and bracketed paste enabled
2. `pixi run euporie-console --kernel-name=stata` (clean console output)
3. Commands: `sysuse auto` → `scatter price mpg` (no counter messages)
4. Graphics: Automatic inline display via euporie's native protocols
5. **Result**: Professional console experience matching Python/R kernels

**Python/R Kernels** (Working Reference):
1. `termint-define` with eat backend and bracketed paste enabled  
2. `pixi run euporie-console --kernel-name={python3|ir}`
3. Environment variables: `TERM=eat-truecolor COLORTERM=truecolor`
4. Native graphics display with automatic protocol detection

#### Buffer Management

- **Buffer names**: 
  - Local: `*euporie-python*`, `*euporie-r*`, `*euporie-stata*`, `*euporie-sas*`
  - Remote: `*euporie-sas-remote*` (SAS only - others use local termint)
- **Process management**: 
  - Local: termint with eat backend (NEVER vterm)
  - Remote: TRAMP + comint with `start-file-process`
- **Display**: Split-window display with automatic plot rendering in same buffer
- **CRITICAL**: eat backend required for sixel graphics - vterm will not work
- **SAS-Specific**: Automatic local/remote routing based on `:dir` parameter
- **Stata-Specific**: Clean console output without graph counter pollution

### Claude Code Integration

- **Screenshots**: Always resize screenshots to below 2000 pixels before uploading to Claude to avoid API errors
- When taking screenshots for debugging/verification, use tools like `convert` or `sips` to resize: `sips -Z 1900 screenshot.png`

### Agent Orchestration Protocol - SAS Integration with Euporie

**PROJECT**: SAS kernel integration with euporie, org-babel, and TRAMP remote execution

**FOCUS**: SAS integration using the same proven stack (eat, termint, sas_kernel, euporie, org-babel) with additional TRAMP remote execution support.

**CRITICAL**: Main Claude's role is ONLY orchestration and planning. Always delegate to specialized agents:

#### 1. **euporie-developer Agent** (SAS Implementation):
   - **Scope**: Implement SAS kernel integration features
   - **Focus**: 
     - SAS kernel integration with euporie-termint.el
     - TRAMP remote execution using `file-remote-p` and `start-file-process`
     - org-babel integration with `:dir` parameter support
     - SAS-mode integration and language aliases
   - **Stack**: eat backend, termint (local), comint (remote), sas_kernel, euporie
   - **Deliverables**: Working SAS integration with local/remote execution
   - **Files**: `euporie-termint.el`, `ob-sas.el`, `config.el`

#### 2. **euporie-tester Agent** (SAS Validation):
   - **Scope**: Comprehensive testing of SAS integration
   - **Focus**: 
     - Local SAS execution testing
     - Remote TRAMP execution validation
     - Graphics display verification (sixel)
     - org-babel block execution testing
     - Cross-language compatibility checks
   - **Test Cases**: 
     - `#+begin_src sas` (local)
     - `#+begin_src sas :dir /sshx:host:/path` (remote)
     - C-RET keybinding functionality
     - Buffer management and process lifecycle
   - **Deliverables**: Test reports with pass/fail analysis

#### 3. **Main Claude** (Orchestration ONLY):
   - **Plan features**: Define SAS integration requirements
   - **Coordinate agents**: Manage develop → test cycles  
   - **Evaluate results**: Analyze outcomes and plan refinements
   - **User interaction**: Gather feedback and refine requirements
   - **NEVER directly code**: Always delegate to specialized agents

#### **SAS Integration Workflow**:
1. **Developer** → Implements SAS kernel features with TRAMP support
2. **Tester** → Validates local and remote execution
3. **Main Claude** → Evaluates results and plans iterations

**Examples**:
- Implementation: "Use the euporie-developer agent to implement SAS kernel integration with TRAMP remote execution support"  
- Validation: "Use the euporie-tester agent to test SAS integration with both local and remote execution scenarios"

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
  (kill-buffer "*euporie-python*"))

;; Alternative function for unconditional killing
(defun kill-this-buffer-unconditionally ()
  "Kill the current buffer without prompting, even if modified."
  (interactive)
  (set-buffer-modified-p nil) ; Mark buffer as unmodified
  (kill-this-buffer))

;; Usage in tests for euporie buffers
(when (get-buffer "*euporie-python*") 
  (with-current-buffer "*euporie-python*" 
    (kill-this-buffer-unconditionally)))

;; Clean up Python, R, and Stata euporie buffers
(dolist (buf '("*euporie-python*" "*euporie-r*" "*euporie-stata*"))
  (when (get-buffer buf)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buf))))
```

**Important**: Regular `kill-buffer` will prompt for confirmation when buffers have running processes (like termint sessions with euporie console). Always use the `kill-buffer-query-functions nil` approach for automated testing. This is especially important for euporie-termint.el buffers which maintain persistent euporie console processes with eat backend.
