# Termint Implementation Progress

## Goal
Replace comint-based jupyter-console with termint.el using eat backend to enable proper bracketed paste mode and sixel graphics support. CRITICAL: Never use vterm backend - eat is required for inline graphics display.

## Current Status
- ✅ Added termint package to packages.el
- ✅ Ran doom sync to install termint  
- ✅ Configured Claude Code integration in config.el
- ✅ Created euporie-termint.el implementation
- ✅ Updated config.el to use euporie-termint instead of jupyter-console
- ✅ Verified termint installation and syntax
- ✅ Created test files for Jupyter multi-line functionality
- ✅ Remapped all Claude Code IDE keybindings to termint versions
- ✅ Added YOLO submenu to claude-code-ide menu
- ✅ Implementation ready for user testing

## Issues Encountered
None yet.

## Configuration Steps

### 1. Package Installation
```elisp
;; In packages.el
(package! termint
  :recipe (:host github :repo "milanglacier/termint.el"))
```

### 2. Planned Claude Code Configuration
```elisp
(use-package termint
  :bind (("C-c C" . termint-claude-code-start))
  :bind-keymap ("C-c c" . termint-claude-code-map)
  :config
  (setq termint-backend 'eat)  ; CRITICAL: eat backend required for sixel graphics
  (termint-define "claude-code" "claude" 
    :bracketed-paste-p t
    :send-delayed-final-ret t
    :source-syntax "@{{file}}"))
```

### 3. Planned Jupyter Configuration
```elisp
(termint-define "euporie-python" "euporie-console --kernel-name python3"
  :bracketed-paste-p t
  :source-syntax termint-ipython-source-syntax-template)
```

## Benefits Expected

1. **Multi-line single cells**: Bracketed paste will make multi-line code execute as single cells
2. **Better terminal emulation**: Full ANSI support, colors, proper rendering
3. **Sixel graphics support**: eat backend provides native inline graphics display
4. **Consistent approach**: Same pattern for Claude Code and Jupyter
5. **Unified keybindings**: All Claude Code functions now use termint backend
6. **Enhanced yolo support**: Separate yolo versions for continue/resume with proper --dangerously-skip-permissions flag

## Testing Plan

1. Test Claude Code integration first (simpler)
2. Test multi-line code handling
3. Migrate Jupyter console implementation
4. Test image-test.org with new implementation
5. Verify inline image display still works

## Function Overrides

All Claude Code IDE functions have been overridden with termint implementations while preserving the original menu interface:

### Menu Access
- `C-c C-'` → `claude-code-ide-menu` (Original menu preserved)
- `C-<escape>` → `termint-claude-code-send-escape` (Send escape key)

### Overridden Functions (called by menu)
**Session Management:**
- `claude-code-ide-start-session` → `termint-claude-code-start`
- `claude-code-ide-continue-conversation` → `termint-claude-code-continue`  
- `claude-code-ide-resume-session` → `termint-claude-code-resume`
- `claude-code-ide-stop-session` → `termint-claude-code-quit`
- `claude-code-ide-list-sessions` → `termint-claude-code-list`

**Navigation:**
- `claude-code-ide-switch-to-buffer` → `termint-claude-code-switch-to-buffer`
- `claude-code-ide-toggle-window` → `termint-claude-code-toggle-window`

**Interaction:**
- `claude-code-ide-insert-selection` → `termint-claude-code-insert-selection`
- `claude-code-ide-send-prompt` → `termint-claude-code-send-prompt`
- `claude-code-ide-send-escape` → `termint-claude-code-send-escape`
- `claude-code-ide-insert-newline` → `termint-claude-code-insert-newline`

**Special Functions:**
- `claude-code-ide-yolo` → `termint-claude-code-yolo`
- `claude-code-ide-yolo-continue` → `termint-claude-code-yolo-continue`
- `claude-code-ide-yolo-resume` → `termint-claude-code-yolo-resume`

**Submenus:**
- `claude-code-ide-yolo-menu` → New YOLO submenu with bypass permissions options
- `claude-code-ide-configuration` → `termint-claude-code-configuration`
- `claude-code-ide-debugging` → `termint-claude-code-debugging`

## YOLO Submenu Structure

The new YOLO submenu (`Y` key in main menu) provides access to --dangerously-skip-permissions options:

### YOLO Menu Options
- `s` → `termint-claude-code-yolo` (Start new session with yolo)
- `c` → `termint-claude-code-yolo-continue` (Continue conversation with yolo) 
- `r` → `termint-claude-code-yolo-resume` (Resume session with yolo)

All yolo functions use the `claude --dangerously-skip-permissions` flag with appropriate resume options.

## Next Steps

1. ✅ Run doom sync to install termint
2. ✅ Configure Claude Code integration
3. ✅ Test basic functionality
4. ✅ Implement Jupyter integration
5. ✅ Remap all keybindings to termint
6. 🔄 User testing and validation