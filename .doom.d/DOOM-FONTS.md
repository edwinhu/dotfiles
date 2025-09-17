# Doom Emacs Font Configuration Guide

## Overview

Doom Emacs has a sophisticated font handling system that integrates with Emacs's fontset mechanism. This guide documents how font configuration works, particularly for handling Nerd Fonts, emojis, and symbol rendering in Claude Code integration.

## Key Concepts

### Font Priority System

Emacs uses a font priority system where fonts can be added to fontsets with different priorities:

- `'prepend` - Adds font to beginning of search list (highest priority)
- `'append` - Adds font to end of search list (lowest priority)
- No flag - Replaces existing font configuration

### Critical Variables

#### `use-default-font-for-symbols`

**CRITICAL**: This variable controls whether Emacs honors fontset configurations for symbols.

- `t` (default) - Emacs uses the default face's font for symbols, ignoring fontsets
- `nil` - Emacs respects fontset configuration for symbols

**Must be set to `nil` to make font priority configuration work.**

#### Fontset Targets

- `"fontset-default"` - Serves as fallback for all fontsets (recommended)
- `t` - Current fontset only
- Specific fontset names for targeted configuration

## Doom Emacs Font Migration

### All-the-Icons → Nerd-Icons Transition

Recent Doom versions have migrated from `all-the-icons` to `nerd-icons`:

- `doom-modeline` now uses nerd fonts instead of all-the-icons
- `nerd-icons` aims to unify icon experience across GUI and terminal
- Nerd Font icons live in Unicode Private Use Area (#xe000-#xf8ff)

### Installation Requirements

1. **System Font Installation**:
   ```bash
   # Download from https://www.nerdfonts.com/
   # Install "Symbols Nerd Font Mono" (recommended)
   ```

2. **Emacs Font Installation**:
   ```elisp
   M-x nerd-icons-install-fonts
   ```

3. **Terminal Configuration**: Set terminal font to a Nerd Font for terminal usage

## Font Configuration Strategies

### 1. Font Priority Approach (Recommended)

```elisp
;; Disable default font for symbols to honor fontset configuration
(setq use-default-font-for-symbols nil)

;; Set Nerd Font with highest priority for all symbols
(set-fontset-font "fontset-default" 'symbol
                  (font-spec :family "Symbols Nerd Font Mono")
                  nil 'prepend)

;; Prioritize main monospace font for technical symbols
(set-fontset-font "fontset-default" 'symbol
                  (font-spec :family doom-font-family)
                  nil 'prepend)

;; Specifically prioritize Nerd Font icons in private use area
(set-fontset-font "fontset-default" '(#xe000 . #xf8ff)
                  (font-spec :family "Symbols Nerd Font Mono")
                  nil 'prepend)

;; Set emoji fonts with lower priority
(set-fontset-font "fontset-default" 'emoji
                  '("Apple Color Emoji" . "iso10646-1")
                  nil 'append)
```

### 2. Explicit Unicode Range Override (Legacy)

```elisp
;; Direct override of specific Unicode ranges
(set-fontset-font t '(#x2300 . #x23ff) "JetBrains Mono") ; Technical Symbols
(set-fontset-font t '(#x25a0 . #x25ff) "JetBrains Mono") ; Geometric Shapes
(set-fontset-font t '(#x2600 . #x26ff) "JetBrains Mono") ; Misc Symbols
```

**Issues with legacy approach**:
- Requires explicit mapping of all Unicode ranges
- Harder to maintain and extend
- Less flexible than priority system

## Common Font Conflicts

### Emoji vs Technical Symbols

Many technical symbols (⚡ ⚠ ⚙ ✅ ❌) have both:
- Monospace variants (preferred for code/terminal)
- Colorful emoji variants (problematic in technical contexts)

**Solution**: Use font priority to favor monospace/nerd fonts over emoji fonts.

### GUI vs Terminal Differences

- **GUI Emacs**: Can use any installed system font
- **Terminal Emacs**: Limited to terminal's configured font
- **Solution**: Use Nerd Font for terminal + proper fontset configuration

## Troubleshooting

### Icons Appear Cut Off

- Install "Symbols Nerd Font Mono" specifically
- Avoid other Nerd Font variants that may not render correctly

### Font Not Found Errors

- Ensure fonts are properly installed system-wide
- Check font names match exactly (case-sensitive)
- Use `M-x describe-fontset` to debug

### Terminal vs GUI Inconsistency

- Set terminal font to matching Nerd Font
- Configure `nerd-icons-font-family` variable
- Test in both environments

## Best Practices

### Configuration Order

1. Set `use-default-font-for-symbols nil`
2. Configure Nerd Fonts with `'prepend`
3. Configure monospace fonts with `'prepend`
4. Configure emoji fonts with `'append`
5. Handle problematic characters individually if needed

### Hook Integration

```elisp
;; Apply immediately
(+configure-fonts)

;; Reapply when fonts change
(add-hook 'after-setting-font-hook #'+configure-fonts)

;; Ensure integration with Doom themes
(after! doom-themes
  (+configure-fonts))

;; Ensure integration with nerd-icons
(with-eval-after-load 'nerd-icons
  (+configure-fonts))
```

### Testing

Use the Unicode test function to verify configuration:

```elisp
(defun unicode-test-claude-code ()
  "Test Unicode rendering for common problematic characters."
  (interactive)
  ;; Creates test buffer with various symbols
  )
```

## Claude Code Integration

The font configuration is particularly important for Claude Code integration because:

- Claude Code output contains many technical symbols
- Consistent monospace rendering improves readability
- Emoji rendering can break terminal layout
- Nerd Font icons provide better visual consistency

The implemented priority system ensures technical symbols render as monospace rather than colorful emojis, maintaining a professional development environment.