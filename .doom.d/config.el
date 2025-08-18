;;; $DOOMDIR/config.el -*- lexical-binding: t; -*- Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; CRITICAL: ZMQ completely eliminated - no jupyter package loaded

;; Load jupyter-console module for org-babel integration
(load (expand-file-name "jupyter-console.el" 
                       (or (bound-and-true-p doom-user-dir)
                          (expand-file-name "~/.doom.d/"))))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "CMU Serif" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'catppuccin)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Enable helm keyboard navigation
(customize-set-variable 'helm-ff-lynx-style-map t)

;; Mixed pitch fonts
(use-package mixed-pitch
  :hook
  (org-roam-mode . mixed-pitch-mode))

;; Get rid of titlebar
;; https://github.com/doomemacs/doomemacs/issues/7532
;; (add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))

;; eat terminal
(use-package eat
  :config
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)

  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  ;; Use standard terminal type
  (setq eat-term-name "xterm-256color")
  
  ;; Fix whitespace issues in eat terminal
  (add-hook 'eat-mode-hook
            (lambda ()
              ;; Disable whitespace visualization
              (when (fboundp 'whitespace-mode) (whitespace-mode -1))
              (setq-local show-trailing-whitespace nil)
              (setq-local nobreak-char-display nil)))

  ;; Optional keybindings
  :bind (("C-c t" . eat)
         ("C-c T" . eat-other-window))
)

;; vterm terminal configuration
(after! vterm
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Disable whitespace visualization
              (when (fboundp 'whitespace-mode) (whitespace-mode -1))
              (setq-local show-trailing-whitespace nil)
              (setq-local nobreak-char-display nil))))

;; Test vterm instead of eat for better Unicode handling
(setq +term-backend 'vterm)

;; Prevent global whitespace mode issues in terminals
(after! whitespace
  ;; Ensure whitespace mode doesn't interfere with terminals
  (setq whitespace-global-modes '(not vterm-mode eat-mode term-mode)))

;; Fix Unicode emoji substitution by overriding Doom's font configuration
(defun fix-unicode-emoji-substitution ()
  "Prevent technical Unicode symbols from rendering as color emoji.
This function forces JetBrains Mono for ranges of Unicode characters
that should be monospace but are often hijacked by Apple Color Emoji."
  (when (fboundp 'set-fontset-font)
    (let ((mono-font "JetBrains Mono"))
      ;; Technical Symbols (U+2300-U+23FF)
      ;; Includes: ⏸ ⏹ ⏺ ⏻ ⏼ ⏽ etc.
      (set-fontset-font t '(#x2300 . #x23ff) mono-font)
      
      ;; Miscellaneous Technical (U+2300-U+23FF already covered above)
      
      ;; Geometric Shapes (U+25A0-U+25FF)
      ;; Includes: ■ □ ▲ △ ▶ ▷ ◀ ◁ ● ○ etc.
      (set-fontset-font t '(#x25a0 . #x25ff) mono-font)
      
      ;; Box Drawing (U+2500-U+257F)
      ;; Includes: ─ │ ┌ ┐ └ ┘ ├ ┤ etc.
      (set-fontset-font t '(#x2500 . #x257f) mono-font)
      
      ;; Block Elements (U+2580-U+259F)
      ;; Includes: ▀ ▄ █ ▌ ▐ etc.
      (set-fontset-font t '(#x2580 . #x259f) mono-font)
      
      ;; Arrows (U+2190-U+21FF)
      ;; Includes: ← → ↑ ↓ ↔ ↕ etc.
      (set-fontset-font t '(#x2190 . #x21ff) mono-font)
      
      ;; Mathematical Operators (U+2200-U+22FF)
      ;; Includes: ∀ ∂ ∃ ∅ ∇ ∈ etc.
      (set-fontset-font t '(#x2200 . #x22ff) mono-font)
      
      ;; Miscellaneous Symbols (U+2600-U+26FF)
      ;; Includes: ☀ ☁ ☂ ☃ ★ ☆ ☎ ☏ ☐ ☑ ☒ ⚡ etc.
      ;; This range has many emoji variants, so be selective
      (set-fontset-font t '(#x2600 . #x26ff) mono-font)
      
      ;; Dingbats (U+2700-U+27BF) - often has emoji variants
      ;; Includes: ✓ ✗ ✦ ✧ ✳ etc.
      (set-fontset-font t '(#x2700 . #x27bf) mono-font)
      
      ;; Supplemental Arrows-A (U+27F0-U+27FF)
      (set-fontset-font t '(#x27f0 . #x27ff) mono-font)
      
      ;; Supplemental Arrows-B (U+2900-U+297F)
      (set-fontset-font t '(#x2900 . #x297f) mono-font)
      
      ;; Miscellaneous Symbols and Arrows (U+2B00-U+2BFF)
      ;; Includes: ⬅ ⬆ ⬇ ⭐ ⭕ etc.
      (set-fontset-font t '(#x2b00 . #x2bff) mono-font)
      
      ;; Additional specific problematic characters
      ;; These are common technical symbols that still might slip through
      (dolist (char '(?⏸ ?⏹ ?⏺ ?⏻ ?⏼ ?⏽  ; Media controls
                      ?✓ ?✗ ?✦ ?✧ ?✳      ; Check marks and asterisks
                      ?▶ ?◀ ?▲ ?▼ ?◆ ?◇    ; Triangles and diamonds
                      ?● ?○ ?■ ?□ ?▪ ?▫    ; Circles and squares
                      ?⚡ ?⚠ ?⚙ ?⚛         ; Warning and tech symbols
                      ?⬅ ?➡ ?⬆ ?⬇         ; Bold arrows
                      ?⭐ ?⭕))             ; Star and circle
        (set-fontset-font t char mono-font)))))

;; Run after Doom's font setup (this is the critical hook)
(add-hook! 'after-setting-font-hook :append #'fix-unicode-emoji-substitution)

;; Nerd Icons configuration
(use-package nerd-icons
  :config
  ;; Configure nerd-icons for better terminal support
  (when (not (display-graphic-p))
    (setq nerd-icons-font-family "JetBrainsMono Nerd Font"))
  ;; Set default scale for consistent sizing
  (setq nerd-icons-scale-factor 1.0))

;; Claude Code
(use-package claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-window-width 120)
  (setq claude-code-ide-terminal-backend 'vterm))

;; Include SAS support
(use-package! ob-sas
  :load-path "~/.doom.d/")

;; Include Stata support
(use-package! ob-stata
  :load-path "~/.doom.d/")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sas . t)
   (R . t)
   ;; Don't load python here - we'll use our own
   ;; (python . t)
   (stata . t)
   ;; (jupyter . t)  ; Disabled to prevent ZMQ issues
   ))

;; Use advice to ensure our functions are always called
(defun +jupyter-console-python-advice (body params)
  "Advice to use jupyter-console for Python."
  (require 'jupyter-console)
  (jupyter-console-log 'info "Python advice called")
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "python3" file))
         (result (jupyter-console-send-string buffer body)))
    (jupyter-console-log 'info "Python execution completed via advice")
    result))

(defun +jupyter-console-stata-advice (body params)
  "Advice to use jupyter-console for Stata."
  (require 'jupyter-console)
  (jupyter-console-log 'info "Stata advice called")
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "stata" file))
         (result (jupyter-console-send-string buffer body)))
    (jupyter-console-log 'info "Stata execution completed via advice")
    result))

(defun +jupyter-console-r-advice (body params)
  "Advice to use jupyter-console for R."
  (require 'jupyter-console)
  (jupyter-console-log 'info "R advice called")
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "ir" file))
         (result (jupyter-console-send-string buffer body)))
    (jupyter-console-log 'info "R execution completed via advice")
    result))

;; Add advice after org loads
(with-eval-after-load 'org
  (with-eval-after-load 'ob
    (require 'jupyter-console)
    ;; Use advice-add with override to ensure our functions run
    (advice-add 'org-babel-execute:python :override #'+jupyter-console-python-advice)
    (advice-add 'org-babel-execute:stata :override #'+jupyter-console-stata-advice)
    (advice-add 'org-babel-execute:R :override #'+jupyter-console-r-advice)
    (jupyter-console-log 'info "Jupyter console advice installed")))
;; Language mode mappings for syntax highlighting in org source blocks
(add-to-list 'org-src-lang-modes '("sas" . SAS))
;; Jupyter language mappings removed to prevent loading jupyter package

;; Jupyter org-babel-execute functions removed to prevent loading jupyter package
;; Use direct python, R, and stata blocks instead of jupyter-* blocks

;; Edit in same window
;; Doom now treats these buffers as pop-ups
;; which breaks the default behavior unless you tell it to ignore
(after! org
  (set-popup-rule! "^\\*Org Src" :ignore t))
(after! vterm
  (set-popup-rule! "^\\*vterm " :ignore t))
(setq org-src-window-setup 'current-window)
(setq org-latex-pdf-process
  '("tectonic %f"))

;; enable org shift select
(setq org-support-shift-select 'always)

;; org hide markers
(setq org-hide-emphasis-markers t)

;; unfill
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; need to have exec-path-from-shell in order to get PATH
(exec-path-from-shell-initialize)

;; eval-in-repl
(use-package! eval-in-repl)
(setq eir-repl-placement 'left)
;; python support  
;; The eval-in-repl-python functionality is included in the main eval-in-repl package
(add-hook 'python-mode-hook
          #'(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))
;; Shell support
;; The eval-in-repl-shell functionality is included in the main eval-in-repl package
(add-hook 'sh-mode-hook
          #'(lambda()
             (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))

;; File-based logging for debugging
(defvar jupyter-debug-log-file (expand-file-name "jupyter-debug.log" "~/"))

(defun log-to-file (message)
  "Log MESSAGE to debug file with timestamp"
  (with-temp-buffer
    (insert (format "[%s] %s\n" 
                    (format-time-string "%Y-%m-%d %H:%M:%S") 
                    message))
    (append-to-file (point-min) (point-max) jupyter-debug-log-file)))

(defun clear-jupyter-log ()
  "Clear the jupyter debug log file"
  (interactive)
  (when (file-exists-p jupyter-debug-log-file)
    (delete-file jupyter-debug-log-file))
  (log-to-file "=== Jupyter Debug Log Started ==="))

;; Initialize log
(clear-jupyter-log)

;; Test without loading ZMQ at all to isolate the issue
(log-to-file "Skipping ZMQ loading to test if that's the source of the error")

;; Direnv integration for environment management
(use-package! envrc
  :config
  (envrc-global-mode)
  ;; Ensure envrc updates exec-path and PATH
  (setq envrc-debug t)) ; Enable debug mode for troubleshooting

;; Function to find jupyter in pixi environment - kept for jupyter-console.el
(defun find-pixi-jupyter ()
  "Find jupyter executable in current pixi environment."
  (let* ((default-directory (locate-dominating-file default-directory ".envrc"))
         (pixi-jupyter (when default-directory
                        (expand-file-name ".pixi/envs/default/bin/jupyter" default-directory))))
    (when (and pixi-jupyter (file-executable-p pixi-jupyter))
      pixi-jupyter)))

;; DISABLED - These were for the old jupyter package
;; ;; Function to update jupyter paths for current project
;; (defun update-jupyter-paths ()
;;   "Update jupyter paths for current project."
;;   (when-let ((jupyter-path (find-pixi-jupyter)))
;;     (setq-local jupyter-command jupyter-path
;;                 jupyter-executable jupyter-path
;;                 org-babel-jupyter-command jupyter-path)
;;     (message "Updated jupyter path to: %s" jupyter-path)))

;; ;; Hook to update jupyter paths when entering a directory
;; (add-hook 'find-file-hook #'update-jupyter-paths)

;; Simple logging for jupyter debugging
(defvar jupyter-debug-log-file (expand-file-name "jupyter-debug.log" default-directory))

(defun jupyter-debug-log (level format-string &rest args)
  "Simple logging function."
  (let ((message (apply #'format format-string args)))
    (message "%s: %s" (upcase (symbol-name level)) message)))

;; STRATEGY A: DISABLED - Jupyter package removed to prevent ZMQ issues
;; Using jupyter-console.el instead for org-babel jupyter functionality
;; The old jupyter package configuration has been completely removed to avoid errors

;; NOTE: All jupyter package code has been removed. The following block (lines 327-544)
;; contained the old jupyter configuration which is no longer needed:
;; - jupyter-run-repl functions
;; - jupyter-connect-repl functions  
;; - jupyter kernel startup overrides
;; - ZMQ-related settings
;; This has all been replaced by our simpler jupyter-console.el module

;; PLACEHOLDER START - old jupyter code was here
;; [All old jupyter configuration code removed - lines 336-552]
;; PLACEHOLDER END - old jupyter code was here
;; keybindings
(load! "bindings")

;; Testing utilities (uncomment to test different Unicode approaches)
;; (load! "test-unicode-fix")
