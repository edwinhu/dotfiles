;;; $DOOMDIR/config.el -*- lexical-binding: t; -*- Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

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
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sas . t)
   (R . t)
   (python . t)
   (jupyter . t)
   ))
(add-to-list 'org-src-lang-modes '("sas" . SAS))

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
(use-package! eval-in-repl-python)
(add-hook 'python-mode-hook
          #'(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))
;; Shell support
(use-package! eval-in-repl-shell)
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

;; Function to find jupyter in pixi environment
(defun find-pixi-jupyter ()
  "Find jupyter executable in current pixi environment."
  (let* ((default-directory (locate-dominating-file default-directory ".envrc"))
         (pixi-jupyter (when default-directory
                        (expand-file-name ".pixi/envs/default/bin/jupyter" default-directory))))
    (when (and pixi-jupyter (file-executable-p pixi-jupyter))
      pixi-jupyter)))

;; Function to update jupyter paths for current project
(defun update-jupyter-paths ()
  "Update jupyter paths for current project."
  (when-let ((jupyter-path (find-pixi-jupyter)))
    (setq-local jupyter-command jupyter-path
                jupyter-executable jupyter-path
                org-babel-jupyter-command jupyter-path)
    (message "Updated jupyter path to: %s" jupyter-path)))

;; Hook to update jupyter paths when entering a directory
(add-hook 'find-file-hook #'update-jupyter-paths)

;; Jupyter configuration  
(use-package! jupyter
  :defer t
  :commands (jupyter-run-repl jupyter-connect-repl jupyter-available-kernelspecs
             jupyter-repl-associate-buffer org-babel-execute:jupyter)
  :init
  ;; Skip ZMQ loading in init to test if it's causing conflicts
  (log-to-file "Jupyter package init: Skipping ZMQ for testing")
  ;; Add autoloads for jupyter functions
  (autoload 'jupyter-run-repl "jupyter" "Run a Jupyter REPL" t)
  (autoload 'jupyter-connect-repl "jupyter" "Connect to a Jupyter kernel" t)
  (log-to-file "Jupyter package init: Autoloads added")
  :config
  ;; Force emacs-jupyter to use nix-provided ZMQ, don't build it
  (setq jupyter-use-zmq nil) ; Disable ZMQ module loading entirely
  (log-to-file "Jupyter package config: jupyter-use-zmq set to nil (using nix zmq)")
  
  ;; Explicitly load zmq module from nix
  (condition-case err
      (progn
        (require 'zmq)
        (log-to-file "Successfully loaded nix-provided zmq module"))
    (error
     (log-to-file (format "Failed to load zmq: %s" err))))
  
  ;; Set multiple possible variable names for jupyter executable
  (let ((jupyter-path (or (find-pixi-jupyter)
                         "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter"
                         "jupyter")))
    (setq jupyter-command jupyter-path
          jupyter-executable jupyter-path
          org-babel-jupyter-command jupyter-path
          python-shell-interpreter "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/python"))
  
  ;; Advice to ensure jupyter-run-repl uses the correct path
  (defun jupyter-ensure-executable (orig-fun &rest args)
    "Ensure jupyter executable is available before running jupyter commands."
    (let ((jupyter-path (find-pixi-jupyter)))
      (when jupyter-path
        (setq jupyter-command jupyter-path
              jupyter-executable jupyter-path))
      (apply orig-fun args)))
  
  ;; Remove any advice functions that might cause hot loading conflicts
  (when (fboundp 'jupyter-run-repl)
    (advice-remove 'jupyter-run-repl #'jupyter-ensure-executable)
    (advice-remove 'jupyter-run-repl #'debug-jupyter-run-repl)
    (log-to-file "Removed advice from jupyter-run-repl"))
  
  ;; Ensure REPL buffer is displayed properly
  (setq jupyter-repl-echo-eval-p t)
  (setq jupyter-repl-prompt-margin-width 4)
  
  ;; Debug function to check REPL status
  (defun debug-jupyter-repl ()
    "Debug jupyter REPL status"
    (interactive)
    (message "Current client: %s" jupyter-current-client)
    (message "All buffers: %s" (mapcar #'buffer-name (buffer-list)))
    (message "Jupyter buffers: %s" (seq-filter (lambda (b) (string-match-p "jupyter" (buffer-name b))) (buffer-list))))
  
  ;; Add hook to ensure buffer is displayed
  (add-hook 'jupyter-repl-mode-hook
            (lambda ()
              (message "REPL buffer created: %s" (buffer-name))
              (switch-to-buffer (current-buffer))))
  
  ;; Alternative function to start jupyter with explicit kernel
  (defun start-jupyter-r-repl ()
    "Start Jupyter R REPL with explicit configuration"
    (interactive)
    (let ((jupyter-command (or (find-pixi-jupyter) "jupyter")))
      (message "Using jupyter: %s" jupyter-command)
      (jupyter-run-repl "ir" nil t)))
  
  ;; Simple function to show jupyter buffers
  (defun show-jupyter-buffers ()
    "Show all jupyter-related buffers"
    (interactive)
    (let ((jupyter-buffers (seq-filter (lambda (b) 
                                       (string-match-p "jupyter\\|repl" (buffer-name b))) 
                                     (buffer-list))))
      (message "Jupyter buffers: %s" 
               (mapcar #'buffer-name jupyter-buffers))))
  
  ;; Simple function to connect to existing kernel
  (defun connect-to-kernel ()
    "Connect to existing kernel"
    (interactive)
    (jupyter-connect-repl "/Users/vwh7mb/Library/Jupyter/runtime/kernel-529.json"))
  
  ;; Load jupyter org-babel support
  (condition-case err
      (progn
        (require 'ob-jupyter)
        (log-to-file "ob-jupyter loaded successfully"))
    (error 
     (log-to-file (format "Failed to load ob-jupyter: %s" err))))
  
  ;; Configure org-babel integration after jupyter is loaded
  (with-eval-after-load 'org
    (log-to-file "Configuring org-babel languages")
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (R . t)
       (jupyter . t)))
    (log-to-file "org-babel languages configured")
    
    ;; Remove any existing advice that might be causing conflicts
    (when (fboundp 'org-babel-execute:jupyter)
      (advice-remove 'org-babel-execute:jupyter #'jupyter-ensure-executable)
      (log-to-file "Removed potentially conflicting advice from org-babel-execute:jupyter")))
  
  ) ; End use-package! jupyter

;; Set default jupyter kernels
(setq org-babel-default-header-args:jupyter '((:async . "yes")
                                              (:session . "py") 
                                              (:kernel . "python")))

;; keybindings
(load! "bindings")

;; Testing utilities (uncomment to test different Unicode approaches)
;; (load! "test-unicode-fix")
