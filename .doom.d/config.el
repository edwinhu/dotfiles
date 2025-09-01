;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; ================================================================
;;;                STREAMLINED EUPORIE INTEGRATION ARCHITECTURE
;;; ================================================================
;; This section loads the two core modules for unified euporie integration:
;;   1. tramp-qrsh.el    - Simplified qrsh compute node sessions  
;;   2. euporie-termint.el - Unified euporie console integration
;;
;; All languages (Python, R, Stata, SAS) use IDENTICAL workflows through
;; universal functions with automatic local/remote routing.

(let ((doom-dir (or (bound-and-true-p doom-user-dir)
                   (expand-file-name "~/.doom.d/"))))
  ;; Load simplified TRAMP qrsh support first
  (load (expand-file-name "tramp-qrsh.el" doom-dir))
  ;; Load unified euporie-termint module  
  (load (expand-file-name "euporie-termint.el" doom-dir)))

;; Initialize euporie-termint immediately after loading
(when (and (featurep 'euporie-termint)
           (fboundp 'euporie-termint-setup) 
           (fboundp 'euporie-termint-setup-keybindings))
  (euporie-termint-setup)
  (euporie-termint-setup-keybindings))

;; Setup euporie-termint integration for org-mode  
(with-eval-after-load 'org
  (message "Setting up unified euporie-termint integration for org-mode")
  (when (featurep 'euporie-termint)
    (message "euporie-termint unified functions available for all languages")
    ;; Ensure initialization is complete
    (when (and (fboundp 'euporie-termint-setup) (fboundp 'euporie-termint-setup-keybindings))
      (euporie-termint-setup)
      (euporie-termint-setup-keybindings))))

;; ================================================================
;;;                     ESSENTIAL ORG-BABEL CONFIGURATION
;;; ================================================================

;; Load org-syntax-overlay module - WORKING fix for syntax highlighting in Doom Emacs  
(load! "org-syntax-overlay")

;; Load ob-sas and ob-stata packages - CRITICAL for org-babel functionality
(use-package! ob-sas
  :load-path "~/.doom.d/")

(use-package! ob-stata  
  :load-path "~/.doom.d/")

;; Language mode mappings and syntax highlighting configuration
(after! org
  ;; ESSENTIAL org-src settings - handled by org-syntax-fix module
  ;; These are kept here for reference but will be overridden by the fix module
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t) 
  (setq org-src-tab-acts-natively t)
  
  ;; Language mode mappings - essential for org-babel integration
  (add-to-list 'org-src-lang-modes '("python" . python))
  (add-to-list 'org-src-lang-modes '("R" . r))
  (add-to-list 'org-src-lang-modes '("sas" . SAS))
  (add-to-list 'org-src-lang-modes '("stata" . stata))
  
  ;; Org-babel language support
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (python . t)
                                   (R . t)
                                   (stata . t)
                                   (sas . t))))

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
(setq doom-font (font-spec :family "JetBrains Mono" :size 13.0 :dpi 96 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "CMU Serif" :size 13.0 :dpi 96))

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
;; - `add-hook!' for adding functions to hooks
;; - `quiet!' for suppressing output generated by a call to a function
;; - `pushnew!' for adding a new element to a list, avoiding duplicates
;; - `delq!' for deleting elements from lists
;; - `delete!' for deleting files or directories
;; - `define-key!' for binding keys
;; - `undefine-key!' for unbinding keys

;;; ================================================================
;;;                    UNIVERSAL TEST FUNCTIONS KEYBINDINGS
;;; ================================================================
;; Global keybindings for testing any kernel using universal functions

(map! "C-c e t" #'test-euporie-integration    ; Run comprehensive test suite
      "C-c e l" #'test-local-execution       ; Test any kernel local execution
      "C-c e r" #'test-remote-execution      ; Test remote execution (SAS)
      "C-c e w" #'test-window-management     ; Test window management
      "C-c e k" #'test-keybinding-dispatch   ; Test keybinding dispatch
      "C-c e g" #'test-graphics-display      ; Test graphics display
      "C-c s t" #'org-syntax-overlay-test)     ; Test working syntax highlighting fix

;; Accept completion from copilot and fallback to company
;; DISABLED temporarily due to loading issues
;; (use-package! copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; Load LSP configurations
(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  ;; Increase the amount of data which Emacs reads from the process
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;; Increase the amount of data allowed from LSP process
  (setq lsp-log-io nil)
  (setq lsp-print-performance nil))

;; Python LSP setup
(after! lsp-pyright
  (setq lsp-pyright-langserver-command "pyright")
  (setq lsp-pyright-multi-root nil))

;; R LSP setup
(use-package! ess
  :config
  (setq ess-use-flymake nil)
  (setq ess-use-eldoc 'script-only)
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t))))

;; Stata mode setup
(use-package! stata-mode
  :mode "\\.do\\'")

;; SAS mode setup  
(use-package! ess
  :config
  (require 'ess-sas-l)
  ;; Create SAS-mode alias for org-babel integration
  (unless (fboundp 'SAS-mode)
    (defalias 'SAS-mode 'ess-sas-mode)))

;; Markdown setup
(after! markdown-mode
  (setq markdown-command "multimarkdown"))

;; Org mode customizations - consolidated with syntax fix
(after! org
  (setq org-directory "~/org/"
        org-agenda-files (list org-directory)
        org-default-notes-file (concat org-directory "/notes.org")
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷")
        org-superstar-prettify-item-bullets t
        org-log-done 'time
        org-hide-emphasis-markers t
        org-edit-src-content-indentation 0)
  
  ;; CRITICAL: Popup rules for org-src buffers - required for org-edit-src-code
  (set-popup-rule! "^\\*Org Src" :ignore t)
  (set-popup-rule! "^\\*SAS Console" :ignore t)
  
  ;; Essential org-src window setup - prevents "marker does not point anywhere" error
  (setq org-src-window-setup 'current-window)
  
  ;; Additional essential org-mode settings
  (setq org-support-shift-select 'always)
  
  ;; NOTE: org-src syntax highlighting settings are handled by org-syntax-fix.el
  ;; to prevent conflicts with Doom's default configuration
  )

;; Projectile setup
(after! projectile
  (setq projectile-project-search-path '("~/projects/")
        projectile-switch-project-action #'projectile-dired))

;; Dired setup
(after! dired
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t))

;; Which-key setup
(after! which-key
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.01)
  (which-key-mode))

;; Company mode setup
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

;; Flycheck setup
(use-package! flycheck
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8))

;; Git setup
(after! magit
  (setq magit-repository-directories '(("~/projects" . 2))
        magit-save-repository-buffers nil
        magit-inhibit-save-previous-winconf t))

;; Load org-syntax-overlay for automatic syntax highlighting in org-mode
;; (already loaded at line 44)

