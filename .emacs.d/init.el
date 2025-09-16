;;; init.el --- Personal Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Modern vanilla Emacs configuration with Evil mode and leader key system

;;; Code:

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; Set user information
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Basic UI and behavior settings
(setq-default
 cursor-type 'bar                        ; Bar cursor
 fill-column 80                          ; Default line width
 tab-width 4                             ; Tab width
 indent-tabs-mode nil                    ; Use spaces instead of tabs
 require-final-newline t                 ; Add newline at end of file
 ring-bell-function 'ignore              ; Disable bell
 backup-directory-alist '(("." . "~/.cache/emacs/backups")) ; Backup location
 auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-save-list/" t)) ; Auto-save location
 )

;; Create cache directories
(make-directory "~/.cache/emacs/backups" t)
(make-directory "~/.cache/emacs/auto-save-list" t)

;; Enable useful defaults
(fset 'yes-or-no-p 'y-or-n-p)           ; Use y/n instead of yes/no
(global-auto-revert-mode 1)             ; Automatically reload files (LazyVim-style)
(delete-selection-mode 1)               ; Replace selected text when typing
(electric-pair-mode 1)                  ; Auto-pair parentheses
(show-paren-mode 1)                     ; Highlight matching parentheses
(column-number-mode 1)                  ; Show column number in mode line
(global-display-line-numbers-mode 1)    ; Show line numbers
(savehist-mode 1)                       ; Save minibuffer history

;; Font configuration - preserve JetBrains Mono setup like Doom
(when (display-graphic-p)
  ;; CRITICAL: Allow fontset configuration to work (from Doom config)
  (setq use-default-font-for-symbols nil
        inhibit-compacting-font-caches t)

  ;; Main font (JetBrains Mono)
  (set-face-attribute 'default nil
                      :font (font-spec :family "JetBrains Mono"
                                      :size 13.0
                                      :dpi 96
                                      :weight 'regular))

  ;; Variable pitch font (CMU Serif)
  (set-face-attribute 'variable-pitch nil
                      :font (font-spec :family "CMU Serif"
                                      :size 13.0
                                      :dpi 96)))

;; Theme setup - install and load Catppuccin
(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin :no-confirm))

;; Evil mode - Vim emulation
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)

  ;; LazyVim-style cursor shapes (compatible with terminal and GUI)
  (setq evil-normal-state-cursor '(box . 2)        ; Block cursor for normal mode
        evil-insert-state-cursor '(bar . 2)        ; Line cursor for insert mode
        evil-visual-state-cursor '(hollow . 2)     ; Hollow block for visual mode
        evil-replace-state-cursor '(hbar . 2)      ; Horizontal bar for replace mode
        evil-operator-state-cursor '(hollow . 2)   ; Hollow for operator-pending
        evil-motion-state-cursor '(box . 2))       ; Block for motion state

  ;; Set initial states for buffers
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Evil Collection - Better Evil integration for other modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Which-key - Show available keybindings
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0.05
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-show-prefix 'echo
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → "))

;; General - Leader key system
(use-package general
  :config
  (general-evil-setup)

  ;; Define leader key
  (general-create-definer my-leader-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  ;; Define local leader key
  (general-create-definer my-local-leader-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ","
    :non-normal-prefix "M-,"))

;; Org mode configuration
(use-package org
  :config
  (setq org-directory "~/org/"
        org-agenda-files '("~/org/")
        org-default-notes-file (concat org-directory "notes.org")

        ;; Better org bullets
        org-hide-emphasis-markers t
        org-startup-indented t
        org-pretty-entities t

        ;; Source code blocks
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-support-shift-select 'always

        ;; Babel configuration
        org-babel-load-languages '((emacs-lisp . t)
                                   (python . t)
                                   (R . t)
                                   (stata . t)))

  ;; Create org directory if it doesn't exist
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))

  ;; Local leader bindings for org-mode
  (my-local-leader-def
    :keymaps 'org-mode-map
    "e" '(org-export-dispatch :which-key "Export")
    "t" '(org-todo :which-key "Toggle TODO")
    "s" '(org-schedule :which-key "Schedule")
    "d" '(org-deadline :which-key "Deadline")
    "a" '(org-agenda :which-key "Agenda")
    "c" '(org-capture :which-key "Capture")
    "l" '(org-store-link :which-key "Store link")
    "i" '(org-insert-link :which-key "Insert link")))

;; Vertico - Better minibuffer completion
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

;; Orderless - Flexible completion style
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia - Rich annotations in minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Consult - Enhanced search and navigation
(use-package consult)

;; Magit - Git integration
(use-package magit)

;; VTerm - Better terminal emulator
(use-package vterm
  :config
  (setq vterm-environment '("TERM=xterm-256color"
                           "COLORTERM=truecolor")))

;; Eat - Another terminal emulator option
(use-package eat
  :config
  (setq eat-term-name "xterm-256color"))

;; Exec path from shell - Ensure Emacs inherits shell PATH
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; Termint - Terminal integration
(use-package termint
  :straight (:host github :repo "milanglacier/termint.el")
  :config
  (setq termint-backend 'eat))

;; Load custom configurations after dependencies are ready
(let ((config-dir user-emacs-directory))
  ;; Load keybindings configuration
  (load (expand-file-name "keybindings.el" config-dir) t)

  ;; Load euporie-termint integration
  (load (expand-file-name "euporie-termint.el" config-dir) t)

  ;; Load SAS org-babel support
  (load (expand-file-name "ob-sas.el" config-dir) t)

  ;; Load Stata org-babel support
  (load (expand-file-name "ob-stata.el" config-dir) t)

  ;; Load Claude Code IDE configuration
  (load (expand-file-name "claude-code-config.el" config-dir) t))

;; Dashboard - Modern splash screen
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)

  ;; Dashboard configuration
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-items '((recents . 5)
                         (projects . 5)
                         (bookmarks . 5)
                         (agenda . 5))

        ;; Custom dashboard content
        dashboard-banner-logo-title "Welcome to Emacs!"
        dashboard-footer-messages '("Happy coding! 🎉")
        dashboard-footer-icon (if (display-graphic-p) "🚀" ""))

  ;; Set dashboard as initial buffer
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  ;; Custom dashboard sections
  (defun dashboard-insert-custom-keybindings (list-size)
    "Insert custom keybindings section."
    (dashboard-insert-heading "Quick Actions:")
    (insert "\n")
    (widget-create 'item
                   :tag "  SPC f f"
                   :action '(lambda (&rest ignore) (call-interactively 'find-file))
                   :mouse-face 'highlight
                   :button-face 'dashboard-text-banner-face
                   :help-echo "Find file"
                   :format "%[%t%]")
    (insert " → Find file\n")

    (widget-create 'item
                   :tag "  SPC b b"
                   :action '(lambda (&rest ignore) (call-interactively 'switch-to-buffer))
                   :mouse-face 'highlight
                   :button-face 'dashboard-text-banner-face
                   :help-echo "Switch buffer"
                   :format "%[%t%]")
    (insert " → Switch buffer\n")

    (widget-create 'item
                   :tag "  SPC g g"
                   :action '(lambda (&rest ignore) (call-interactively 'magit-status))
                   :mouse-face 'highlight
                   :button-face 'dashboard-text-banner-face
                   :help-echo "Git status"
                   :format "%[%t%]")
    (insert " → Git status\n")

    (widget-create 'item
                   :tag "  SPC /"
                   :action '(lambda (&rest ignore) (call-interactively 'consult-ripgrep))
                   :mouse-face 'highlight
                   :button-face 'dashboard-text-banner-face
                   :help-echo "Grep (Root Dir)"
                   :format "%[%t%]")
    (insert " → Grep (Root Dir)\n")

    (widget-create 'item
                   :tag "  SPC r  "
                   :action '(lambda (&rest ignore) (call-interactively 'reload-config))
                   :mouse-face 'highlight
                   :button-face 'dashboard-text-banner-face
                   :help-echo "Reload config"
                   :format "%[%t%]")
    (insert " → Reload config\n\n"))

  ;; Add custom section to dashboard
  (add-to-list 'dashboard-item-generators '(keybindings . dashboard-insert-custom-keybindings))
  (setq dashboard-items '((keybindings . t)
                         (recents . 5)
                         (projects . 5)
                         (bookmarks . 3)))

  ;; Dashboard in emacsclient
  (setq dashboard-force-refresh t))

;; Custom functions
(defun reload-config ()
  "Reload Emacs configuration."
  (interactive)
  (load-file user-init-file)
  (message "Configuration reloaded!"))

(defun open-config ()
  "Open Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defun open-config-dir ()
  "Open Emacs configuration directory."
  (interactive)
  (dired user-emacs-directory))


;;; init.el ends here
