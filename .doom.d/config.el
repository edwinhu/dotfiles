;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
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

;;; ================================================================
;;;                STREAMLINED EUPORIE INTEGRATION ARCHITECTURE
;;; ================================================================

;; Hook into org-mode-hook to ensure configuration loads when org-mode activates
(add-hook 'org-mode-hook 
  (lambda ()
    
    (use-package! euporie-termint :load-path "~/.doom.d/")
    (use-package! tramp-sh :load-path "~/.doom.d/")
    ;; Load ob-sas and ob-stata packages immediately 
    (use-package! ob-sas :load-path "~/.doom.d/")
    (use-package! ob-stata :load-path "~/.doom.d/")
    ; Initialize euporie-termint when org-mode loads
    (when (and (featurep 'euporie-termint)
               (fboundp 'euporie-termint-setup) 
               (fboundp 'euporie-termint-setup-keybindings))
      (euporie-termint-setup)
      (euporie-termint-setup-keybindings))
    
    ;; Essential org-babel configuration
    (setq org-src-fontify-natively t
          org-src-preserve-indentation t 
          org-src-tab-acts-natively t)
    

    ;; Org-babel language support
    (setq org-babel-load-languages '((emacs-lisp . t)
                                     (python . t)
                                     (R . t)
                                     (stata . t)
                                     (sas . t)))
    
    ;; Essential org-src window setup
    (setq org-src-window-setup 'current-window
          org-support-shift-select 'always)))

;; Doom-specific configuration that needs to run after doom loads
(after! org
  ;; CRITICAL: Popup rules for org-src buffers - required for org-edit-src-code
  (set-popup-rule! "^\\*Org Src" :ignore t)
  (set-popup-rule! "^\\*SAS Console" :ignore t)
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
