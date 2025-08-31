;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Load required modules for SAS euporie integration
(let ((doom-dir (or (bound-and-true-p doom-user-dir)
                   (expand-file-name "~/.doom.d/"))))
  ;; Load TRAMP WRDS support first (required by euporie-termint)
  (load (expand-file-name "tramp-wrds.el" doom-dir))
  ;; Load termint org-src integration
  (load (expand-file-name "termint-org-src.el" doom-dir))
  ;; Load euporie-termint module for native euporie graphics integration
  (load (expand-file-name "euporie-termint.el" doom-dir)))

;; Ensure euporie-termint-display-console-right function is available
;; (workaround for loading issues)
(unless (fboundp 'euporie-termint-display-console-right)
  (defun euporie-termint-display-console-right (buffer &optional original-buffer original-window)
    "Display console BUFFER in a right split window."
    (let ((initial-window (or original-window (selected-window)))
          (initial-buffer (or original-buffer (current-buffer))))
      
      ;; Display buffer in right side window
      (let ((console-window (display-buffer buffer
                                            '((display-buffer-reuse-window
                                               display-buffer-in-side-window)
                                              (side . right)
                                              (window-width . 0.5)
                                              (inhibit-same-window . t)))))
        
        (when console-window
          ;; Scroll to bottom in console
          (with-selected-window console-window
            (goto-char (point-max)))
          
          ;; Restore focus to original window
          (when (window-live-p initial-window)
            (select-window initial-window))
          
          (when (buffer-live-p initial-buffer)
            (set-window-buffer (selected-window) initial-buffer)))))))

;; Initialize euporie-termint immediately after loading
(when (and (featurep 'euporie-termint)
           (fboundp 'euporie-termint-setup) 
           (fboundp 'euporie-termint-setup-keybinding))
  (euporie-termint-setup)
  (euporie-termint-setup-keybinding))

;; Setup euporie-termint integration
(with-eval-after-load 'org
  (message "Setting up euporie-termint integration")
  (when (featurep 'euporie-termint)
    (message "euporie-termint functions available: %s"
             (mapcar (lambda (f) (if (fboundp f) f (format "MISSING-%s" f)))
                     '(euporie-python-start euporie-r-start euporie-stata-start)))
    ;; Initialize euporie-termint after confirming it's loaded
    (when (and (fboundp 'euporie-termint-setup) (fboundp 'euporie-termint-setup-keybinding))
      (euporie-termint-setup)
      (euporie-termint-setup-keybinding))))

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

;; eat terminal
(use-package eat
  :config
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)

  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  ;; Use eat's native terminal type with graphics support
  (setq eat-term-name 'eat-term-get-suitable-term-name)
  
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
              (setq-local nobreak-char-display nil)))
  
  ;; Better integration with TRAMP and remote sessions
  (setq vterm-max-scrollback 10000)  ; Increase scrollback for long sessions
  (setq vterm-kill-buffer-on-exit t) ; Clean up buffers when sessions end
  )

;; Use eat for sixel graphics support (required for euporie inline graphics)
(setq +term-backend 'eat)

;; Prevent global whitespace mode issues in terminals
(after! whitespace
  ;; Ensure whitespace mode doesn't interfere with terminals
  (setq whitespace-global-modes '(not vterm-mode eat-mode term-mode)))

;; Termint configuration for REPL integration with eat (for sixel graphics)
(use-package termint
  :demand t
  :config
  ;; Use eat as backend for sixel graphics support
  (setq termint-backend 'eat))

;; Include SAS support
(use-package! ob-sas
  :load-path "~/.doom.d/"
  :config
  ;; Map 'sas' language to SAS-mode for org-src blocks
  (add-to-list 'org-src-lang-modes '("sas" . SAS)))

;; Include Stata support
(use-package! ob-stata
  :load-path "~/.doom.d/")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sas . t)
   (R . t)
   (stata . t)))

;; Edit in same window
;; Doom now treats these buffers as pop-ups
;; which breaks the default behavior unless you tell it to ignore
(after! org
  (set-popup-rule! "^\\*Org Src" :ignore t)
  (set-popup-rule! "^\\*SAS Console" :ignore t))
(after! vterm
  (set-popup-rule! "^\\*vterm " :ignore t))
(setq org-src-window-setup 'current-window)

;; use tectonic for pdf processing as it is faster
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

;; Direnv integration for environment management
(use-package! envrc
  :config
  (envrc-global-mode))

;; Function to find jupyter in pixi environment - kept for compatibility
(defun find-pixi-jupyter ()
  "Find jupyter executable in current pixi environment."
  (let* ((default-directory (locate-dominating-file default-directory ".envrc"))
         (pixi-jupyter (when default-directory
                        (expand-file-name ".pixi/envs/default/bin/jupyter" default-directory))))
    (when (and pixi-jupyter (file-executable-p pixi-jupyter))
      pixi-jupyter)))

;; keybindings
(load! "bindings")