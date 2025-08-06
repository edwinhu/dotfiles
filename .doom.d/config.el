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
  
  ;; Fix display issues in eat terminal
  (add-hook 'eat-mode-hook
            (lambda ()
              ;; Disable whitespace visualization
              (whitespace-mode -1)
              (setq-local show-trailing-whitespace nil)
              (setq-local nobreak-char-display nil)
              ;; Create buffer display table to replace problematic Unicode ranges
              (let ((dt (or buffer-display-table (make-display-table))))
                ;; Claude Code commonly uses these Unicode ranges that get emoji-fied:
                ;; Miscellaneous Symbols (U+2600-U+26FF) - warnings, stars, etc.
                (dotimes (i (- #x26FF #x2600))
                  (let ((char (+ #x2600 i)))
                    (aset dt char [?*])))  ; Replace with asterisk
                ;; Dingbats (U+2700-U+27BF) - check marks, crosses, etc.
                (dotimes (i (- #x27BF #x2700))
                  (let ((char (+ #x2700 i)))
                    (aset dt char [?+])))  ; Replace with plus
                ;; Miscellaneous Symbols and Arrows (U+2B00-U+2BFF) - more arrows/symbols
                (dotimes (i (- #x2BFF #x2B00))
                  (let ((char (+ #x2B00 i)))
                    (aset dt char [?>])))  ; Replace with arrow
                ;; Emoticons range (U+1F600-U+1F64F) and other emoji blocks
                (dotimes (i (- #x1F64F #x1F600))
                  (let ((char (+ #x1F600 i)))
                    (aset dt char [?:])))  ; Replace with colon
                ;; Miscellaneous Symbols and Pictographs (U+1F300-U+1F5FF)
                (dotimes (i (- #x1F5FF #x1F300))
                  (let ((char (+ #x1F300 i)))
                    (aset dt char [?#])))  ; Replace with hash
                ;; Keep specific useful ones - just the record symbol
                (aset dt ?⏺ [?●])   ; Record symbol U+23FA
                (setq-local buffer-display-table dt))))

  ;; Optional keybindings
  :bind (("C-c t" . eat)
         ("C-c T" . eat-other-window))
)

;; Replace vterm with eat
(setq +term-backend 'eat)


;; Claude Code
(use-package claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'eat))

;; org-roam
;; (setq org-roam-directory "~/org-roam")

;; Include SAS support
(use-package! ob-sas
  :load-path "~/.doom.d/")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sas . t)
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

;; keybindings
(load! "bindings")
