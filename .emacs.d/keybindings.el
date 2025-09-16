;;; keybindings.el --- LazyVim-inspired keybindings for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; LazyVim-inspired keybindings using general.el
;; Organized by functionality categories

;;; Code:

;; Require general.el (leader definitions are in init.el)
(require 'general)

;;; Window Management (LazyVim-inspired)
;; Use Ctrl+hjkl for window navigation like LazyVim
(general-define-key
 :states '(normal insert visual emacs)
 "C-h" 'windmove-left
 "C-j" 'windmove-down
 "C-k" 'windmove-up
 "C-l" 'windmove-right
 "C-<up>" 'enlarge-window
 "C-<down>" 'shrink-window
 "C-<left>" 'shrink-window-horizontally
 "C-<right>" 'enlarge-window-horizontally)

;;; Buffer Management (LazyVim-inspired)
;; Use Shift+hl for buffer navigation like LazyVim
(general-define-key
 :states '(normal insert visual emacs)
 "S-h" 'previous-buffer
 "S-l" 'next-buffer)

;;; Basic Editing (LazyVim-inspired)
(general-define-key
 :states '(normal insert visual emacs)
 "C-s" 'save-buffer)

;;; Leader Key Bindings

;; Core commands
(my-leader-def
  "SPC" '(execute-extended-command :which-key "M-x")

  ;; Files (LazyVim-inspired)
  "f"   '(:ignore t :which-key "Files")
  "ff"  '(find-file :which-key "Find files")
  "fn"  '(find-file :which-key "New file")
  "fr"  '(recentf-open-files :which-key "Recent files")
  "fs"  '(save-buffer :which-key "Save file")
  "fS"  '(save-some-buffers :which-key "Save all files")
  "fc"  '(open-config :which-key "Find config file")
  "fC"  '(open-config-dir :which-key "Open config dir")

  ;; Buffers (LazyVim-inspired)
  "b"   '(:ignore t :which-key "Buffers")
  "bb"  '(switch-to-buffer :which-key "Switch to other buffer")
  "bd"  '(kill-current-buffer :which-key "Delete buffer")
  "bD"  '(kill-buffer :which-key "Delete buffer (choose)")
  "bo"  '(kill-other-buffers :which-key "Delete other buffers")
  "br"  '(revert-buffer :which-key "Revert buffer")

  ;; Windows (LazyVim-inspired)
  "w"   '(:ignore t :which-key "Windows")
  "wv"  '(split-window-right :which-key "Split vertical")
  "ws"  '(split-window-below :which-key "Split horizontal")
  "wd"  '(delete-window :which-key "Delete window")
  "wo"  '(delete-other-windows :which-key "Delete other windows")
  "wm"  '(delete-other-windows :which-key "Toggle zoom/maximize")
  "wh"  '(windmove-left :which-key "Move left")
  "wj"  '(windmove-down :which-key "Move down")
  "wk"  '(windmove-up :which-key "Move up")
  "wl"  '(windmove-right :which-key "Move right")

  ;; Search (LazyVim-inspired)
  "/"   '(consult-ripgrep :which-key "Grep (Root Dir)")
  "s"   '(:ignore t :which-key "Search")
  "ss"  '(consult-line :which-key "Search line")
  "sg"  '(consult-ripgrep :which-key "Grep (Root Dir)")
  "sw"  '(consult-ripgrep :which-key "Search word")
  "sk"  '(describe-key :which-key "Keymaps")
  "sh"  '(describe-function :which-key "Help pages")
  "sr"  '(consult-ripgrep :which-key "Ripgrep")
  "si"  '(consult-imenu :which-key "Imenu")
  "sb"  '(consult-buffer :which-key "Switch buffer")
  "sf"  '(consult-find :which-key "Find file")
  "so"  '(consult-outline :which-key "Outline")

  ;; Git (LazyVim-inspired)
  "g"   '(:ignore t :which-key "Git")
  "gg"  '(magit-status :which-key "Git status")
  "gs"  '(magit-status :which-key "Git status")
  "gd"  '(magit-diff :which-key "Git diff")
  "gb"  '(magit-blame :which-key "Git blame line")
  "gc"  '(magit-commit :which-key "Git commit")
  "gp"  '(magit-push :which-key "Git push")
  "gf"  '(magit-fetch :which-key "Git fetch")
  "gl"  '(magit-log :which-key "Git log")

  ;; Code (LazyVim-inspired)
  "c"   '(:ignore t :which-key "Code")
  "cf"  '(format-buffer :which-key "Format")
  "ca"  '(eglot-code-actions :which-key "Code action")
  "cr"  '(eglot-rename :which-key "Rename")

  ;; Help (LazyVim-inspired)
  "h"   '(:ignore t :which-key "Help")
  "hf"  '(describe-function :which-key "Describe function")
  "hv"  '(describe-variable :which-key "Describe variable")
  "hk"  '(describe-key :which-key "Describe key")
  "hm"  '(describe-mode :which-key "Describe mode")

  ;; Quit (LazyVim-inspired)
  "q"   '(:ignore t :which-key "Quit")
  "qq"  '(save-buffers-kill-terminal :which-key "Quit all")
  "qQ"  '(kill-emacs :which-key "Force quit Emacs")

  ;; System
  "r"   '(reload-config :which-key "Reload config")
  "R"   '(revert-buffer :which-key "Reload file"))

;; Clear search highlight in normal mode (LazyVim-inspired)
;; Use a different key since escape is critical for Evil mode
(my-leader-def
  "<escape>" (lambda () (interactive) (evil-ex-nohighlight)))

;;; Helper Functions

(defun kill-other-buffers ()
  "Kill all buffers except current one."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (unless (or (eq buffer current-buffer)
                  (string-match "^\\*" (buffer-name buffer)))
        (kill-buffer buffer))))
  (message "Killed other buffers"))

(defun format-buffer ()
  "Format the current buffer."
  (interactive)
  (cond
   ((bound-and-true-p eglot--managed-mode)
    (eglot-format-buffer))
   (t
    (indent-region (point-min) (point-max))
    (message "Buffer formatted"))))

;;; Enhanced Auto-Reload (LazyVim-style)

;; Configure auto-revert for LazyVim-like behavior
(with-eval-after-load 'autorevert
  (setq auto-revert-interval 1              ; Check every 1 second (faster than default 5s)
        auto-revert-check-vc-info t          ; Also revert VC info
        auto-revert-remote-files t           ; Revert remote files too
        auto-revert-verbose nil              ; Don't show revert messages
        auto-revert-mode-text ""             ; Don't show "AR" in modeline
        auto-revert-tail-mode-text ""))      ; Don't show "Tail" in modeline

;; Enhanced revert function that shows what changed (LazyVim-style notification)
(defun enhanced-auto-revert-handler ()
  "Enhanced auto-revert with LazyVim-style notifications."
  (when (and (buffer-file-name)
             (not (buffer-modified-p))
             (file-exists-p (buffer-file-name)))
    (let ((file-time (nth 5 (file-attributes (buffer-file-name))))
          (buffer-time (visited-file-modtime)))
      (when (time-less-p buffer-time file-time)
        (let ((old-point (point))
              (old-line (line-number-at-pos)))
          (revert-buffer t t t)
          (goto-char old-point)
          (message "File reloaded: %s (line %d)"
                   (file-name-nondirectory (buffer-file-name))
                   old-line))))))

;; Hook for focus events (like LazyVim's FocusGained)
(defun check-buffer-on-focus ()
  "Check and reload buffer when Emacs gains focus (LazyVim-style)."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (buffer-file-name)
                 (not (buffer-modified-p))
                 (file-exists-p (buffer-file-name)))
        (auto-revert-handler)))))

;; Add focus hooks (when available)
(when (fboundp 'focus-in-hook)
  (add-hook 'focus-in-hook 'check-buffer-on-focus))

;; For terminals or when focus hooks aren't available
(when (not window-system)
  (run-with-timer 2 2 'check-buffer-on-focus))

(provide 'keybindings)
;;; keybindings.el ends here