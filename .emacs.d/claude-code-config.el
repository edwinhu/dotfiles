;;; claude-code-config.el --- Claude Code IDE configuration for vanilla Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Claude Code IDE configuration for vanilla Emacs without Doom dependencies

;;; Code:

;; Claude Code IDE integration
(use-package claude-code-ide
  :straight (:host github :repo "manzaltu/claude-code-ide.el")
  :config
  ;; Display Claude Code buffers in right side window
  (add-to-list 'display-buffer-alist
               '("^\\*claude"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 90)))

  ;; Disable visual modes that interfere with Claude Code IDE terminal
  (defun claude-code-ide-buffer-setup ()
    "Setup Claude Code IDE buffer configuration."
    (when (string-match-p "^\\*claude" (buffer-name))
      ;; Disable whitespace visualization that can interfere with Unicode
      (when (bound-and-true-p whitespace-mode)
        (whitespace-mode -1))
      ;; Disable line numbers
      (when (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1))
      ;; Disable trailing whitespace visualization
      (setq-local show-trailing-whitespace nil)
      ;; Disable non-breaking char display that can interfere with Unicode
      (setq-local nobreak-char-display nil)))

  (add-hook 'buffer-list-update-hook #'claude-code-ide-buffer-setup)
  )

;; Add to leader key bindings - outside use-package to ensure they load immediately
(when (fboundp 'my-leader-def)
  (my-leader-def
    "a"   '(:ignore t :which-key "Claude Code")
    "ac"  '(claude-code-ide :which-key "Start Claude Code IDE")
    "ad"  '(claude-code-ide-stop :which-key "Stop Claude Code IDE")
    "ar"  '(claude-code-ide-resume :which-key "Resume Claude Code IDE")
    "as"  '(claude-code-ide-send-prompt :which-key "Send prompt")
    "at"  '(claude-code-ide-toggle :which-key "Toggle Claude Code window")
    "ab"  '(claude-code-ide-switch-to-buffer :which-key "Switch to Claude buffer")
    "am"  '(claude-code-ide-menu :which-key "Claude Code IDE menu")

    ;; YOLO mode submenu
    "ay"  '(:ignore t :which-key "YOLO mode")
    "ays" '(claude-code-ide-yolo-start :which-key "YOLO start")
    "ayc" '(claude-code-ide-yolo-continue :which-key "YOLO continue")
    "ayr" '(claude-code-ide-yolo-resume :which-key "YOLO resume")))

(provide 'claude-code-config)
;;; claude-code-config.el ends here
