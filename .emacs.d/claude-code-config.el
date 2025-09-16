;;; claude-code-config.el --- Claude Code IDE configuration for vanilla Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Adapted for vanilla Emacs without Doom dependencies

;;; Code:

;; Claude Code IDE integration
(use-package claude-code-ide
  :straight (:host github :repo "manzaltu/claude-code-ide.el")
  :config
  (when (fboundp 'claude-code-ide-emacs-tools-setup)
    (claude-code-ide-emacs-tools-setup))
  (setq claude-code-ide-window-width 140
        claude-code-ide-terminal-backend 'vterm
        claude-code-ide-vterm-anti-flicker t))

;; Add to leader key bindings - outside use-package to ensure they load immediately
(when (fboundp 'my-leader-def)
  (my-leader-def
    "a"   '(:ignore t :which-key "Claude Code")
    "am"  '(claude-code-ide-menu :which-key "Menu")
    "as"  '(claude-code-ide--start-if-no-session :which-key "Start new session")
    "ac"  '(claude-code-ide-continue :which-key "Continue")
    "ar"  '(claude-code-ide-resume :which-key "Resume")
    "ay"  '(:ignore t :which-key "YOLO mode")
    "ay s" '(claude-code-ide-yolo-start :which-key "Start")
    "ay c" '(claude-code-ide-yolo-continue :which-key "Continue")
    "ay r" '(claude-code-ide-yolo-resume :which-key "Resume")
    "ab"  '(claude-code-ide-switch-to-buffer :which-key "Switch to buffer")
    "ap"  '(claude-code-ide-send-prompt :which-key "Send prompt")
    "al"  '(claude-code-ide-list-sessions :which-key "List sessions")))

;; Custom yolo mode functions
(defun claude-code-ide-yolo-start ()
  "Start Claude Code in YOLO mode (bypass permissions)."
  (interactive)
  (let ((claude-code-ide-cli-extra-flags "--dangerously-skip-permissions"))
    (claude-code-ide--start-if-no-session)))

(defun claude-code-ide-yolo-continue ()
  "Continue Claude Code session in YOLO mode (bypass permissions)."
  (interactive)
  (let ((claude-code-ide-cli-extra-flags "--dangerously-skip-permissions"))
    (claude-code-ide-continue)))

(defun claude-code-ide-yolo-resume ()
  "Resume Claude Code session in YOLO mode (bypass permissions)."
  (interactive)
  (let ((claude-code-ide-cli-extra-flags "--dangerously-skip-permissions"))
    (claude-code-ide-resume)))

;; Prevent whitespace mode issues in terminals (Claude Code IDE specific)
(with-eval-after-load 'whitespace
  ;; Ensure whitespace mode doesn't interfere with Claude Code IDE terminals
  (setq whitespace-global-modes '(not vterm-mode eat-mode term-mode)))

;; Configure vterm for better Unicode handling (like Doom config)
(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Disable whitespace visualization that can interfere with Unicode
              (when (fboundp 'whitespace-mode) (whitespace-mode -1))
              (when (fboundp 'global-whitespace-mode) (global-whitespace-mode -1))
              (setq-local show-trailing-whitespace nil)
              (setq-local nobreak-char-display nil)
              ;; Also disable any other whitespace highlighting
              (setq-local whitespace-display-mappings nil)
              (setq-local indicate-empty-lines nil)
              (setq-local indicate-buffer-boundaries nil)

)))

;; Additional fix: Disable whitespace in claude-code buffers specifically
(defun claude-code-disable-whitespace ()
  "Disable all whitespace visualization in Claude Code buffers."
  (when (and (buffer-name)
             (string-match-p "\\*claude-code" (buffer-name)))
    (when (fboundp 'whitespace-mode) (whitespace-mode -1))
    (setq-local show-trailing-whitespace nil)
    (setq-local nobreak-char-display nil)
    (setq-local whitespace-display-mappings nil)))

(add-hook 'buffer-list-update-hook 'claude-code-disable-whitespace)


(provide 'claude-code-config)
;;; claude-code-config.el ends here
