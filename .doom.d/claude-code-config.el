;;; claude-code-config.el --- Claude Code IDE configuration with termint integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Claude Code IDE integration with termint backend
;; Provides enhanced REPL functionality with vterm terminal backend

;;; Code:

;; Unicode and emoji fixes for Claude Code IDE output
(defun fix-unicode-emoji-substitution ()
  "Prevent technical Unicode symbols from rendering as color emoji.
This function forces JetBrains Mono for ranges of Unicode characters
that should be monospace but are often hijacked by Apple Color Emoji."
  (when (fboundp 'set-fontset-font)
    (let ((mono-font "JetBrains Mono"))
      ;; Technical Symbols (U+2300-U+23FF)
      ;; Includes: ⏸ ⏹ ⏺ ⏻ ⏼ ⏽ etc.
      (set-fontset-font t '(#x2300 . #x23ff) mono-font)
      
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

;; Unicode testing function for Claude Code IDE
(defun unicode-test-claude-code ()
  "Test Unicode rendering that's commonly problematic in Claude Code output.
Creates a test buffer with characters that often get rendered as emoji."
  (interactive)
  (let ((buf (get-buffer-create "*Claude Code Unicode Test*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== CLAUDE CODE UNICODE RENDERING TEST ===\n")
      (insert (format "Created: %s\n" (current-time-string)))
      (insert (format "Font: %s\n\n" (face-attribute 'default :font)))
      
      (insert "=== Common Claude Code Output Symbols ===\n")
      (insert "Record/Status: ⏺ ✅ ❌ ⚠ ✓ ✗\n")
      (insert "Navigation: ▶ ◀ ▲ ▼ → ← ↑ ↓\n") 
      (insert "Progress: ■ □ ● ○ ★ ☆\n")
      (insert "Technical: ⚡ ⚙ 💡 🔧 📁 📂\n")
      (insert "Shapes: ◆ ◇ ▪ ▫ ⭐ ⭕\n\n")
      
      (insert "=== Expected Rendering ===\n")
      (insert "All symbols above should be monospace, not colorful emoji.\n")
      (insert "Technical symbols should match the terminal monospace font.\n")
      (insert "If you see colorful emoji instead, the Unicode fix needs adjustment.\n\n")
      
      (insert "=== Test String for Claude Code ===\n")
      (insert "⏺ Starting analysis... ✓ Processing files ▶ Running tests\n")
      (insert "⚡ Performance: 95% ● Status: Active ⭐ Rating: Excellent\n")
      (insert "📁 Files processed: 42 ⚙ Operations completed ✅ Success!\n"))
    (pop-to-buffer buf)
    (goto-char (point-min))
    (message "Unicode test buffer created. Check if symbols are monospace, not emoji.")))

;; Add keybinding for Unicode testing (Claude Code IDE specific)
(global-set-key (kbd "C-c u") #'unicode-test-claude-code)

;; Prevent global whitespace mode issues in terminals (Claude Code IDE specific)
(after! whitespace
  ;; Ensure whitespace mode doesn't interfere with Claude Code IDE terminals
  (setq whitespace-global-modes '(not vterm-mode eat-mode term-mode)))

;; Claude Code IDE - keep original menu but override functions with termint
(use-package claude-code-ide
  :bind (("C-c C-'" . claude-code-ide-menu)
         ("C-c a" . claude-code-ide-menu))
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-window-width 140)
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-vterm-anti-flicker t))

;; Add LEADER keybinding for Claude Code IDE (SPC a for AI)
(map! :leader
      :desc "Claude Code AI" "a" #'claude-code-ide-menu)

;; Termint configuration for better REPL integration with vterm backend
(use-package termint
  :demand t
  :after vterm
  :config
  ;; Ensure vterm is loaded first
  (require 'vterm)
  ;; Use vterm as backend for proper terminal emulation
  (setq termint-backend 'vterm)
  
  ;; Configure vterm for better Unicode handling
  (after! vterm
    (add-hook 'vterm-mode-hook
              (lambda ()
                ;; Disable whitespace visualization that can interfere with Unicode
                (when (fboundp 'whitespace-mode) (whitespace-mode -1))
                (setq-local show-trailing-whitespace nil)
                (setq-local nobreak-char-display nil))))
  
  ;; Configure Claude Code with termint for better multi-line handling
  (termint-define "claude-code" "claude" 
    :bracketed-paste-p t
    :send-delayed-final-ret t
    :source-syntax "@{{file}}")
  
  ;; Define custom termint functions that use the auto-generated ones
  (defun termint-claude-code-continue ()
    "Continue most recent Claude Code conversation using termint."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (switch-to-buffer buffer)
      (progn
        (termint-claude-code-start)
        (run-with-timer 0.5 nil (lambda ()
          (termint-claude-code-send-string "claude --continue"))))))
    
  (defun termint-claude-code-resume ()
    "Resume Claude Code session from previous conversation using termint."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (switch-to-buffer buffer)
      (progn
        (termint-claude-code-start)
        (run-with-timer 0.5 nil (lambda ()
          (termint-claude-code-send-string "claude --resume"))))))
    
  (defun termint-claude-code-quit ()
    "Stop current Claude Code session."
    (interactive)
    (when-let ((buffer (get-buffer "*claude-code*")))
      (kill-buffer buffer)))
      
  (defun termint-claude-code-list ()
    "List all Claude Code sessions."
    (interactive)
    (message "Active Claude Code sessions: %s" 
             (mapcar #'buffer-name 
                     (seq-filter (lambda (b) 
                                   (string-match-p "claude-code" (buffer-name b)))
                                 (buffer-list)))))
  
  (defun termint-claude-code-switch-to-buffer ()
    "Switch to Claude Code buffer."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (switch-to-buffer buffer)
      (termint-claude-code-start)))
      
  (defun termint-claude-code-toggle-window ()
    "Toggle Claude Code window visibility."
    (interactive)
    (if-let ((window (get-buffer-window "*claude-code*")))
        (delete-window window)
      (termint-claude-code-switch-to-buffer)))
      
  (defun termint-claude-code-send-prompt ()
    "Send prompt from minibuffer to Claude Code."
    (interactive)
    (let ((prompt (read-string "Claude Code prompt: ")))
      (termint-claude-code-send-string prompt)))
      
  (defun termint-claude-code-insert-selection ()
    "Insert current selection into Claude Code."
    (interactive)
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (termint-claude-code-send-string text))
      (message "No region selected")))
      
  (defun termint-claude-code-insert-newline ()
    "Insert newline in Claude Code."
    (interactive)
    (termint-claude-code-send-string "\n"))
      
  (defun termint-claude-code-send-escape ()
    "Send escape key to Claude Code."
    (interactive)
    (termint-claude-code-send-string "\e"))
    
  (defun termint-claude-code-yolo ()
    "Start Claude Code with yolo (bypass permissions) using termint."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (progn
          (switch-to-buffer buffer)
          (termint-claude-code-send-string "claude --dangerously-skip-permissions"))
      (progn
        (termint-claude-code-start)
        (run-with-timer 0.5 nil (lambda ()
          (termint-claude-code-send-string "claude --dangerously-skip-permissions"))))))
          
  (defun termint-claude-code-yolo-continue ()
    "Continue Claude Code conversation with yolo permissions using termint."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (progn
          (switch-to-buffer buffer)
          (termint-claude-code-send-string "claude --dangerously-skip-permissions --continue"))
      (progn
        (termint-claude-code-start)
        (run-with-timer 0.5 nil (lambda ()
          (termint-claude-code-send-string "claude --dangerously-skip-permissions --continue"))))))
          
  (defun termint-claude-code-yolo-resume ()
    "Resume Claude Code session with yolo permissions using termint."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (progn
          (switch-to-buffer buffer)
          (termint-claude-code-send-string "claude --dangerously-skip-permissions --resume"))
      (progn
        (termint-claude-code-start)
        (run-with-timer 0.5 nil (lambda ()
          (termint-claude-code-send-string "claude --dangerously-skip-permissions --resume"))))))
        
  (defun termint-claude-code-configuration ()
    "Open Claude Code configuration."
    (interactive)
    (message "Claude Code configuration: Use termint settings"))
    
  (defun termint-claude-code-debugging ()
    "Open Claude Code debugging."
    (interactive)
    (message "Claude Code debugging: Check *claude-code* buffer"))
  
  ;; Override claude-code-ide functions to use termint implementations
  (with-eval-after-load 'claude-code-ide
    ;; Create a wrapper function for starting since termint-claude-code-start is auto-generated
    (defun claude-code-ide-start-session ()
      "Start Claude Code session using termint."
      (interactive)
      (if (fboundp 'termint-claude-code-start)
          (termint-claude-code-start)
        (error "termint-claude-code-start function not available. Make sure termint is loaded properly.")))
    
    ;; Session Management  
    (defalias 'claude-code-ide-continue-conversation 'termint-claude-code-continue)  
    (defalias 'claude-code-ide-resume-session 'termint-claude-code-resume)
    (defalias 'claude-code-ide-stop-session 'termint-claude-code-quit)
    (defalias 'claude-code-ide-list-sessions 'termint-claude-code-list)
    
    ;; Navigation
    (defalias 'claude-code-ide-switch-to-buffer 'termint-claude-code-switch-to-buffer)
    (defalias 'claude-code-ide-toggle-window 'termint-claude-code-toggle-window)
    
    ;; Interaction
    (defalias 'claude-code-ide-insert-selection 'termint-claude-code-insert-selection)
    (defalias 'claude-code-ide-send-prompt 'termint-claude-code-send-prompt)
    (defalias 'claude-code-ide-send-escape 'termint-claude-code-send-escape)
    (defalias 'claude-code-ide-insert-newline 'termint-claude-code-insert-newline)
    
    ;; Special functions
    (defalias 'claude-code-ide-yolo 'termint-claude-code-yolo)
    (defalias 'claude-code-ide-yolo-continue 'termint-claude-code-yolo-continue)
    (defalias 'claude-code-ide-yolo-resume 'termint-claude-code-yolo-resume)
    
    ;; Submenus  
    (defalias 'claude-code-ide-configuration 'termint-claude-code-configuration)
    (defalias 'claude-code-ide-debugging 'termint-claude-code-debugging)
    
    (message "claude-code-ide functions overridden with termint implementations"))
  
  ;; Add yolo submenu to claude-code-ide menu
  (with-eval-after-load 'claude-code-ide
    (require 'transient)
    
    ;; Define yolo submenu
    (transient-define-prefix claude-code-ide-yolo-menu ()
      "Claude Code IDE yolo (bypass permissions) menu."
      ["Claude Code YOLO (Bypass Permissions)"
       ["Session Management with --dangerously-skip-permissions"
        ("s" "Start new session with yolo" termint-claude-code-yolo)
        ("c" "Continue conversation with yolo" termint-claude-code-yolo-continue)
        ("r" "Resume session with yolo" termint-claude-code-yolo-resume)]])
    
    ;; Override the main menu to include yolo submenu
    (transient-define-prefix claude-code-ide-menu ()
      "Claude Code IDE main menu."
      [:description claude-code-ide--session-status]
      ["Claude Code IDE"
       ["Session Management"
        ("s" claude-code-ide--start-if-no-session :description claude-code-ide--start-description)
        ("c" claude-code-ide--continue-if-no-session :description claude-code-ide--continue-description)
        ("r" claude-code-ide--resume-if-no-session :description claude-code-ide--resume-description)
        ("q" "Stop current session" claude-code-ide-stop)
        ("l" "List all sessions" claude-code-ide-list-sessions)]
       ["Navigation"
        ("b" "Switch to Claude buffer" claude-code-ide-switch-to-buffer)
        ("w" "Toggle window visibility" claude-code-ide-toggle-window)]
       ["Interaction"
        ("i" "Insert selection" claude-code-ide-insert-at-mentioned)
        ("p" "Send prompt from minibuffer" claude-code-ide-send-prompt)
        ("e" "Send escape key" claude-code-ide-send-escape)
        ("n" "Insert newline" claude-code-ide-insert-newline)]
       ["Submenus"
        ("Y" "YOLO (Bypass Permissions)" claude-code-ide-yolo-menu)
        ("C" "Configuration" claude-code-ide-config-menu)
        ("d" "Debugging" claude-code-ide-debug-menu)]]))
  
  (message "termint: Claude Code integration configured with vterm backend"))

(provide 'claude-code-config)
;;; claude-code-config.el ends here