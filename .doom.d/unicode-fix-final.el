;;; unicode-fix-final.el --- Final attempt at unicode emoji fix

;;; Research findings:
;;; 1. macOS has system-level emoji substitution that overrides Emacs font settings
;;; 2. Apple Color Emoji font takes priority even when fontset is configured
;;; 3. Need to intercept at display level, not font level

;;; Approach: Character substitution with similar-looking alternatives
(defun unicode-substitute-problematic-chars ()
  "Replace emoji with visually similar non-emoji alternatives."
  (when (display-graphic-p)
    (let ((substitutions '((?✅ . ?✓)    ; Heavy check mark -> Check mark
                          (?❌ . ?✗)    ; Cross mark -> Ballot X
                          (?⚠ . ?!)     ; Warning sign -> Exclamation mark
                          (?⭐ . ?★)    ; White medium star -> Black star
                          (?⚡ . ?↯)    ; High voltage -> Downwards zigzag arrow
                          (?💡 . ?◉)    ; Light bulb -> Fisheye
                          (?🔧 . ?⚒)    ; Wrench -> Hammer and pick
                          (?📁 . ?📂)   ; Keep folder emoji as is for now
                          (?⚙ . ?☸))))  ; Gear -> Wheel of dharma

      (dolist (sub substitutions)
        (let ((from-char (car sub))
              (to-char (cdr sub)))
          ;; Set up global substitution
          (unless standard-display-table
            (setq standard-display-table (make-display-table)))
          (aset standard-display-table from-char (vector to-char))

          ;; Also set buffer-local if needed
          (when (current-buffer)
            (unless buffer-display-table
              (setq buffer-display-table (copy-sequence standard-display-table)))
            (when buffer-display-table
              (aset buffer-display-table from-char (vector to-char))))))

      (message "Applied character substitution approach"))))

;;; Approach: Font lock override with exact character matching
(defun unicode-font-lock-override ()
  "Use font-lock to force specific rendering."
  (when (display-graphic-p)
    ;; Define a monospace face for unicode
    (defface unicode-mono-override
      '((t (:family "JetBrains Mono"
            :foreground "#cdd6f4"  ; Catppuccin text color
            :background nil
            :weight normal)))
      "Monospace override for unicode symbols.")

    ;; Create font-lock rules
    (font-lock-add-keywords
     nil
     '(("[✅❌⚠⭐⚡💡🔧⚙]" 0 'unicode-mono-override prepend)))

    ;; Enable font-lock if not already enabled
    (unless font-lock-mode
      (font-lock-mode 1))

    (message "Applied font-lock override approach")))

;;; Approach: Post-command hook to continuously apply fixes
(defvar unicode-fix-active nil
  "Whether unicode fix is currently active.")

(defun unicode-continuous-fix ()
  "Continuously fix unicode rendering."
  (when (and unicode-fix-active (display-graphic-p))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (re-search-forward "[✅❌⚠⭐⚡💡🔧⚙]" nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (put-text-property start end 'face 'unicode-mono-override)
            (put-text-property start end 'font-lock-face 'unicode-mono-override)))))))

(defun unicode-enable-continuous-fix ()
  "Enable continuous unicode fixing."
  (interactive)
  (setq unicode-fix-active t)
  (add-hook 'post-command-hook #'unicode-continuous-fix nil t)
  (message "Enabled continuous unicode fix"))

(defun unicode-disable-continuous-fix ()
  "Disable continuous unicode fixing."
  (interactive)
  (setq unicode-fix-active nil)
  (remove-hook 'post-command-hook #'unicode-continuous-fix t)
  (message "Disabled continuous unicode fix"))

;;; Approach: Complete system override attempt
(defun unicode-nuclear-option ()
  "Nuclear option - try everything at once."
  (when (display-graphic-p)
    ;; 1. Character substitution
    (unicode-substitute-problematic-chars)

    ;; 2. Font-lock override
    (unicode-font-lock-override)

    ;; 3. Continuous fixing
    (unicode-enable-continuous-fix)

    ;; 4. Force fontset to specific non-emoji font
    (dolist (char '(?✅ ?❌ ?⚠ ?⭐ ?⚡ ?💡 ?🔧 ?⚙))
      (set-fontset-font t char (font-spec :family "Symbols Nerd Font Mono" :size 13) nil 'prepend))

    ;; 5. Try removing emoji fonts entirely from system
    (setq face-font-rescale-alist
          (append '(("Apple Color Emoji" . 0.0001)  ; Make emoji font tiny
                   ("Noto Color Emoji" . 0.0001))
                  face-font-rescale-alist))

    ;; 6. Force redisplay
    (redraw-display)
    (force-window-update)

    (message "Applied nuclear option - all unicode fixes at once")))

;;; Main test function
(defun test-final-unicode-approach ()
  "Test the final unicode fix approach."
  (interactive)
  (message "=== FINAL UNICODE FIX ATTEMPT ===")

  ;; Apply nuclear option
  (unicode-nuclear-option)

  ;; Create test content
  (with-current-buffer (get-buffer-create "*Final Unicode Test*")
    (erase-buffer)
    (insert "=== FINAL UNICODE FIX TEST ===\n")
    (insert "Original symbols: ✅ ❌ ⚠ ⭐ ⚡ 💡 🔧 ⚙\n")
    (insert "After substitution and overrides:\n")
    (insert "✅ ❌ ⚠ ⭐ ⚡ 💡 🔧 ⚙\n")
    (insert "\nIf working: symbols should be monospace, not colorful\n")

    ;; Apply fixes to this buffer specifically
    (unicode-font-lock-override)
    (font-lock-fontify-buffer)
    (unicode-continuous-fix)

    (switch-to-buffer (current-buffer)))

  (message "=== Check *Final Unicode Test* buffer for results ==="))

(provide 'unicode-fix-final)