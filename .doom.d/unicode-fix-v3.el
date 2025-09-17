;;; unicode-fix-v3.el --- Aggressive font override approaches

;;; Approach: Override find-font-for-character function
(defun unicode-aggressive-font-override ()
  "Override font selection at the core level."
  (when (display-graphic-p)
    ;; Override the core font selection mechanism
    (advice-add 'font-at :around
                (lambda (orig-fun &optional position window)
                  (let ((char (char-after position)))
                    (if (and char (memq char '(?✅ ?❌ ?⚠ ?⭐ ?⚡ ?⚙ ?💡 ?🔧 ?📁 ?📂)))
                        ;; Force return monospace font spec for problematic characters
                        (font-spec :family "JetBrains Mono" :size 13)
                      (funcall orig-fun position window)))))

    ;; Also override fontset-font to prevent emoji font resolution
    (advice-add 'fontset-font :around
                (lambda (orig-fun fontset character &optional generic-char)
                  (if (memq character '(?✅ ?❌ ?⚠ ?⭐ ?⚡ ?⚙ ?💡 ?🔧 ?📁 ?📂))
                      ;; Return monospace font for problematic characters
                      (cons "JetBrains Mono" "iso10646-1")
                    (funcall orig-fun fontset character generic-char))))

    (message "Applied aggressive font override")))

;;; Approach: Text property overlay method
(defun unicode-text-property-method ()
  "Use text properties to override font rendering."
  (when (display-graphic-p)
    ;; Create a face with specific font
    (defface unicode-monospace-face
      '((t (:family "JetBrains Mono" :height 130 :foreground "white")))
      "Face for forcing monospace unicode rendering.")

    ;; Apply to all occurrences in buffer
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[✅❌⚠⭐⚡⚙💡🔧📁📂]" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (put-text-property start end 'face 'unicode-monospace-face)
          (put-text-property start end 'font-lock-face 'unicode-monospace-face))))

    (message "Applied text property method")))

;;; Approach: Font substitution in display table
(defun unicode-display-table-method ()
  "Use display table to substitute characters."
  (when (display-graphic-p)
    (unless buffer-display-table
      (setq buffer-display-table (make-display-table)))

    ;; Substitute emoji with text equivalents that look similar
    (aset buffer-display-table ?✅ [?✓])  ; checkmark -> check
    (aset buffer-display-table ?❌ [?✗])  ; cross mark -> ballot X
    (aset buffer-display-table ?⚠ [?!])   ; warning -> exclamation
    (aset buffer-display-table ?⭐ [?*])  ; star -> asterisk
    (aset buffer-display-table ?⚡ [?~])  ; lightning -> tilde

    (message "Applied display table substitution")))

;;; Approach: Terminal emacs test
(defun test-terminal-emacs ()
  "Launch terminal emacs to test unicode rendering."
  (interactive)
  (let ((test-file "/tmp/unicode-test.txt"))
    (with-temp-file test-file
      (insert "=== TERMINAL EMACS UNICODE TEST ===\n")
      (insert "Symbols: ✅ ❌ ⚠ ⚡ ⭐\n")
      (insert "Expected: Should render differently than GUI Emacs\n"))
    (message "Created test file: %s" test-file)
    (message "Run: emacs -nw %s" test-file)))

;;; Approach: macOS specific - disable system emoji substitution
(defun unicode-disable-macos-emoji ()
  "Disable macOS emoji substitution at system level."
  (when (eq system-type 'darwin)
    ;; This might work for some macOS versions
    (setenv "TERM" "xterm-256color")
    ;; Try to disable emoji substitution
    (when (fboundp 'ns-hide-emacs)
      (message "Attempting to disable macOS emoji substitution"))
    (message "Applied macOS emoji disabling attempt")))

;;; Test function that applies multiple approaches
(defun test-aggressive-unicode-fixes ()
  "Test aggressive unicode fix approaches."
  (interactive)
  (message "=== TESTING AGGRESSIVE UNICODE FIXES ===")

  ;; Apply all methods
  (unicode-aggressive-font-override)
  (unicode-text-property-method)
  (unicode-display-table-method)
  (unicode-disable-macos-emoji)

  ;; Force redisplay
  (redraw-display)

  (message "=== All aggressive fixes applied - check buffer ==="))

(provide 'unicode-fix-v3)