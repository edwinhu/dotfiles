;;; unicode-fix-v2.el --- Alternative approaches to fix unicode emoji rendering

;;; Approach 1: Composition-function-table override
(defun unicode-fix-composition-approach ()
  "Force monospace rendering using composition-function-table."
  (when (display-graphic-p)
    (let ((problematic-chars '(?✅ ?❌ ?⚠ ?⭐ ?⚡ ?⚙ ?💡 ?🔧 ?📁 ?📂)))
      (dolist (char problematic-chars)
        ;; Remove any existing composition
        (set-char-table-range composition-function-table char nil)
        ;; Force character to render as itself (no composition)
        (aset composition-function-table char
              (lambda (from to font-object string)
                nil))))))

;;; Approach 2: Display property override
(defun unicode-fix-display-property-approach ()
  "Use display properties to force font rendering."
  (when (display-graphic-p)
    (let ((mono-font-spec (font-spec :family "JetBrains Mono" :size 13)))
      (dolist (char '(?✅ ?❌ ?⚠ ?⭐ ?⚡))
        ;; Set a global display property for this character
        (put-char-code-property char 'display
                               `(font ,mono-font-spec))))))

;;; Approach 3: char-displayable-p override
(defun unicode-fix-char-displayable-override ()
  "Override char-displayable-p to force specific font choices."
  (advice-add 'char-displayable-p :around
              (lambda (orig-fun char &optional face)
                (if (memq char '(?✅ ?❌ ?⚠ ?⭐ ?⚡))
                    ;; Force these chars to be considered displayable with our font
                    (font-has-char-p (font-spec :family "JetBrains Mono") char)
                  (funcall orig-fun char face)))))

;;; Approach 4: Aggressive fontset override with priority manipulation
(defun unicode-fix-aggressive-fontset ()
  "Aggressively override fontset with priority manipulation."
  (when (display-graphic-p)
    (let ((mono-font "JetBrains Mono"))
      ;; Clear existing fontset entries for these chars
      (dolist (char '(?✅ ?❌ ?⚠ ?⭐ ?⚡))
        (set-fontset-font t char nil)
        (set-fontset-font "fontset-default" char nil))

      ;; Force set with explicit priority
      (dolist (char '(?✅ ?❌ ?⚠ ?⭐ ?⚡))
        (set-fontset-font t char (cons mono-font "iso10646-1") nil 'prepend)
        (set-fontset-font "fontset-default" char (cons mono-font "iso10646-1") nil 'prepend))

      ;; Explicitly block emoji fonts for these characters
      (dolist (emoji-font '("Apple Color Emoji" "Noto Color Emoji"))
        (dolist (char '(?✅ ?❌ ?⚠ ?⭐ ?⚡))
          (set-fontset-font t char emoji-font nil 'append))))))

;;; Approach 5: Terminal vs GUI test
(defun unicode-test-terminal-vs-gui ()
  "Test rendering in both GUI and terminal."
  (if (display-graphic-p)
      (message "GUI Mode: Unicode rendering test")
    (message "Terminal Mode: Unicode rendering test"))
  (message "Test symbols: ✅ ❌ ⚠ ⚡ ⭐")
  (message "Display type: %s" (if (display-graphic-p) "GUI" "Terminal")))

;;; Approach 6: Disable emoji completely
(defun unicode-fix-disable-emoji-completely ()
  "Completely disable emoji font fallback."
  (when (display-graphic-p)
    ;; Remove all emoji fonts from fontset
    (dolist (emoji-font '("Apple Color Emoji" "Noto Color Emoji" "Segoe UI Emoji"))
      (set-fontset-font t 'emoji nil)
      (set-fontset-font "fontset-default" 'emoji nil))

    ;; Set emoji script to use monospace font instead
    (set-fontset-font t 'emoji (font-spec :family "JetBrains Mono"))

    ;; Disable emoji-related variables
    (setq use-default-font-for-symbols nil)
    (setq auto-composition-mode nil))) ; This might be drastic

;;; Test all approaches
(defun test-all-unicode-fixes ()
  "Test all unicode fix approaches."
  (interactive)
  (message "=== TESTING UNICODE FIX APPROACHES ===")

  ;; Show current state
  (message "Before fixes - fontset for ✅: %s" (fontset-font t ?✅))

  ;; Try approach 1
  (unicode-fix-composition-approach)
  (message "After composition approach - fontset for ✅: %s" (fontset-font t ?✅))

  ;; Try approach 2
  (unicode-fix-display-property-approach)
  (message "After display property approach")

  ;; Try approach 3
  (unicode-fix-char-displayable-override)
  (message "After char-displayable override")

  ;; Try approach 4
  (unicode-fix-aggressive-fontset)
  (message "After aggressive fontset - fontset for ✅: %s" (fontset-font t ?✅))

  ;; Try approach 6
  (unicode-fix-disable-emoji-completely)
  (message "After disable emoji - fontset for ✅: %s" (fontset-font t ?✅))

  (message "=== All approaches applied ==="))

(provide 'unicode-fix-v2)