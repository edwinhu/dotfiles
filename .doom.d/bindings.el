;;; bindings.el -*- lexical-binding: t; -*-

(map! :map global-map "M-Q" #'unfill-paragraph)

;; SAS console keybindings
(after! sas-console
  ;; Global SAS console commands
  (map! "C-c s o" #'sas-console-open
        "C-c s s" #'sas-console-open-split
        "C-c s k" #'sas-console-kill-all-sessions
        "C-c s l" #'sas-console-list-sessions)
  
  ;; Keybindings for org-src-mode (when editing SAS source blocks)
  (map! :map org-src-mode-map
        "C-<return>" #'sas-console-send-line
        "C-c C-<return>" #'sas-console-send-region
        "C-c s" #'sas-console-open-for-org-src))

;; CRITICAL: C-RET override for Jupyter integration
;; This uses emulation-mode-map-alists for HIGHEST priority

(defun smart-org-src-send ()
  "Smart dispatcher for org-src C-RET based on detected language."
  (interactive)
  (message "=== smart-org-src-send called ===")
  
  ;; Enhanced context detection
  (let* ((in-org (derived-mode-p 'org-mode))
         (in-org-src (and (boundp 'org-src--beg-marker) org-src--beg-marker))
         (in-src-block (and in-org (org-in-src-block-p)))
         (element (when in-org (org-element-at-point)))
         (element-lang (when element (org-element-property :language element)))
         ;; Buffer name detection for org-src buffers
         (buffer-name-lang (when (string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" (buffer-name))
                            (downcase (match-string 1 (buffer-name)))))
         ;; Variable detection from org-src
         (var-lang (bound-and-true-p org-src--lang))
         ;; Final language determination
         (detected-lang (or element-lang var-lang buffer-name-lang)))
    
    (message "Context: org=%s org-src=%s in-block=%s element-lang=%s buffer-lang=%s var-lang=%s final-lang=%s"
             in-org in-org-src in-src-block element-lang buffer-name-lang var-lang detected-lang)
    
    ;; Route to appropriate handler
    (cond
     ;; Stata code execution
     ((and detected-lang
           (member (downcase detected-lang) '("stata" "jupyter-stata"))
           (or in-org-src in-src-block))
      (message "✓ Calling Stata execution handler")
      (cond
       ;; If we're in org-src edit buffer, use the org-src specific handler
       ((and in-org-src (fboundp 'termint-org-src-send-line-or-paragraph-or-fun-or-region))
        (termint-org-src-send-line-or-paragraph-or-fun-or-region))
       ((fboundp 'euporie-termint-send-region-or-line)
        (euporie-termint-send-region-or-line))
       ((fboundp 'org-babel-execute-src-block)
        (message "Using org-babel fallback for Stata")
        (org-babel-execute-src-block))
       (t
        (message "Stata handler not available"))))
     
     ;; Python code execution  
     ((and detected-lang
           (member (downcase detected-lang) '("python" "jupyter-python"))
           (or in-org-src in-src-block))
      (message "✓ Calling Python execution handler")
      (cond
       ;; If we're in org-src edit buffer, use the org-src specific handler
       ((and in-org-src (fboundp 'termint-org-src-send-line-or-paragraph-or-fun-or-region))
        (termint-org-src-send-line-or-paragraph-or-fun-or-region))
       ((fboundp 'euporie-termint-send-region-or-line)
        (euporie-termint-send-region-or-line))
       (t
        (message "Python handler not available"))))
     
     ;; R code execution
     ((and detected-lang
           (member (downcase detected-lang) '("r" "jupyter-r"))
           (or in-org-src in-src-block))
      (message "✓ Calling R execution handler")
      (cond
       ;; If we're in org-src edit buffer, use the org-src specific handler
       ((and in-org-src (fboundp 'termint-org-src-send-line-or-paragraph-or-fun-or-region))
        (termint-org-src-send-line-or-paragraph-or-fun-or-region))
       ((fboundp 'euporie-termint-send-region-or-line)
        (euporie-termint-send-region-or-line))
       (t
        (message "R handler not available"))))
     
     ;; Default behavior
     (t
      (message "✗ No jupyter context detected, using default newline behavior")
      (if (fboundp '+default/newline-below)
          (+default/newline-below)
        (newline))))))

;; Create keymap for C-RET override with maximum priority
(defvar euporie-termint-override-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-RET") #'smart-org-src-send)
    (define-key map (kbd "C-<return>") #'smart-org-src-send)
    map)
  "Keymap for jupyter-termint C-RET override.")

;; Recursion guard
(defvar euporie-termint--in-advice nil
  "Prevent recursive calls to jupyter-termint advice.")

;; NUCLEAR OPTION: Advice +default/newline-below to intercept C-RET
(defun euporie-termint--intercept-newline-below (orig-func &rest args)
  "Intercept +default/newline-below and call jupyter instead when appropriate."
  (if euporie-termint--in-advice
      ;; If already in advice, just call original function
      (apply orig-func args)
    ;; Enhanced context detection with multiple fallback approaches
    (let* ((in-org (derived-mode-p 'org-mode))
           (in-org-src (and (boundp 'org-src--beg-marker) org-src--beg-marker))
           (in-src-block (and in-org (org-in-src-block-p)))
           (element (when in-org (org-element-at-point)))
           (element-lang (when element (org-element-property :language element)))
           (major-mode-lang (cond
                            ((derived-mode-p 'python-mode 'python-ts-mode) "python")
                            ((derived-mode-p 'R-mode 'ess-r-mode) "R") 
                            ((derived-mode-p 'stata-mode) "stata")
                            (t nil)))
           ;; Buffer name detection for org-src buffers
           (buffer-name-lang (when (string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" (buffer-name))
                               (downcase (match-string 1 (buffer-name)))))
           ;; Variable detection from org-src
           (var-lang (bound-and-true-p org-src--lang))
           ;; Final language determination
           (detected-lang (or element-lang var-lang buffer-name-lang major-mode-lang)))
      
      ;; Debug logging (disabled - system working)
      ;; (message "C-RET DEBUG: buffer=%s org=%s org-src=%s in-block=%s element-lang=%s major-mode-lang=%s buffer-lang=%s var-lang=%s final-lang=%s"
      ;;          (buffer-name) in-org in-org-src in-src-block element-lang major-mode-lang buffer-name-lang var-lang detected-lang)
      
      ;; Enhanced condition check
      (if (and detected-lang
               (member (downcase detected-lang) '("python" "stata" "r"))
               (or in-org-src in-src-block))
          (let ((euporie-termint--in-advice t))
            ;; Debug enabled for troubleshooting
            (message "✓ Calling jupyter for %s" detected-lang)
            (smart-org-src-send))
        (progn
          ;; Debug enabled for troubleshooting  
          (message "✗ Using original +default/newline-below (no jupyter context)")
          (apply orig-func args))))))

;; Install the advice immediately - aggressive override of C-RET
(advice-add '+default/newline-below :around #'euporie-termint--intercept-newline-below)

;; Also intercept org-mode's C-RET binding
(defun euporie-termint--intercept-org-insert-item (orig-func &rest args)
  "Intercept +org/insert-item-below and call jupyter instead when appropriate."
  (if euporie-termint--in-advice
      ;; If already in advice, just call original function  
      (apply orig-func args)
    ;; Use the same detection logic as newline-below
    (let* ((in-org (derived-mode-p 'org-mode))
           (in-org-src (and (boundp 'org-src--beg-marker) org-src--beg-marker))
           (in-src-block (and in-org (org-in-src-block-p)))
           (element (when in-org (org-element-at-point)))
           (element-lang (when element (org-element-property :language element)))
           (detected-lang element-lang))
      
      (if (and detected-lang
               (member (downcase detected-lang) '("python" "stata" "r"))
               (or in-org-src in-src-block))
          (let ((euporie-termint--in-advice t))
            (message "✓ Org-mode C-RET intercepted for %s" detected-lang)
            (smart-org-src-send))
        (progn
          (message "✗ Using original +org/insert-item-below")
          (apply orig-func args))))))

(advice-add '+org/insert-item-below :around #'euporie-termint--intercept-org-insert-item)

;; Terminal testing keybindings
(map! "C-c t v" #'vterm
      "C-c t e" #'eat
      "C-c u" #'unicode-test-insert
      "C-c U" #'unicode-test-buffer
      "C-c T" #'unicode-compare-terminals)

;; Unicode testing functions
(defun unicode-test-insert ()
  "Insert test Unicode characters for terminal testing"
  (interactive)
  (insert "Test Unicode: ⏺ ✳ ⭐ 🔴 📍 ▶️ ⚡ 🌟 ❄️ 🎯"))

(defun unicode-test-buffer ()
  "Create a dedicated buffer for Unicode testing with comprehensive test cases"
  (interactive)
  (let ((buf (get-buffer-create "*Unicode Test*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== UNICODE RENDERING TEST BUFFER ===\n")
      (insert (format "Created: %s\n" (current-time-string)))
      (insert (format "Font: %s\n\n" (face-attribute 'default :font)))
      
      (insert "=== Critical Test Characters ===\n")
      (insert "Record symbol: ⏺ (should be red circle)\n")
      (insert "Asterisk: ✳ (should be 8-spoked star)\n")
      (insert "Star: ⭐ (should be yellow star)\n")
      (insert "Red circle: 🔴 (should be red emoji)\n")
      (insert "Play button: ▶️ (should be triangle)\n")
      (insert "Lightning: ⚡ (should be yellow bolt)\n\n")
      
      (insert "=== Geometric Shapes ===\n")
      (insert "Circles: ● ○ ⚫ ⚪ 🔴 🔵 🟢 🟡\n")
      (insert "Squares: ■ □ ▪ ▫ 🟥 🟦 🟩 🟨\n")
      (insert "Triangles: ▲ △ ▼ ▽ ◆ ◇\n\n")
      
      (insert "=== Arrows and Pointers ===\n")
      (insert "Basic arrows: → ← ↑ ↓ ↔ ↕\n")
      (insert "Pointer triangles: ▶ ◀ ▲ ▼\n")
      (insert "Double arrows: ⇒ ⇐ ⇑ ⇓\n\n")
      
      (insert "=== Symbols and Marks ===\n")
      (insert "Check marks: ✓ ✔ ☑ ✅ ❌ ❎\n")
      (insert "Stars: ★ ☆ ⭐ 🌟 ✨ 💫\n")
      (insert "Warnings: ⚠ 🚨 ❗ ❓ ⚡\n\n")
      
      (insert "=== Box Drawing ===\n")
      (insert "┌─┬─┐\n")
      (insert "│ │ │\n") 
      (insert "├─┼─┤\n")
      (insert "│ │ │\n")
      (insert "└─┴─┘\n\n")
      
      (insert "=== Mixed Content ===\n")
      (insert "Progress: [████████████████████] 100% ✔\n")
      (insert "Status: ⏺ Recording • ⚡ Active • 🔴 Live\n")
      (insert "Controls: ▶ Play ⏸ Pause ⏹ Stop ⏭ Next\n")
      (insert "Rating: ⭐⭐⭐⭐⭐ (5 stars)\n\n")
      
      (insert "=== Font Fallback Test ===\n")
      (insert "ASCII equivalents: * vs ✳, > vs ▶, - vs —\n")
      (insert "Similar shapes: O vs ○ vs ● vs ⭕ vs 🔴\n")
      (insert "Variation selectors: ▶ vs ▶️ (with vs without)\n\n")
      
      (insert "=== End of Test Buffer ===\n"))
    (pop-to-buffer buf)
    (goto-char (point-min))))

(defun unicode-compare-terminals ()
  "Set up side-by-side comparison of Unicode rendering in different contexts"
  (interactive)
  (delete-other-windows)
  ;; Create test buffer
  (unicode-test-buffer)
  ;; Split and create terminal
  (split-window-right)
  (other-window 1)
  (vterm)
  ;; Split again for eat terminal
  (split-window-below)
  (other-window 1)
  (eat)
  ;; Go back to test buffer
  (other-window 1)
  (message "Unicode comparison setup complete. Run test scripts in terminals."))

;; R jupyter sixel integration is now handled in unified jupyter-termint.el
;; No separate loading needed - R support is included in jupyter-termint.el