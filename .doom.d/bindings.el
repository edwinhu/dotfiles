;;; bindings.el -*- lexical-binding: t; -*-

;;; SAS Workflow Logging Infrastructure
(defvar sas-workflow-debug-log-file (expand-file-name "sas-workflow-debug.log" "~/")
  "Log file for SAS workflow debugging information.")

(defun sas-workflow-debug-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to SAS workflow debug file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) sas-workflow-debug-log-file))))

(defun sas-workflow-clear-log ()
  "Clear the SAS workflow debug log file."
  (interactive)
  (when (file-exists-p sas-workflow-debug-log-file)
    (delete-file sas-workflow-debug-log-file))
  (sas-workflow-debug-log 'info "=== SAS Workflow Log Started ==="))

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

;; Helper functions for SAS workflow (defined here to avoid loading issues)
(defun smart-org-src-get-language ()
  "Get the detected language from current context."
  (let* ((in-org (derived-mode-p 'org-mode))
         (in-org-src (and (boundp 'org-src--beg-marker) org-src--beg-marker))
         (in-src-block (and in-org (org-in-src-block-p)))
         ;; Language detection from multiple sources
         (element-lang (when (and in-org in-src-block)
                        (let ((element (org-element-at-point)))
                          (when (eq (org-element-type element) 'src-block)
                            (org-element-property :language element)))))
         (buffer-lang (when in-org-src
                       (let ((buffer-name (buffer-name)))
                         (when (string-match "\\*Org Src .*\\[\\([^]]+\\)\\]\\*" buffer-name)
                           (match-string 1 buffer-name)))))
         (var-lang (and (boundp 'org-src-lang-mode) 
                       (symbol-name org-src-lang-mode)
                       (replace-regexp-in-string "-mode$" "" (symbol-name org-src-lang-mode))))
         (final-lang (or element-lang buffer-lang var-lang "unknown")))
    final-lang))

(defun smart-org-src-parse-remote-dir ()
  "Parse remote directory from org-babel-info."
  (let* ((babel-info (bound-and-true-p org-src--babel-info))
         (dir (when babel-info (cdr (assoc :dir (nth 2 babel-info))))))
    dir))

(defun smart-org-src-send ()
  "Smart dispatcher for org-src C-RET based on detected language."
  (interactive)
  (sas-workflow-debug-log 'info "=== C-RET pressed in smart-org-src-send ===")
  (message "=== smart-org-src-send called ===")
  
  ;; Enhanced context detection with safe marker handling
  (let* ((in-org (derived-mode-p 'org-mode))
         (in-org-src (and (boundp 'org-src--beg-marker) 
                         org-src--beg-marker
                         (condition-case nil
                             (marker-buffer org-src--beg-marker)
                           (error nil))))
         (in-src-block (and in-org (condition-case nil
                                       (org-in-src-block-p)
                                     (error nil))))
         (element (when in-org (org-element-at-point)))
         (element-lang (when element (org-element-property :language element)))
         ;; Buffer name detection for org-src buffers
         (buffer-name-lang (when (string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" (buffer-name))
                            (downcase (match-string 1 (buffer-name)))))
         ;; Variable detection from org-src
         (var-lang (bound-and-true-p org-src--lang))
         ;; Final language determination
         (detected-lang (or element-lang var-lang buffer-name-lang)))
    
    (sas-workflow-debug-log 'debug "Language detection - buffer: %s, in-org: %s, in-org-src: %s, in-src-block: %s"
                            (buffer-name) in-org in-org-src in-src-block)
    (sas-workflow-debug-log 'debug "Language detection - element-lang: %s, buffer-lang: %s, var-lang: %s, final: %s"
                            element-lang buffer-name-lang var-lang detected-lang)
    
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
     
     ;; SAS code execution
     ((and detected-lang
           (member (downcase detected-lang) '("sas" "jupyter-sas"))
           (or in-org-src in-src-block))
      (sas-workflow-debug-log 'info "✓ SAS code execution detected")
      (message "✓ Calling SAS execution handler")
      (let* ((code (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (thing-at-point 'line t)))
             ;; Safe extraction of :dir - check different contexts
             (babel-info (cond
                         ;; org-src editing buffer context  
                         ((and in-org-src (boundp 'org-src--babel-info)) org-src--babel-info)
                         ;; org-mode src block context
                         ((and in-src-block element) 
                          (let ((params (org-element-property :parameters element)))
                            (when params
                              (list nil nil (org-babel-parse-header-arguments params)))))
                         (t nil)))
             (dir (when babel-info 
                   (condition-case err
                       (cdr (assoc :dir (nth 2 babel-info)))
                     (error 
                      (sas-workflow-debug-log 'error "Error accessing babel-info dir: %s" err)
                      nil)))))
        
        (sas-workflow-debug-log 'debug "SAS execution context - code: %s" 
                                (if code (substring code 0 (min 50 (length code))) "nil"))
        (sas-workflow-debug-log 'debug "SAS execution context - babel-info: %s, dir: %s" babel-info dir)
        (sas-workflow-debug-log 'info "SAS execution: dir=%s remote=%s" dir (when dir (file-remote-p dir)))
        (message "SAS execution: dir=%s remote=%s" dir (when dir (file-remote-p dir)))
        (cond
         ;; Remote SAS execution via WRDS - detect both TRAMP paths and WRDS filesystem paths
         ((and dir 
               (or (file-remote-p dir)  ; Standard TRAMP paths like /ssh:host:path
                   (string-match "^/wrds/" dir))  ; WRDS filesystem paths like /wrds/home/user/
               (fboundp 'euporie-sas-start-remote))
          (sas-workflow-debug-log 'info "✓ Using remote SAS execution via WRDS")
          (message "✓ Using remote SAS execution via WRDS")
          (sas-workflow-debug-log 'debug "Calling euporie-sas-start-remote with dir: %s" dir)
          (euporie-sas-start-remote dir)
          (when (fboundp 'termint-euporie-sas-send-region)
            (sas-workflow-debug-log 'debug "Sending SAS region via termint-euporie-sas-send-region")
            (termint-euporie-sas-send-region (point-at-bol) (point-at-eol))))
         ;; Local SAS execution
         ((fboundp 'euporie-sas-start)
          (sas-workflow-debug-log 'info "✓ Using local SAS execution")
          (message "✓ Using local SAS execution")
          (sas-workflow-debug-log 'debug "Calling euporie-sas-start")
          (euporie-sas-start)
          (when (fboundp 'termint-euporie-sas-send-region)
            (sas-workflow-debug-log 'debug "Sending SAS region via termint-euporie-sas-send-region")
            (termint-euporie-sas-send-region (point-at-bol) (point-at-eol))))
         ;; Fallback to send only
         ((fboundp 'termint-euporie-sas-send-region)
          (sas-workflow-debug-log 'info "✓ Using SAS send region fallback")
          (message "✓ Using SAS send region fallback")
          (sas-workflow-debug-log 'debug "Calling termint-euporie-sas-send-region fallback")
          (termint-euporie-sas-send-region (point-at-bol) (point-at-eol)))
         (t
          (sas-workflow-debug-log 'error "✗ SAS handler not available - functions missing")
          (message "✗ SAS handler not available - functions missing")))))
     
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
                            ((derived-mode-p 'sas-mode) "sas")
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
               (member (downcase detected-lang) '("python" "stata" "r" "sas"))
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
               (member (downcase detected-lang) '("python" "stata" "r" "sas"))
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