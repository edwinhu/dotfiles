;;; org-syntax-fix.el --- Comprehensive org-mode syntax highlighting fix for Doom Emacs

;;; Commentary:
;; This module addresses org-mode syntax highlighting issues in Doom Emacs by:
;; 1. Checking for tree-sitter interference and disabling it in org-src blocks
;; 2. Implementing manual fontification triggers with proper timing
;; 3. Overriding Doom's restrictive font-lock settings
;; 4. Adding buffer-local fontification hooks
;; 5. Forcing major mode activation in src blocks

;;; Code:

;; Debug logging for syntax highlighting diagnostics
(defvar org-syntax-debug-log-file (expand-file-name "org-syntax-debug.log" "~/"))

(defun org-syntax-debug-log (level format-string &rest args)
  "Log org-mode syntax highlighting debugging information."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) org-syntax-debug-log-file))))

;; ================================================================
;;; 1. TREE-SITTER INTERFERENCE DETECTION AND MITIGATION
;; ================================================================

(defun org-syntax-disable-tree-sitter-in-src ()
  "Disable tree-sitter in org-src blocks to prevent font-lock conflicts."
  (when (and (bound-and-true-p tree-sitter-mode)
             (derived-mode-p 'org-mode))
    (org-syntax-debug-log 'info "Disabling tree-sitter in org-mode for syntax highlighting compatibility")
    (tree-sitter-mode -1)))

;; ================================================================
;;; 2. DOOM FONT-LOCK OVERRIDE SYSTEM
;; ================================================================

(defun org-syntax-override-doom-restrictions ()
  "Override Doom's restrictive font-lock and org-src settings."
  (org-syntax-debug-log 'info "Overriding Doom font-lock restrictions")
  
  ;; Force maximum font-lock decoration
  (setq-local font-lock-maximum-decoration t)
  (setq-local font-lock-maximum-size nil)
  
  ;; Disable performance optimizations that break syntax highlighting
  (setq-local font-lock-support-mode nil)
  (setq-local jit-lock-stealth-time nil)
  (setq-local jit-lock-defer-time nil)
  (setq-local jit-lock-stealth-nice nil)
  
  ;; CRITICAL: Force org-src fontification settings
  (setq-local org-src-fontify-natively t)
  (setq-local org-src-preserve-indentation t)
  (setq-local org-src-tab-acts-natively t)
  
  ;; Force font-lock keywords to be applied
  (setq-local org-src-block-faces nil)  ; Remove custom faces that interfere
  
  ;; DOOM SPECIFIC: Override org-mode font-lock restrictions
  (when (boundp '+org-src-block-faces)
    (setq-local +org-src-block-faces nil))
  
  ;; Ensure org-fontify-meta-lines-and-blocks runs properly
  (setq-local org-highlight-latex-and-related '(latex script entities)))

;; ================================================================
;;; 3. MANUAL FONTIFICATION TRIGGERS
;; ================================================================

(defun org-syntax-force-src-fontification ()
  "Force syntax highlighting using org-mode's built-in refresh mechanism."
  (org-syntax-debug-log 'info "Forcing org-mode syntax highlighting refresh")
  (when (derived-mode-p 'org-mode)
    ;; Use org-mode's native refresh mechanism
    (org-restart-font-lock)
    ;; Additional refresh to ensure src blocks are processed
    (font-lock-fontify-buffer)
    (org-syntax-debug-log 'info "Org-mode syntax highlighting refreshed")))

(defun org-syntax-fontify-src-block (lang start end)
  "Fontify a specific source block with LANG from START to END using org-mode's native mechanism."
  (org-syntax-debug-log 'info "Requesting fontification for %s block at %d-%d" lang start end)
  
  ;; Use org-mode's built-in fontification by calling org-fontify-meta-lines-and-blocks
  (save-excursion
    (goto-char start)
    (let ((element (org-element-at-point)))
      (when (eq (org-element-type element) 'src-block)
        (org-syntax-debug-log 'info "Found org src-block element, applying fontification")
        ;; Force org-mode to re-fontify this specific block
        (org-fontify-meta-lines-and-blocks-1 (org-element-property :begin element)
                                             (org-element-property :end element))))))

(defun org-syntax-copy-fontification (src-buffer start end)
  "Copy fontification from SRC-BUFFER to current buffer region START to END."
  (let ((src-props (with-current-buffer src-buffer
                     (buffer-substring (point-min) (point-max)))))
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert src-props))))

;; ================================================================
;;; 4. BUFFER-LOCAL FONTIFICATION HOOKS
;; ================================================================

(defun org-syntax-setup-buffer-local-hooks ()
  "Setup buffer-local hooks for syntax highlighting maintenance."
  (org-syntax-debug-log 'info "Setting up buffer-local fontification hooks")
  
  ;; Hook for after changes to re-fontify affected blocks
  (add-hook 'after-change-functions #'org-syntax-after-change-fontify nil t)
  
  ;; Hook for when entering org-edit-src-code
  (advice-add 'org-edit-src-code :before #'org-syntax-before-src-edit)
  
  ;; Hook for when exiting org-edit-src-code  
  (advice-add 'org-edit-src-exit :after #'org-syntax-after-src-exit))

(defun org-syntax-after-change-fontify (beg end len)
  "Re-fontify source blocks after buffer changes from BEG to END with LEN."
  (when (and (derived-mode-p 'org-mode)
             org-src-fontify-natively)
    (save-excursion
      (goto-char beg)
      (when (re-search-backward "^[ \t]*#\\+begin_src" nil t)
        (let ((block-start (match-beginning 0)))
          (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
            (let ((block-end (match-end 0)))
              (when (and (>= end block-start) (<= beg block-end))
                (org-syntax-debug-log 'info "Re-fontifying block after change at %d-%d" beg end)
                (run-with-idle-timer 0.1 nil #'org-syntax-force-src-fontification)))))))))

(defun org-syntax-before-src-edit ()
  "Prepare for org-edit-src-code to ensure proper syntax highlighting."
  (org-syntax-debug-log 'info "Preparing for src edit - ensuring fontification"))

(defun org-syntax-after-src-exit ()
  "Clean up after org-edit-src-exit and ensure proper fontification."
  (org-syntax-debug-log 'info "After src edit - refreshing fontification")
  (run-with-idle-timer 0.1 nil #'org-syntax-force-src-fontification))

;; ================================================================
;;; 5. COMPREHENSIVE INITIALIZATION SYSTEM
;; ================================================================

(defun org-syntax-comprehensive-setup ()
  "Comprehensive setup for org-mode syntax highlighting in Doom Emacs."
  (org-syntax-debug-log 'info "Starting comprehensive org-mode syntax highlighting setup")
  
  ;; Step 1: Disable tree-sitter interference
  (org-syntax-disable-tree-sitter-in-src)
  
  ;; Step 2: Override Doom restrictions
  (org-syntax-override-doom-restrictions)
  
  ;; Step 3: Setup buffer-local hooks
  (org-syntax-setup-buffer-local-hooks)
  
  ;; Step 4: Force immediate fontification with delay to allow mode setup
  (run-with-idle-timer 0.2 nil #'org-syntax-force-src-fontification)
  
  ;; Step 5: Setup additional timer to catch any missed blocks
  (run-with-idle-timer 1.0 nil #'org-syntax-force-src-fontification)
  
  (org-syntax-debug-log 'info "Comprehensive org-mode syntax highlighting setup completed"))

;; ================================================================
;;; 6. TEST AND DIAGNOSTIC FUNCTIONS
;; ================================================================

(defun org-syntax-test-highlighting ()
  "Test syntax highlighting with a minimal example."
  (interactive)
  (let ((test-buffer (get-buffer-create "*org-syntax-test*")))
    (with-current-buffer test-buffer
      (org-mode)
      (erase-buffer)
      (insert "* Test Syntax Highlighting\n\n")
      (insert "#+begin_src python\n")
      (insert "def hello_world():\n")
      (insert "    print(\"Hello, World!\")\n")
      (insert "    return 42\n")
      (insert "#+end_src\n\n")
      (insert "#+begin_src R\n")
      (insert "library(ggplot2)\n")
      (insert "data <- mtcars\n")
      (insert "plot(data$mpg, data$hp)\n")
      (insert "#+end_src\n\n")
      (insert "#+begin_src sas\n")
      (insert "proc print data=sashelp.cars;\n")
      (insert "run;\n")
      (insert "#+end_src\n\n")
      (insert "#+begin_src stata\n")
      (insert "sysuse auto\n")
      (insert "scatter price mpg\n")
      (insert "#+end_src\n")
      
      ;; Force setup and fontification
      (org-syntax-comprehensive-setup)
      (font-lock-fontify-buffer)
      
      ;; Display the buffer
      (pop-to-buffer test-buffer)
      (goto-char (point-min))
      (message "Created test buffer with syntax highlighting examples"))))

(defun org-syntax-diagnose ()
  "Diagnose current org-mode syntax highlighting configuration."
  (interactive)
  (let ((diagnosis (list)))
    (push (format "org-src-fontify-natively: %s" org-src-fontify-natively) diagnosis)
    (push (format "font-lock-mode: %s" font-lock-mode) diagnosis)
    (push (format "font-lock-maximum-decoration: %s" font-lock-maximum-decoration) diagnosis)
    (push (format "tree-sitter-mode: %s" (bound-and-true-p tree-sitter-mode)) diagnosis)
    (push (format "org-src-lang-modes: %s" org-src-lang-modes) diagnosis)
    (push (format "org-babel-load-languages: %s" org-babel-load-languages) diagnosis)
    (push (format "Current major mode: %s" major-mode) diagnosis)
    
    (with-current-buffer (get-buffer-create "*org-syntax-diagnosis*")
      (erase-buffer)
      (insert "=== Org-Mode Syntax Highlighting Diagnosis ===\n\n")
      (dolist (item diagnosis)
        (insert item "\n"))
      (insert "\n=== Recent Debug Log Entries ===\n")
      (when (file-exists-p org-syntax-debug-log-file)
        (insert-file-contents org-syntax-debug-log-file nil
                             (max 0 (- (nth 7 (file-attributes org-syntax-debug-log-file)) 2000))
                             nil))
      (pop-to-buffer (current-buffer)))))

;; ================================================================
;;; 7. INTEGRATION WITH DOOM EMACS
;; ================================================================

;; Hook into org-mode loading
(add-hook 'org-mode-hook #'org-syntax-comprehensive-setup)

;; Ensure setup runs after org-mode is fully loaded
(with-eval-after-load 'org
  (org-syntax-debug-log 'info "org-syntax-fix loaded with org-mode"))

;; Integration with existing config
(defun org-syntax-integrate-with-doom ()
  "Integrate syntax fix with existing Doom configuration."
  (org-syntax-debug-log 'info "Integrating org-syntax-fix with Doom Emacs")
  
  ;; Override any conflicting settings from main config
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  
  ;; Ensure our hooks have priority
  (remove-hook 'org-mode-hook #'org-syntax-comprehensive-setup)
  (add-hook 'org-mode-hook #'org-syntax-comprehensive-setup 90))

;; Run integration when this module is loaded
(org-syntax-integrate-with-doom)

(provide 'org-syntax-fix)
;;; org-syntax-fix.el ends here