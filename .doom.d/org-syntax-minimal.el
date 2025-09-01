;;; org-syntax-minimal.el --- Minimal fix for org-mode syntax highlighting in Doom

;;; Commentary:
;; This module provides the minimal necessary fix to get syntax highlighting
;; working in org-mode code blocks under Doom Emacs.

;;; Code:

;; Debug logging
(defvar org-syntax-minimal-log-file (expand-file-name "org-syntax-minimal.log" "~/"))

(defun org-syntax-minimal-log (message)
  "Log a message with timestamp."
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] %s\n" timestamp message))
      (append-to-file (point-min) (point-max) org-syntax-minimal-log-file))))

;; Core fix: restore org-mode's native syntax highlighting behavior
(defun org-syntax-minimal-fix ()
  "Apply minimal fix for org-mode syntax highlighting."
  (org-syntax-minimal-log "Applying minimal org-mode syntax highlighting fix")
  
  ;; Step 1: Ensure org-src-fontify-natively is enabled
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t) 
  (setq org-src-tab-acts-natively t)
  
  ;; Step 2: Remove any Doom-specific overrides that break syntax highlighting
  (when (boundp 'org-src-block-faces)
    (setq org-src-block-faces nil))
  
  ;; Step 3: Ensure proper font-lock settings
  (setq font-lock-maximum-decoration t)
  
  ;; CRITICAL: This is the key setting that was preventing syntax highlighting!
  (setq font-lock-keywords-only nil)
  
  (org-syntax-minimal-log "Minimal fix applied - org-src-fontify-natively enabled and font-lock-keywords-only disabled"))

;; Apply fix when org-mode loads
(with-eval-after-load 'org
  (org-syntax-minimal-fix))

;; Test function
(defun org-syntax-minimal-test ()
  "Test syntax highlighting with a minimal example."
  (interactive)
  (let ((test-buffer (get-buffer-create "*org-syntax-minimal-test*")))
    (with-current-buffer test-buffer
      (org-mode)
      (erase-buffer)
      (insert "* Syntax Highlighting Test\n\n")
      (insert "#+begin_src python\n")
      (insert "def hello():\n")
      (insert "    print('Hello World')\n") 
      (insert "    return True\n")
      (insert "#+end_src\n\n")
      (insert "#+begin_src elisp\n")
      (insert "(defun test-function ()\n")
      (insert "  \"A test function.\"\n")
      (insert "  (message \"Hello from Elisp\"))\n")
      (insert "#+end_src\n")
      
      ;; Display buffer
      (pop-to-buffer test-buffer)
      (goto-char (point-min))
      
      ;; Check current values
      (message "org-src-fontify-natively: %s, font-lock-mode: %s" 
               org-src-fontify-natively font-lock-mode))))

(provide 'org-syntax-minimal)
;;; org-syntax-minimal.el ends here