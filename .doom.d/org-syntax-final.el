;;; org-syntax-final.el --- Comprehensive fix for org-mode syntax highlighting in Doom Emacs

;;; Commentary:
;; This module provides a comprehensive solution for org-mode syntax highlighting
;; issues in Doom Emacs by addressing the root cause: font-lock configuration problems.

;;; Code:

;; Debug logging
(defvar org-syntax-final-log-file (expand-file-name "org-syntax-final.log" "~/"))

(defun org-syntax-final-log (message)
  "Log a message with timestamp."
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] %s\n" timestamp message))
      (append-to-file (point-min) (point-max) org-syntax-final-log-file))))

;; Core fix function
(defun org-syntax-final-fix ()
  "Comprehensive fix for org-mode syntax highlighting in Doom Emacs."
  (org-syntax-final-log "Applying comprehensive org-mode syntax highlighting fix")
  
  ;; Step 1: Ensure org-src-fontify-natively is enabled globally
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  
  ;; Step 2: Fix font-lock settings that Doom might have broken
  (setq font-lock-maximum-decoration t)
  (setq font-lock-keywords-only nil)  ; Critical for syntax highlighting
  
  ;; Step 3: Remove any org-src-block-faces that override syntax highlighting
  (when (boundp 'org-src-block-faces)
    (setq org-src-block-faces nil))
  
  ;; Step 4: Ensure proper font-lock initialization for language modes
  (advice-add 'org-src-font-lock-fontify-block :around #'org-syntax-final-fontify-wrapper)
  
  (org-syntax-final-log "Comprehensive fix applied"))

;; Wrapper function to ensure proper font-lock setup
(defun org-syntax-final-fontify-wrapper (orig-func lang start end)
  "Wrapper around org-src-font-lock-fontify-block to ensure proper font-lock setup."
  (org-syntax-final-log (format "Fontifying %s block from %d to %d" lang start end))
  
  ;; Get the language mode
  (let* ((lang-mode (org-src-get-lang-mode lang))
         (mode-func (and lang-mode (intern (format "%s-mode" lang-mode)))))
    
    (if (and mode-func (fboundp mode-func))
        (progn
          (org-syntax-final-log (format "Using mode function: %s" mode-func))
          
          ;; Extract the code content
          (let ((code-start (save-excursion
                             (goto-char start)
                             (re-search-forward "^[ \t]*#\\+begin_src.*$" end t)
                             (forward-line 1)
                             (point)))
                (code-end (save-excursion
                           (goto-char end)
                           (re-search-backward "^[ \t]*#\\+end_src" start t)
                           (point))))
            
            (when (and code-start code-end (< code-start code-end))
              (org-syntax-final-log (format "Code region: %d to %d" code-start code-end))
              
              ;; Apply syntax highlighting using temporary buffer
              (let ((code-text (buffer-substring-no-properties code-start code-end)))
                (with-temp-buffer
                  ;; Set up the language mode properly
                  (funcall mode-func)
                  (font-lock-mode 1)
                  
                  ;; Insert the code and fontify it
                  (insert code-text)
                  (font-lock-fontify-buffer)
                  
                  ;; Copy the fontified text back to the original buffer
                  (let ((fontified-text (buffer-string)))
                    (with-current-buffer (marker-buffer (point-marker))
                      (save-excursion
                        (goto-char code-start)
                        (delete-region code-start code-end)
                        (insert fontified-text)
                        (org-syntax-final-log (format "Applied fontification to %s block" lang))))))))))
      
      ;; Fallback to original function
      (funcall orig-func lang start end))))

;; Hook for org-mode buffers
(defun org-syntax-final-setup-buffer ()
  "Setup syntax highlighting for org-mode buffer."
  (when (derived-mode-p 'org-mode)
    (org-syntax-final-log "Setting up org-mode buffer for syntax highlighting")
    
    ;; Apply buffer-local settings
    (setq-local org-src-fontify-natively t)
    (setq-local font-lock-keywords-only nil)
    (setq-local font-lock-maximum-decoration t)
    
    ;; Force refresh after a short delay
    (run-with-idle-timer 0.1 nil 
                        (lambda ()
                          (when (buffer-live-p (current-buffer))
                            (with-current-buffer (current-buffer)
                              (font-lock-fontify-buffer)
                              (org-syntax-final-log "Buffer fontification refreshed")))))))

;; Test function that creates a comprehensive example
(defun org-syntax-final-test ()
  "Test comprehensive syntax highlighting fix."
  (interactive)
  (let ((test-buffer (get-buffer-create "*org-syntax-final-test*")))
    (with-current-buffer test-buffer
      (org-mode)
      (erase-buffer)
      (insert "* Comprehensive Syntax Highlighting Test\n\n")
      
      ;; Python example
      (insert "** Python Code\n\n")
      (insert "#+begin_src python\n")
      (insert "def fibonacci(n):\n")
      (insert "    \"\"\"Calculate fibonacci number.\"\"\"\n")
      (insert "    if n <= 1:\n")
      (insert "        return n\n")
      (insert "    else:\n")
      (insert "        return fibonacci(n-1) + fibonacci(n-2)\n")
      (insert "\n")
      (insert "# Test the function\n")
      (insert "result = fibonacci(10)\n")
      (insert "print(f'Fibonacci(10) = {result}')\n")
      (insert "#+end_src\n\n")
      
      ;; Elisp example
      (insert "** Emacs Lisp Code\n\n") 
      (insert "#+begin_src elisp\n")
      (insert "(defun greet (name)\n")
      (insert "  \"Greet someone by name.\"\n")
      (insert "  (interactive \"sName: \")\n")
      (insert "  (message \"Hello, %s!\" name))\n")
      (insert "\n")
      (insert ";; Call the function\n")
      (insert "(greet \"World\")\n")
      (insert "#+end_src\n\n")
      
      ;; R example
      (insert "** R Code\n\n")
      (insert "#+begin_src R\n")
      (insert "# Load required library\n")
      (insert "library(ggplot2)\n")
      (insert "\n")
      (insert "# Create sample data\n")
      (insert "data <- data.frame(\n")
      (insert "  x = 1:10,\n")
      (insert "  y = rnorm(10)\n")
      (insert ")\n")
      (insert "\n")
      (insert "# Create plot\n")
      (insert "ggplot(data, aes(x = x, y = y)) + \n")
      (insert "  geom_point() + \n")
      (insert "  geom_line()\n")
      (insert "#+end_src\n")
      
      ;; Apply the fix to this buffer
      (org-syntax-final-setup-buffer)
      
      ;; Display the buffer
      (pop-to-buffer test-buffer)
      (goto-char (point-min))
      (message "Created comprehensive test buffer - check syntax highlighting"))))

;; Apply fix on org-mode load
(with-eval-after-load 'org
  (org-syntax-final-fix))

;; Hook into org-mode
(add-hook 'org-mode-hook #'org-syntax-final-setup-buffer)

(provide 'org-syntax-final)
;;; org-syntax-final.el ends here