;;; org-syntax-simple.el --- Simple org-mode syntax highlighting fix for Doom Emacs

;;; Commentary:
;; A minimal approach to fix org-mode syntax highlighting in Doom Emacs
;; by ensuring org-src-fontify-natively works properly.

;;; Code:

;; Debug logging
(defvar org-syntax-simple-log-file (expand-file-name "org-syntax-simple.log" "~/"))

(defun org-syntax-simple-log (message)
  "Log a message with timestamp."
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] %s\n" timestamp message))
      (append-to-file (point-min) (point-max) org-syntax-simple-log-file))))

;; Simple fix: ensure org-src-fontify-natively is respected
(defun org-syntax-simple-setup ()
  "Simple setup to ensure org-src syntax highlighting works."
  (org-syntax-simple-log "Setting up simple org-mode syntax highlighting fix")
  
  ;; Force the essential settings
  (setq-local org-src-fontify-natively t)
  (setq-local org-src-preserve-indentation t)
  (setq-local org-src-tab-acts-natively t)
  
  ;; Disable any Doom optimizations that interfere
  (setq-local font-lock-maximum-decoration t)
  (setq-local font-lock-support-mode nil)
  
  ;; CRITICAL: Override Doom's org-src-block-faces that prevent syntax highlighting
  (setq-local org-src-block-faces nil)
  
  ;; Force org-mode to use proper syntax highlighting in src blocks
  (when (derived-mode-p 'org-mode)
    ;; Advice org-fontify-meta-lines-and-blocks to ensure src blocks get fontified
    (advice-add 'org-fontify-meta-lines-and-blocks 
                :after #'org-syntax-simple-force-src-fontification)
    
    ;; Immediate refresh
    (run-with-idle-timer 0.1 nil #'org-syntax-simple-refresh-buffer))
  
  (org-syntax-simple-log "Simple org-mode syntax highlighting setup completed"))

(defun org-syntax-simple-force-src-fontification (&rest args)
  "Force fontification of source blocks after org fontification runs."
  (org-syntax-simple-log "Forcing source block fontification")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+begin_src\\s-+\\([a-zA-Z0-9_-]+\\)" nil t)
      (let ((lang (match-string 1))
            (start (save-excursion (beginning-of-line) (forward-line 1) (point)))
            (end (save-excursion 
                   (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
                     (beginning-of-line)
                     (point)))))
        (when end
          (org-syntax-simple-log (format "Fontifying %s block from %d to %d" lang start end))
          (org-syntax-simple-fontify-region lang start end))))))

(defun org-syntax-simple-fontify-region (lang start end)
  "Fontify region from START to END using LANG mode."
  (let* ((lang-mode (assoc lang org-src-lang-modes))
         (mode (when lang-mode (cdr lang-mode)))
         (mode-func (when mode (intern (format "%s-mode" mode)))))
    (when (and mode-func (fboundp mode-func))
      (let ((text (buffer-substring-no-properties start end)))
        (with-temp-buffer
          (insert text)
          (funcall mode-func)
          (font-lock-fontify-buffer)
          (let ((fontified-text (buffer-string)))
            (save-excursion
              (goto-char start)
              (delete-region start end)
              (insert fontified-text))))))))

(defun org-syntax-simple-refresh-buffer ()
  "Refresh the current buffer's fontification."
  (when (buffer-live-p (current-buffer))
    (with-current-buffer (current-buffer)
      (font-lock-fontify-buffer))))

;; Test function
(defun org-syntax-simple-test ()
  "Create a simple test buffer to verify syntax highlighting."
  (interactive)
  (let ((test-buffer (get-buffer-create "*org-syntax-simple-test*")))
    (with-current-buffer test-buffer
      (org-mode)
      (erase-buffer)
      (insert "* Test Buffer\n\n")
      (insert "#+begin_src python\n")
      (insert "def test_function():\n")
      (insert "    return \"Hello, World!\"\n")
      (insert "#+end_src\n")
      
      ;; Apply the fix
      (org-syntax-simple-setup)
      
      ;; Display the buffer
      (pop-to-buffer test-buffer)
      (goto-char (point-min))
      (message "Created simple test buffer - check if 'def' is highlighted"))))

;; Hook into org-mode
(add-hook 'org-mode-hook #'org-syntax-simple-setup)

;; Integration with after! org for Doom
(with-eval-after-load 'org
  (org-syntax-simple-log "org-syntax-simple loaded with org-mode"))

(provide 'org-syntax-simple)
;;; org-syntax-simple.el ends here