;;; org-syntax-overlay.el --- Overlay-based syntax highlighting fix for org-mode in Doom Emacs

;;; Commentary:
;; This module uses overlays to apply syntax highlighting to org-mode source blocks
;; when the standard font-lock approach fails in Doom Emacs.

;;; Code:

;; Debug logging
(defvar org-syntax-overlay-log-file (expand-file-name "org-syntax-overlay.log" "~/"))

(defun org-syntax-overlay-log (message)
  "Log a message with timestamp."
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] %s\n" timestamp message))
      (append-to-file (point-min) (point-max) org-syntax-overlay-log-file))))

;; Store overlays for cleanup
(defvar-local org-syntax-overlay-list nil
  "List of overlays created for syntax highlighting.")

;; Apply syntax highlighting using overlays
(defun org-syntax-overlay-apply ()
  "Apply syntax highlighting to all source blocks using overlays."
  (interactive)
  (org-syntax-overlay-log "Applying overlay-based syntax highlighting")
  
  ;; Clear existing overlays
  (org-syntax-overlay-clear)
  
  (save-excursion
    (goto-char (point-min))
    (let ((blocks-processed 0))
      (while (re-search-forward "^[ \t]*#\\+begin_src\\s-+\\([a-zA-Z0-9_-]+\\)" nil t)
        (let* ((lang (match-string 1))
               (code-start (save-excursion (forward-line 1) (point)))
               (code-end (save-excursion
                          (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
                            (beginning-of-line)
                            (point)))))
          (when code-end
            (org-syntax-overlay-log (format "Processing %s block from %d to %d" lang code-start code-end))
            (org-syntax-overlay-fontify-region lang code-start code-end)
            (setq blocks-processed (1+ blocks-processed)))))
      
      (org-syntax-overlay-log (format "Processed %d blocks with overlay syntax highlighting" blocks-processed))
      (message "Applied overlay syntax highlighting to %d blocks" blocks-processed))))

(defun org-syntax-overlay-fontify-region (lang start end)
  "Apply syntax highlighting to region using overlays."
  (let* ((lang-mode (assoc lang org-src-lang-modes))
         (mode (when lang-mode (cdr lang-mode)))
         (mode-func (when mode (intern (format "%s-mode" mode)))))
    
    (when (and mode-func (fboundp mode-func))
      (org-syntax-overlay-log (format "Applying %s syntax highlighting via overlays" mode-func))
      
      (let ((code-text (buffer-substring-no-properties start end)))
        (with-temp-buffer
          ;; Set up the language mode
          (funcall mode-func)
          (font-lock-mode 1)
          (insert code-text)
          (font-lock-fontify-buffer)
          
          ;; Extract the face information and create overlays
          (let ((pos (point-min)))
            (while (< pos (point-max))
              (let ((face (get-text-property pos 'face))
                    (next-pos (next-single-property-change pos 'face nil (point-max))))
                (when face
                  ;; Create overlay in the original buffer
                  (let* ((overlay-start (+ start (- pos (point-min))))
                         (overlay-end (+ start (- next-pos (point-min))))
                         (overlay (make-overlay overlay-start overlay-end)))
                    (overlay-put overlay 'face face)
                    (overlay-put overlay 'org-syntax-overlay t)
                    (push overlay org-syntax-overlay-list)
                    (org-syntax-overlay-log (format "Created overlay %d-%d with face %s" 
                                                   overlay-start overlay-end face))))
                (setq pos next-pos)))))))))

(defun org-syntax-overlay-clear ()
  "Clear all syntax highlighting overlays."
  (interactive)
  (org-syntax-overlay-log "Clearing syntax highlighting overlays")
  (dolist (overlay org-syntax-overlay-list)
    (when (overlay-buffer overlay)
      (delete-overlay overlay)))
  (setq org-syntax-overlay-list nil)
  (message "Cleared syntax highlighting overlays"))

;; Hook function to auto-apply overlays
(defun org-syntax-overlay-setup ()
  "Setup overlay syntax highlighting for org-mode buffer."
  (when (derived-mode-p 'org-mode)
    (org-syntax-overlay-log "Setting up overlay syntax highlighting for org-mode buffer")
    
    ;; Apply overlays after a short delay
    (run-with-idle-timer 0.5 nil 
                        (lambda ()
                          (when (buffer-live-p (current-buffer))
                            (with-current-buffer (current-buffer)
                              (org-syntax-overlay-apply)))))))

;; Test function
(defun org-syntax-overlay-test ()
  "Test overlay-based syntax highlighting."
  (interactive)
  (let ((test-buffer (get-buffer-create "*org-syntax-overlay-test*")))
    (with-current-buffer test-buffer
      (org-mode)
      (erase-buffer)
      (insert "* Overlay-Based Syntax Highlighting Test\n\n")
      (insert "#+begin_src python\n")
      (insert "def test_function():\n")
      (insert "    \"\"\"A test function with docstring.\"\"\"\n")
      (insert "    x = 42  # An integer\n")
      (insert "    return x * 2\n")
      (insert "#+end_src\n\n")
      (insert "#+begin_src elisp\n")
      (insert "(defun test-elisp ()\n")
      (insert "  \"Test Emacs Lisp function.\"\n")
      (insert "  (let ((x 42))\n")
      (insert "    (message \"Result: %d\" (* x 2))))\n")
      (insert "#+end_src\n")
      
      ;; Apply overlay syntax highlighting
      (org-syntax-overlay-apply)
      
      ;; Display the buffer
      (pop-to-buffer test-buffer)
      (goto-char (point-min))
      (message "Created overlay test buffer with syntax highlighting"))))

;; Add keybindings for manual control
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x h") #'org-syntax-overlay-apply)
  (define-key org-mode-map (kbd "C-c C-x H") #'org-syntax-overlay-clear))

;; Auto-setup for org-mode buffers
(add-hook 'org-mode-hook #'org-syntax-overlay-setup)

(provide 'org-syntax-overlay)
;;; org-syntax-overlay.el ends here
