;;; test-missing-functions.el --- Test missing functions

(require 'org)

(defun euporie-termint-detect-kernel ()
  "Detect kernel from org-src buffer language or org-mode code block."
  (let ((lang-from-buffer-name (when (string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" (buffer-name))
                                 (match-string 1 (buffer-name))))
        (lang-from-variable (bound-and-true-p org-src--lang))
        (lang-from-org-element (when (eq major-mode 'org-mode)
                                 (org-element-property :language (org-element-at-point)))))
    
    (let ((detected-lang (or lang-from-variable lang-from-buffer-name lang-from-org-element)))
      (cond
       ((or (and detected-lang (or (string-equal detected-lang "r") (string-equal detected-lang "R"))) 
            (eq major-mode 'ess-r-mode)) 
        "r")
       ((or (and detected-lang (string-equal detected-lang "python")) 
            (eq major-mode 'python-mode) 
            (eq major-mode 'python-ts-mode)) 
        "python")
       ((or (and detected-lang (string-equal detected-lang "stata")) 
            (eq major-mode 'stata-mode)) 
        "stata")
       ((or (and detected-lang (or (string-equal detected-lang "sas") (string-equal detected-lang "SAS"))) 
            (eq major-mode 'SAS-mode)) 
        "sas")
       (t "python")))))

(defun euporie-termint-display-console-right (buffer &optional original-buffer original-window)
  "Display console BUFFER in a right split window."
  (let ((initial-window (or original-window (selected-window)))
        (initial-buffer (or original-buffer (current-buffer))))
    
    ;; Check if we're in an org-src edit buffer - if so, preserve it on the left
    (if (and (buffer-live-p initial-buffer)
             (string-match-p "\\*Org Src.*\\[" (buffer-name initial-buffer)))
        (progn
          ;; Org-src context: create explicit split with org-src on left
          (delete-other-windows)
          (set-window-buffer (selected-window) initial-buffer)
          (let ((right-window (split-window-right)))
            (with-selected-window right-window
              (set-window-buffer right-window buffer)
              (goto-char (point-max)))
            ;; Stay in org-src buffer (left window)
            (select-window initial-window)))
      
      ;; Non org-src context: use standard display
      (let ((console-window (display-buffer buffer
                                            '((display-buffer-reuse-window
                                               display-buffer-in-side-window)
                                              (side . right)
                                              (window-width . 0.5)
                                              (inhibit-same-window . t)))))
        (when console-window
          ;; Scroll to bottom in console
          (with-selected-window console-window
            (goto-char (point-max)))
          
          ;; Restore focus to original window
          (when (window-live-p initial-window)
            (select-window initial-window))
          
          (when (buffer-live-p initial-buffer)
            (set-window-buffer (selected-window) initial-buffer)))))))

(provide 'test-missing-functions)
;;; test-missing-functions.el ends here