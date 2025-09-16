;;; euporie-termint.el --- Euporie Console Integration with Termint -*- lexical-binding: t; -*-

;; Author: Claude
;; Description: Euporie console integration using termint with eat backend
;; Keywords: euporie, jupyter, remote, termint

;;; Commentary:
;;
;; This module provides euporie console integration that handles:
;; - Local execution: Direct termint-define
;; - SSH execution: Remote termint execution via TRAMP
;;
;; Architecture: Euporie console integration with termint and eat backend
;;
;;; Code:

(require 'termint)

;;;; Helper Functions

(defun euporie-parse-context ()
  "Parse org-babel context from current buffer.
Returns: (language dir execution-context) tuple"
  (let ((language nil)
        (dir nil)
        (execution-context 'local))

    ;; Extract language and dir from org-babel info
    (when (boundp 'org-src--babel-info)
      (let ((babel-info org-src--babel-info))
        (when babel-info
          (setq language (downcase (car babel-info)))  ; First element is the language
          (setq dir (cdr (assoc :dir (nth 2 babel-info)))))))  ; Third element is params

    ;; Fallback: try to detect from major mode if babel-info not available
    (unless language
      (setq language (pcase major-mode
                       ('python-mode "python")
                       ('ess-r-mode "r")
                       ('SAS-mode "sas")
                       ('stata-mode "stata")
                       ('fundamental-mode
                        ;; In org-src buffers, try to guess from buffer name
                        (cond
                         ((string-match-p "\\*Org Src.*python" (buffer-name)) "python")
                         ((string-match-p "\\*Org Src.*r\\*" (buffer-name)) "r")
                         ((string-match-p "\\*Org Src.*sas" (buffer-name)) "sas")
                         ((string-match-p "\\*Org Src.*stata" (buffer-name)) "stata")
                         (t nil)))
                       (_ nil))))

    ;; Determine execution context from dir
    (when dir
      (setq execution-context
            (cond
             ;; Multi-hop TRAMP path with qrsh (e.g., /sshx:wrds-uva|qrsh::/path)
             ((and (file-remote-p dir)
                   (string-match-p "|qrsh::" dir))
              'ssh-qrsh)

             ;; SSH-only TRAMP path (e.g., /ssh:user@host:/path)
             ((file-remote-p dir)
              'ssh)

             ;; Local path
             (t 'local))))

    ;; Return tuple
    (list language dir execution-context)))


(defun euporie-language-to-kernel (language)
  "Map LANGUAGE to euporie kernel name."
  (pcase language
    ("python" "python3")
    ("r" "ir")
    ("stata" "stata")
    ("sas" "sas")
    (_ language)))

(defun euporie-buffer-to-session (buffer-name)
  "Convert BUFFER-NAME to session ID for termint functions."
  ;; Remove asterisks and convert to session ID
  ;; "*euporie-python*" -> "euporie-python"
  (replace-regexp-in-string "[*]" "" buffer-name))


(defun euporie-cleanup-termint-functions (session-id)
  "Clean up existing termint functions for SESSION-ID to prevent conflicts."
  (dolist (func-suffix '("start" "send-string" "send-region" "send-buffer" 
                        "send-defun" "send-paragraph" "source-region" "source-buffer"
                        "source-defun" "source-paragraph" "hide-window" "window"))
    (let ((func-name (intern (format "termint-%s-%s" session-id func-suffix))))
      (when (fboundp func-name)
        (fmakunbound func-name))))
  
  ;; Clean up termint variables
  (dolist (var-suffix '("cmd" "backend" "use-bracketed-paste-mode" "start-pattern" 
                       "end-pattern" "str-process-func" "source-syntax" 
                       "show-source-command-hint" "send-delayed-final-ret"))
    (let ((var-name (intern (format "termint-%s-%s" session-id var-suffix))))
      (when (boundp var-name)
        (makunbound var-name)))))



;;;; Main Unified Function

(defun euporie-ensure-session (language dir execution-context &optional session)
  "Ensure euporie session exists for LANGUAGE, starting if needed.
LANGUAGE: programming language name
DIR: directory (local path or TRAMP path)
EXECUTION-CONTEXT: 'local, 'ssh, or 'ssh-qrsh
SESSION: optional session name suffix
Returns the session-id and send-func."

  (let* ((session-suffix (if session (concat "-" session) ""))
         (buffer-name (format "*euporie-%s%s*" language session-suffix))
         (kernel-name (euporie-language-to-kernel language))
         (session-id (euporie-buffer-to-session buffer-name))
         (send-func (intern (format "termint-%s-send-string" session-id))))
    
    ;; Check if session already exists
    (if (and (fboundp send-func) (get-buffer (format "*%s*" session-id)))
        ;; Session exists, return info
        (cons session-id send-func)
      
      ;; Session doesn't exist, create it
      (euporie-cleanup-termint-functions session-id)
      
      ;; Step 0: Compute base termint command and define session
      (let ((base-command (pcase execution-context
                            ('local "bash")
                            ((or 'ssh 'ssh-qrsh) (format "ssh -t -q %s" (file-remote-p dir 'host))))))
        
        (eval `(termint-define ,session-id 
                  ,base-command
                  :bracketed-paste-p t
                  :backend 'eat
                  :env '(("TERM" . "xterm-256color")
                         ("COLORTERM" . "truecolor")
                         ("EUPORIE_GRAPHICS" . "sixel")))))
      
      ;; Configure window display behavior for this buffer
      (add-to-list 'display-buffer-alist
                   `(,(format "\\*%s\\*" buffer-name)
                     (display-buffer-in-side-window display-buffer-reuse-window)
                     (window-width . 0.5)
                     (side . right)
                     (slot . -1)))
      
      ;; Start the session and execute sequential commands
      (let ((start-func (intern (format "termint-%s-start" session-id))))
        
        (when (fboundp start-func)
          (funcall start-func)
          
          ;; Sequential commands: 1. SSH (if remote), 2. qrsh (if needed), 3. euporie (always)
          (when (memq execution-context '(ssh ssh-qrsh))
            (sleep-for 1)) ; Wait for SSH connection

          (when (eq execution-context 'ssh-qrsh)
            (funcall send-func "qrsh -q interactive.q")
            (sleep-for 2)) ; Wait for qrsh allocation
          
          ;; Always send euporie command (now works for all contexts)
          (let* ((localname (if (eq execution-context 'local) 
                                (expand-file-name "~")
                                (file-remote-p dir 'localname)))
                 (euporie-cmd (format "cd %s && euporie console --kernel-name=%s"
                                     localname kernel-name)))
            (funcall send-func euporie-cmd)
            (sleep-for 3)) ; Wait for euporie startup
          
          ;; Rename buffer (display handled by display-buffer-alist)
          (let ((buffer (get-buffer (format "*%s*" session-id))))
            (when buffer
              (with-current-buffer buffer
                (rename-buffer buffer-name))))
          
          ;; Return session info
          (cons session-id send-func))))))


(defun euporie-send-region-or-paragraph ()
  "Send region or paragraph to euporie session."
  (interactive)
  ;; Parse context from current org-src buffer
  (let* ((context (euporie-parse-context))
         (language (nth 0 context))
         (dir (nth 1 context))
         (execution-context (nth 2 context)))

    (unless language
      (error "Cannot determine language. Not in org-src buffer with babel info."))

    ;; Ensure session exists
    (let* ((session-info (euporie-ensure-session language dir execution-context))
           (session-id (car session-info))
           (send-func (cdr session-info)))

      ;; Determine what to send
      (let ((text (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                      ;; Send paragraph
                      (save-excursion
                        (backward-paragraph)
                        (let ((start (point)))
                          (forward-paragraph)
                          (buffer-substring-no-properties start (point)))))))

        ;; Send the text with newline for immediate execution
        (funcall send-func (concat text "\n"))

        ;; Ensure org-src buffer remains editable after execution
        (when (and (boundp 'org-edit-src-content-indentation)
                   (buffer-local-value 'buffer-read-only (current-buffer)))
          (setq buffer-read-only nil))

        (message "Sent %d chars to %s" (length text) session-id)))))

(defun euporie-unified-execute (language body params)
  "Unified euporie execution (for org-babel compatibility)."
  (let* ((dir (cdr (assoc :dir params)))
         (session (cdr (assoc :session params)))
         ;; Determine execution context
         (execution-context (cond
                             ;; Multi-hop TRAMP path with qrsh
                             ((and dir (file-remote-p dir) (string-match-p "|qrsh::" dir))
                              'ssh-qrsh)
                             ;; SSH-only TRAMP path
                             ((and dir (file-remote-p dir))
                              'ssh)
                             ;; Local or no dir
                             (t 'local))))

    ;; Ensure session exists
    (euporie-ensure-session language dir execution-context session)
    
    ;; Send the code
    (when (and body (not (string-empty-p body)))
      (let* ((session-suffix (if session (concat "-" session) ""))
             (session-id (euporie-buffer-to-session (format "*euporie-%s%s*" language session-suffix)))
             (send-func (intern (format "termint-%s-send-string" session-id))))
        (funcall send-func body)))
    
    "")) ; Return empty for org-babel

;;;; Error Handling

(defun euporie-unified-execute-with-error-handling (language body params)
  "Wrapper for euporie-unified-execute with comprehensive error handling."
  (condition-case err
      (euporie-unified-execute language body params)
    
    (file-missing
     (message "Euporie error: %s not found in PATH" language)
     (format ";; Error: %s kernel not available" language))
    
    (tramp-file-error
     (message "Euporie error: TRAMP connection failed for remote execution")
     (format ";; Error: Remote connection failed"))
    
    (error
     (message "Euporie error: %s" (error-message-string err))
     (format ";; Error: %s" (error-message-string err)))))

;;;; Org-Babel Integration

(defun org-babel-execute:python (body params)
  "Execute Python BODY with PARAMS using unified euporie."
  (euporie-unified-execute-with-error-handling "python" body params))

(defun org-babel-execute:R (body params)
  "Execute R BODY with PARAMS using unified euporie."
  (euporie-unified-execute-with-error-handling "r" body params))

(defun org-babel-execute:stata (body params)
  "Execute Stata BODY with PARAMS using unified euporie."
  (euporie-unified-execute-with-error-handling "stata" body params))

(defun org-babel-execute:sas (body params)
  "Execute SAS BODY with PARAMS using unified euporie."
  (euporie-unified-execute-with-error-handling "sas" body params))

;;;; Interactive Functions

(defun euporie-start-console (language &optional remote-dir)
  "Interactively start euporie console for LANGUAGE with optional REMOTE-DIR."
  (interactive 
   (list (completing-read "Language: " '("python" "r" "stata" "sas"))
         (read-directory-name "Directory (empty for local): " nil nil t)))
  
  (let ((params (if (and remote-dir (not (string-empty-p remote-dir)))
                    `((:dir . ,remote-dir))
                  nil)))
    (euporie-unified-execute language "" params)))

;;;; Org-mode Integration

(defun euporie-auto-start-session ()
  "Automatically start euporie session when entering org-edit-special buffer."
  (let* ((context (euporie-parse-context))
         (language (nth 0 context))
         (dir (nth 1 context))
         (execution-context (nth 2 context)))

    ;; If we have a supported language, start the session
    (when (member language '("python" "r" "stata" "sas"))
      (message "DEBUG: Starting euporie session for %s with dir: %s" language dir)
      (euporie-ensure-session language dir execution-context)

      ;; CRITICAL: After session creation, focus the org-src buffer window
      ;; Find the org-src buffer (should contain "Org Src" and the language)
      (let ((org-src-buffer (car (seq-filter
                                   (lambda (buf)
                                     (and (string-match-p "\\*Org Src" (buffer-name buf))
                                          (string-match-p language (buffer-name buf))))
                                   (buffer-list)))))
        (when org-src-buffer
          (let ((org-src-window (get-buffer-window org-src-buffer)))
            (when org-src-window
              (select-window org-src-window)
              ;; Also switch to the org-src buffer in this window
              (switch-to-buffer org-src-buffer)
              (message "Focused org-src window and buffer for editing")))))

      (message "Started euporie session for %s in %s" language (if (and dir (file-remote-p dir)) "remote" "local")))))

;; Hook into org-edit-special
(add-hook 'org-src-mode-hook #'euporie-auto-start-session)

;;;; Note: Keybindings are now in config.el for proper Doom integration

(provide 'euporie-termint)

;;; euporie-unified.el ends here
