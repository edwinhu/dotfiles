;;; euporie-unified.el --- Unified Euporie Integration with Conditional SSH/qrsh Support -*- lexical-binding: t; -*-

;; Author: Claude
;; Description: Unified euporie integration building on tramp-qrsh foundation
;; Keywords: euporie, jupyter, remote, termint

;;; Commentary:
;; 
;; This module provides unified euporie integration that conditionally handles:
;; - Local execution: Direct termint-define
;; - SSH execution: tramp-qrsh pattern without qrsh step  
;; - SSH+qrsh execution: Full tramp-qrsh workflow
;;
;; Architecture: Layer euporie console on proven tramp-qrsh foundation
;;
;;; Code:

(require 'termint)

;; Global configuration - set termint backend to 'eat BEFORE any termint-define calls
(setq termint-backend 'eat)
(setq termint-default-backend 'eat)

;; Prevent CPR (Cursor Position Request) warnings by ensuring eat is properly configured
(when (boundp 'eat-term-terminfo-directory)
  (setq eat-term-terminfo-directory (expand-file-name "~/.terminfo")))

;; Ensure TERM environment is set correctly for eat
(setenv "TERM" "eat-truecolor")

;; Function to ensure termint is properly configured with eat backend
(defun euporie-ensure-termint-backend ()
  "Ensure termint backend is set to 'eat to prevent CPR warnings."
  (setq termint-backend 'eat)
  (when (boundp 'termint-default-backend)
    (setq termint-default-backend 'eat)))

;; Call configuration function immediately
(euporie-ensure-termint-backend)

;;;; Helper Functions

(defun euporie-parse-context (dir)
  "Parse org-babel :dir parameter to detect execution context.
Returns: 'local, 'ssh, or 'ssh-qrsh"
  (cond
   ;; No dir parameter or local path
   ((or (null dir) (not (file-remote-p dir)))
    'local)
   
   ;; Multi-hop TRAMP path with qrsh (e.g., /sshx:wrds|qrsh::/path)
   ((and (file-remote-p dir)
         (string-match-p "|qrsh::" dir))
    'ssh-qrsh)
   
   ;; SSH-only TRAMP path (e.g., /ssh:user@host:/path)
   ((file-remote-p dir)
    'ssh)
   
   ;; Default to local
   (t 'local)))

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

(defun euporie-wait-for-prompt (buffer &optional timeout)
  "Wait for euporie prompt in BUFFER with optional TIMEOUT (default 10s)."
  (let ((timeout (or timeout 10))
        (start-time (current-time))
        (prompt-found nil))
    (while (and (not prompt-found)
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (with-current-buffer buffer
        (let ((content (buffer-substring-no-properties 
                       (max 1 (- (point-max) 200)) (point-max))))
          (when (or (string-match-p "In \\[[0-9]+\\]:" content)  ; Jupyter prompt
                    (string-match-p ">>>" content)              ; Python prompt
                    (string-match-p "\\[1\\]" content))         ; R prompt
            (setq prompt-found t))))
      (unless prompt-found
        (sleep-for 0.2)))
    prompt-found))

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

(defun euporie-ensure-session (language &optional dir session)
  "Ensure euporie session exists for LANGUAGE, starting if needed.
DIR: optional directory (local path or TRAMP path)
SESSION: optional session name suffix
Returns the session-id and send-func."
  
  (let* ((session-suffix (if session (concat "-" session) ""))
         (buffer-name (format "*euporie-%s%s*" language session-suffix))
         (kernel-name (euporie-language-to-kernel language))
         (execution-context (euporie-parse-context dir))
         (session-id (euporie-buffer-to-session buffer-name))
         (send-func (intern (format "termint-%s-send-string" session-id))))
    
    ;; Check if session already exists
    (if (and (fboundp send-func) (get-buffer (format "*%s*" session-id)))
        ;; Session exists, return info
        (cons session-id send-func)
      
      ;; Session doesn't exist, create it
      (euporie-cleanup-termint-functions session-id)
      (euporie-ensure-termint-backend)
      
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
  "Send region or paragraph to euporie session, starting session if needed."
  (interactive)
  (let* ((language (or (and (boundp 'org-src-lang-modes)
                           (alist-get major-mode 
                                      (mapcar (lambda (x) (cons (cdr x) (car x))) 
                                              org-src-lang-modes)))
                      ;; Fallback to major mode detection
                      (pcase major-mode
                        ('python-mode "python")
                        ('ess-r-mode "r") 
                        ('SAS-mode "sas")
                        (_ (error "Cannot determine language from major mode: %s" major-mode)))))
         ;; Get dir from org-babel context if available
         (dir (when (boundp 'org-edit-src-content-indentation) 
                ;; We're in org-edit buffer, try to get dir from original org buffer
                default-directory))
         (session-info (euporie-ensure-session language dir))
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
      
      ;; Send the text
      (funcall send-func text)
      (message "Sent %d chars to %s" (length text) session-id))))

(defun euporie-unified-execute (language body params)
  "Unified euporie execution (for org-babel compatibility)."
  (let* ((dir (cdr (assoc :dir params)))
         (session (cdr (assoc :session params))))
    
    ;; Ensure session exists
    (euporie-ensure-session language dir session)
    
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
  (when (and (boundp 'org-src-lang-modes)
             (memq major-mode (mapcar #'cdr org-src-lang-modes)))
    (let* ((language (alist-get major-mode 
                                (mapcar (lambda (x) (cons (cdr x) (car x))) 
                                        org-src-lang-modes)))
           (dir default-directory))
      (when (member language '("python" "r" "stata" "sas"))
        (euporie-ensure-session language dir)
        (message "Started euporie session for %s" language)))))

;; Hook into org-edit-special
(add-hook 'org-src-mode-hook #'euporie-auto-start-session)

;; Keybinding for C-RET in org-src buffers
(with-eval-after-load 'org-src
  (define-key org-src-mode-map (kbd "C-RET") #'euporie-send-region-or-paragraph))

(provide 'euporie-unified)

;;; euporie-unified.el ends here