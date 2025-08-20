;;; sas-console.el --- Interactive SAS console integration for org-babel -*- lexical-binding: t; -*-

;; Author: Generated for SAS integration
;; Description: Interactive SAS console sessions for WRDS integration

;;; Commentary:
;; This file provides interactive SAS console integration similar to jupyter-console
;; but specifically designed for WRDS isas sessions. It manages persistent SAS sessions
;; that can be used from org-babel and org-edit-src-code.

;;; Code:

(require 'comint)
(require 'org)
(require 'ob)
(require 'tramp)
(require 'wrds-debug)

(defgroup sas-console nil
  "Interactive SAS console integration."
  :group 'org-babel)

(defcustom sas-console-prompt-regexp
  "\\(?:[0-9]+\\s-*\\?\\s-*\\|\\?\\s-*\\|>\\s-*\\|ERROR\\|WARNING\\|NOTE\\)"
  "Regular expression matching SAS console prompts."
  :type 'string
  :group 'sas-console)

(defcustom sas-console-connection-timeout 30
  "Timeout in seconds for establishing SAS console connection."
  :type 'integer
  :group 'sas-console)

(defvar sas-console-buffers (make-hash-table :test 'equal)
  "Hash table mapping (file . session) pairs to their associated SAS console buffers.")

(defvar sas-console-processes (make-hash-table :test 'equal)
  "Hash table tracking SAS console processes.")

(defvar sas-console-first-run (make-hash-table :test 'equal)
  "Track if this is the first command sent to a buffer.")

;;; Session Management

(defun sas-console-buffer-name (session file)
  "Generate buffer name for SAS console SESSION associated with FILE."
  (format "*SAS Console[%s]:%s*" 
          (or session "default")
          (file-name-nondirectory (or file "global"))))

(defun sas-console-get-or-create (session &optional file)
  "Get existing or create new SAS console for SESSION.
Optionally associate with FILE."
  (wrds-debug-log 'info "Getting SAS console for session: %s, file: %s" session file)
  
  ;; In org-src-mode, use the original org file
  (let* ((actual-file (or file
                         (and (bound-and-true-p org-src-source-file-name) 
                              org-src-source-file-name)
                         (buffer-file-name)))
         ;; Use jupyter-console pattern: (cons file session) for consistent buffer association
         (buffer-key (cons actual-file (or session "default")))
         (buffer-name (sas-console-buffer-name session actual-file))
         (existing-buffer (gethash buffer-key sas-console-buffers)))
    
    (wrds-debug-log 'debug "Buffer key: %s, buffer name: %s" buffer-key buffer-name)
    
    (if (and existing-buffer (buffer-live-p existing-buffer))
        (progn
          (wrds-debug-log 'debug "Using existing SAS console buffer")
          existing-buffer)
      
      (wrds-debug-log 'info "Creating new SAS console buffer")
      (let ((buffer (sas-console-start-session session actual-file)))
        (when buffer
          (puthash buffer-key buffer sas-console-buffers)
          (wrds-debug-log 'info "SAS console created and stored: %s" buffer-name))
        buffer))))

(defun sas-console-start-session (session file)
  "Start a new SAS console session.
SESSION should be a TRAMP path like /sshx:wrds|qrsh::/ or nil for local.
FILE is the associated file for context."
  (let* ((buffer-name (sas-console-buffer-name session file))
         (process-buffer (get-buffer-create buffer-name))
         (is-remote (and session (string-match "^/sshx:" session))))
    
    (wrds-debug-log 'info "Starting SAS session - remote: %s, session: %s" is-remote session)
    
    (with-current-buffer process-buffer
      (unless (comint-check-proc process-buffer)
        (if is-remote
            (sas-console-start-remote-session process-buffer session)
          (sas-console-start-local-session process-buffer))
        
        ;; Set up buffer properties
        (comint-mode)
        (setq-local comint-prompt-regexp sas-console-prompt-regexp)
        (setq-local comint-prompt-read-only t)
        (setq-local comint-scroll-to-bottom-on-output t)
        
        ;; Start monitoring this buffer
        (wrds-debug-monitor-buffer buffer-name)
        
        ;; Wait for SAS to be ready
        (wrds-debug-log 'info "Waiting for SAS to be ready...")
        (if (sas-console-wait-for-ready process-buffer)
            (wrds-debug-log 'info "SAS console is ready!")
          (wrds-debug-log 'warn "SAS console may not be fully ready"))))
    
    process-buffer))

(defun sas-console-start-remote-session (buffer session-path)
  "Start a remote SAS session in BUFFER using SESSION-PATH."
  (wrds-debug-log 'info "Starting remote SAS session: %s" session-path)
  
  ;; Don't use TRAMP directly - it hangs. Instead, use async-start-process
  ;; which creates a truly non-blocking connection
  (wrds-debug-log 'info "Using async process creation to avoid TRAMP hanging")
  
  (condition-case err
      (let* ((host "wrds")  ; Default fallback host
             (process nil))
        
        ;; Try to parse TRAMP path, but have fallback
        (when (string-match "/sshx:\\([^|]+\\)" session-path)
          (setq host (match-string 1 session-path)))
        
        (wrds-debug-log 'debug "Using host: %s for SSH connection" host)
        
        ;; Create async process that connects to remote host
        (setq process 
              (start-process "sas-console-remote"
                            buffer
                            "ssh"
                            "-t"  ; force pseudo-tty allocation
                            host
                            "qrsh -now no 'echo Starting SAS...; qsas -nodms -rsasuser -noovp -nosyntaxcheck -nonews -nonotes -nodate -noaltlog -noaltprint -stdio'"))
        
        (when process
          (wrds-debug-log-process process "started" "async remote SAS session")
          (set-process-filter process 'sas-console-process-filter)
          (wrds-debug-log 'info "Started async isas session via SSH to %s" host)
          
          ;; Store process reference
          (puthash (buffer-name buffer) process sas-console-processes)
          
          ;; Add process sentinel to handle connection issues
          (set-process-sentinel process 
                               (lambda (proc event)
                                 (wrds-debug-log 'info "SAS process event: %s" (string-trim event))
                                 (when (string-match "\\(finished\\|exited\\)" event)
                                   (with-current-buffer (process-buffer proc)
                                     (insert "\n--- SAS session ended ---\n")))))
          
          process))
    (error
     (wrds-debug-log 'error "Failed to start async remote SAS: %s" err)
     ;; Insert error message in buffer for user feedback
     (with-current-buffer buffer
       (insert (format "ERROR: Failed to start remote SAS session: %s\n" err))
       (insert "Please check your WRDS SSH connection.\n")
       (insert "Try manually: ssh wrds 'qrsh -now no isas'\n"))
     nil)))

(defun sas-console-start-local-session (buffer)
  "Start a local SAS session in BUFFER."
  (wrds-debug-log 'info "Starting local SAS session")
  
  ;; For local sessions, try to start SAS directly
  ;; This will likely fail if SAS isn't installed locally, but we handle that
  (condition-case err
      (let ((process (start-process
                     "sas-console-local"
                     buffer
                     "sas" "-nodms" "-stdio")))
        (when process
          (wrds-debug-log-process process "started" "local SAS session")
          (set-process-filter process 'sas-console-process-filter)
          (puthash (buffer-name buffer) process sas-console-processes)
          process))
    (error
     (wrds-debug-log 'error "Failed to start local SAS: %s" err)
     ;; Insert an error message in the buffer
     (with-current-buffer buffer
       (insert "ERROR: Local SAS not available. Use remote session instead.\n"))
     nil)))

(defun sas-console-process-filter (process output)
  "Process filter for SAS console OUTPUT from PROCESS."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((moving (= (point) (process-mark process))))
          (save-excursion
            (goto-char (process-mark process))
            (insert output)
            (set-marker (process-mark process) (point)))
          (if moving (goto-char (process-mark process)))))
      
      ;; Log significant output
      (when (> (length output) 5)  ; Don't log every character
        (wrds-debug-log 'trace "SAS OUTPUT [%s]: %s" 
                        (buffer-name buffer)
                        (if (> (length output) 200)
                            (concat (substring output 0 200) "...")
                          output))))))

;;; Communication with SAS

(defun sas-console-wait-for-ready (buffer &optional timeout)
  "Wait for SAS to be ready in BUFFER.
TIMEOUT defaults to sas-console-connection-timeout."
  (let ((timeout (or timeout sas-console-connection-timeout))
        (start-time (current-time))
        (ready nil))
    
    (while (and (< (time-to-seconds (time-subtract (current-time) start-time)) timeout)
                (not ready))
      
      (with-current-buffer buffer
        (goto-char (point-max))
        
        ;; Look for SAS prompts or ready indicators
        (when (or (re-search-backward sas-console-prompt-regexp nil t)
                  (re-search-backward "\\(?:NOTE\\|Welcome\\|SAS\\)" nil t))
          (setq ready t)))
      
      (unless ready
        (accept-process-output nil 0.5)))
    
    (wrds-debug-log 'debug "SAS ready check: %s (waited %.1fs)" 
                    (if ready "SUCCESS" "TIMEOUT")
                    (time-to-seconds (time-subtract (current-time) start-time)))
    ready))

(defun sas-console-send-string (buffer code)
  "Send CODE to SAS console in BUFFER and return output."
  (wrds-debug-log-sas "send" code (buffer-name buffer))
  
  (unless (buffer-live-p buffer)
    (error "SAS console buffer is not live"))
  
  (let ((process (gethash (buffer-name buffer) sas-console-processes)))
    (unless (and process (process-live-p process))
      (error "SAS console process is not available"))
    
    (with-current-buffer buffer
      (let ((output-start (point-max-marker)))
        
        ;; Send the code
        (process-send-string process (concat code "\n"))
        (wrds-debug-log 'debug "Sent to SAS process: %s" code)
        
        ;; Wait for output (minimal wait since output is instant)
        (accept-process-output nil 0.1)
        
        ;; Extract output
        (goto-char (point-max))
        (let* ((output-end (point))
               (raw-output (buffer-substring-no-properties output-start output-end)))
          
          (wrds-debug-log 'debug "Raw SAS output: %s" raw-output)
          
          ;; Clean up and return output
          (sas-console-extract-output raw-output code))))))


(defun sas-console-extract-output (raw-output code)
  "Extract clean output from RAW-OUTPUT, removing CODE echo and prompts."
  (wrds-debug-log 'debug "Extracting SAS output from: %s" raw-output)
  
  (let ((output raw-output))
    ;; Remove the echoed command if present
    (setq output (replace-regexp-in-string 
                  (concat "^" (regexp-quote code) "\n*") "" output))
    
    ;; Remove SAS prompts
    (setq output (replace-regexp-in-string sas-console-prompt-regexp "" output))
    
    ;; Remove line numbers if present
    (setq output (replace-regexp-in-string "^\\s-*[0-9]+\\s-*" "" output))
    
    ;; Clean up whitespace
    (setq output (string-trim output))
    
    ;; If still empty, provide a default message
    (if (string-empty-p output)
        "SAS command executed"
      output)))

;;; Interactive Functions

(defun sas-console-open (&optional session file)
  "Open a SAS console for SESSION and FILE."
  (interactive)
  (let* ((session (or session 
                     (when current-prefix-arg
                       (read-string "Session (TRAMP path): " "/sshx:wrds|qrsh::/"))))
         (buffer (sas-console-get-or-create session file)))
    
    (if buffer
        (progn
          (switch-to-buffer buffer)
          (message "SAS console ready"))
      (error "Failed to create SAS console"))))

(defun sas-console-open-split (&optional session file)
  "Open a SAS console in a split window for SESSION and FILE."
  (interactive)
  (let* ((session (or session 
                     (when current-prefix-arg
                       (read-string "Session (TRAMP path): " "/sshx:wrds|qrsh::/"))))
         (buffer (sas-console-get-or-create session file))
         (current-window (selected-window)))
    
    (if buffer
        (progn
          ;; Split window to the right and switch to SAS console
          (split-window-right)
          (other-window 1)
          (switch-to-buffer buffer)
          ;; Return to original window
          (select-window current-window)
          (message "SAS console opened in split window"))
      (error "Failed to create SAS console"))))

(defun sas-console-open-for-org-src ()
  "Open SAS console for current org-src buffer."
  (interactive)
  (wrds-debug-log 'info "sas-console-open-for-org-src called (C-c ' workflow)")
  
  (if (bound-and-true-p org-src-mode)
      (let* ((session (sas-console-get-org-src-session))
             (file (or (bound-and-true-p org-src-source-file-name) (buffer-file-name))))
        
        (wrds-debug-log 'info "Opening SAS console for org-src: session=%s, file=%s" session file)
        (sas-console-open-split session file))
    (wrds-debug-log 'warn "sas-console-open-for-org-src called but not in org-src-mode")))

(defun sas-console-get-org-src-session ()
  "Extract session parameter from org-src buffer's source block."
  (when (bound-and-true-p org-src-mode)
    (wrds-debug-log 'debug "Detecting session in org-src-mode")
    (wrds-debug-log 'debug "org-src-source-file-name: %s" (bound-and-true-p org-src-source-file-name))
    (wrds-debug-log 'debug "org-src-beg-pos: %s" (bound-and-true-p org-src-beg-pos))
    
    ;; Try method 1: Use org-src--babel-info if available
    (or (ignore-errors
          (when (fboundp 'org-src--babel-info)
            (let ((info (org-src--babel-info)))
              (when info
                (let ((session (cdr (assoc :session (nth 2 info)))))
                  (when session
                    (wrds-debug-log 'debug "Found session via org-src--babel-info: %s" session)
                    session))))))
        
        ;; Try method 2: Use org-src--babel-info params
        (ignore-errors
          (when (and (bound-and-true-p org-src--babel-info)
                     (bound-and-true-p org-src-mode))
            (let ((params (nth 2 org-src--babel-info)))
              (when params
                (let ((session (cdr (assoc :session params))))
                  (when session
                    (wrds-debug-log 'debug "Found session via org-src--babel-info params: %s" session)
                    session))))))
        
        ;; Try method 3: Parse from source file and position  
        (let ((source-file (bound-and-true-p org-src-source-file-name))
              (beg-pos (bound-and-true-p org-src-beg-pos)))
          
          (when (and source-file beg-pos)
            (wrds-debug-log 'debug "Parsing session from source file: %s at pos %s" source-file beg-pos)
            (with-current-buffer (find-file-noselect source-file)
              (save-excursion
                (goto-char beg-pos)
                ;; Look for the #+begin_src line
                (when (re-search-backward "^\\s-*#\\+begin_src\\s-+sas" nil t)
                  (let ((line (buffer-substring-no-properties 
                              (line-beginning-position) 
                              (line-end-position))))
                    (wrds-debug-log 'debug "Found src line: %s" line)
                    ;; Extract :session parameter - improved regex to handle complex paths
                    (when (string-match ":session\\s-+\\(\\S-+\\)" line)
                      (let ((session (match-string 1 line)))
                        (wrds-debug-log 'debug "Extracted session parameter: %s" session)
                        session))))))))
        
        ;; Try method 4: Search current buffer for session parameter
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward ":session\\s-+\\(\\S-+\\)" nil t)
            (let ((session (match-string 1)))
              (wrds-debug-log 'debug "Found session in current buffer: %s" session)
              session)))
        
        ;; Method 5: Return nil if nothing found
        (progn
          (wrds-debug-log 'debug "No session found via any method")
          nil))))

(defun sas-console-send-region (start end)
  "Send region from START to END to the SAS console."
  (interactive "r")
  (let ((code (buffer-substring-no-properties start end))
        (session (when (string-match "/sshx:" default-directory) default-directory))
        (file (buffer-file-name)))
    
    (let ((buffer (sas-console-get-or-create session file)))
      (when buffer
        (sas-console-send-string buffer code)
        (message "Sent region to SAS console")))))

(defun sas-console-send-line ()
  "Send current line to SAS console."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (sas-console-send-region start end)))

(provide 'sas-console)

;;; sas-console.el ends here