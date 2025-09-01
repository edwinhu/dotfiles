;;; tramp-qrsh.el --- Simplified qrsh compute node sessions -*- lexical-binding: t; -*-

;; Author: Generated for streamlined euporie integration  
;; Description: Simplified qrsh compute node sessions using termint and eat

;;; Commentary:
;; STREAMLINED MODULE: Focused solely on establishing qrsh compute node sessions.
;; Statistical software launching is handled by euporie-termint.el.
;;
;; SINGLE PURPOSE: Handle qrsh compute node sessions only
;;
;; MAIN FUNCTION:
;;   tramp-qrsh-session(&optional queue) - returns ready buffer
;;
;; CLEAN INTERFACE: Just establish connection, return to caller
;;
;; Usage:
;;   (let ((buffer (tramp-qrsh-session)))
;;     (when buffer 
;;       ;; Send commands using termint functions
;;       (termint-qrsh-session-send-string "your-command")))

;;; Code:

(require 'tramp)
(require 'termint nil t)

;; Define the qrsh method for TRAMP (uses sshx as base)
(add-to-list 'tramp-methods
  '("qrsh"
    ;; Use qrsh as the login program
    (tramp-login-program "qrsh")
    ;; Arguments for qrsh - use interactive queue by default
    (tramp-login-args
     (("-q" "interactive.q")  ; Use interactive queue
      ("%h")))                ; Optional host specification
    ;; Remote shell configuration
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-login ("-l"))
    (tramp-remote-shell-args ("-c"))
    ;; Connection parameters
    (tramp-connection-timeout 60)        ; Allow time for queue allocation
    (tramp-session-timeout 43200)       ; 12 hours (WRDS limit)
    (tramp-password-previous-hop t)))    ; Use SSH credentials from previous hop

;; Define high-memory queue variant
(add-to-list 'tramp-methods
  '("qrshmem"
    (tramp-login-program "qrsh")
    (tramp-login-args
     (("-q" "highmem.q")      ; Use high-memory queue
      ("%h")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-login ("-l"))
    (tramp-remote-shell-args ("-c"))
    (tramp-connection-timeout 60)
    (tramp-session-timeout 43200)
    (tramp-password-previous-hop t)))

;; Define immediate execution variant
(add-to-list 'tramp-methods
  '("qrshnow"
    (tramp-login-program "qrsh")
    (tramp-login-args
     (("-now" "yes")          ; Execute immediately if resources available
      ("-q" "interactive.q")
      ("%h")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-login ("-l"))
    (tramp-remote-shell-args ("-c"))
    (tramp-connection-timeout 30)        ; Shorter timeout for immediate execution
    (tramp-session-timeout 43200)
    (tramp-password-previous-hop t)))

;; Configure TRAMP for better WRDS compatibility
(with-eval-after-load 'tramp
  ;; Increase default timeouts for queue-based systems
  (setq tramp-completion-reread-directory-timeout 300)
  
  ;; Cache connection properties for better performance
  (setq tramp-persistency-file-name
        (expand-file-name "tramp" (or (bound-and-true-p doom-cache-dir)
                                     "~/.emacs.d/.cache/")))
  
  ;; Enable verbose logging for troubleshooting if needed
  ;; (setq tramp-verbose 6)
  )

;;; Logging Infrastructure

(defvar tramp-qrsh-debug-log-file (expand-file-name "tramp-qrsh-debug.log" "~/")
  "Log file for tramp-qrsh debugging information.")

(defun tramp-qrsh-debug-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to debug file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) tramp-qrsh-debug-log-file))))

;;; Main Session Function

(defun tramp-qrsh-session (&optional queue)
  "Open a termint+eat session on WRDS compute node via qrsh.
QUEUE can be 'highmem', 'now', or nil for default interactive queue.
Returns buffer ready for command sending."
  (interactive (list (when current-prefix-arg
                      (completing-read "Queue: " '("interactive" "highmem" "now") nil t))))
  (unless (fboundp 'termint-define)
    (error "termint is not available. Install termint package"))
  
  (tramp-qrsh-debug-log 'info "Starting qrsh session with queue: %s" (or queue "default"))
  
  (let* ((method (cond
                  ((equal queue "highmem") "qrshmem")
                  ((equal queue "now") "qrshnow")
                  (t "qrsh")))
         (session-name (format "qrsh-%s" method))
         (buffer-name (format "*%s*" session-name))
         (qrsh-command (cond
                        ((equal queue "highmem") "qrsh -q highmem.q")
                        ((equal queue "now") "qrsh -now yes -q interactive.q")
                        (t "qrsh -q interactive.q")))
         (full-command (format "ssh -t -q wrds %s" qrsh-command)))
    
    ;; Kill existing buffer if it exists
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))
    
    (tramp-qrsh-debug-log 'info "Using method %s with command: %s" method full-command)
    
    ;; Define termint session with eat backend using literal names
    (cond
     ((string= method "qrsh")
      (termint-define "qrsh-session" full-command
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "eat-truecolor")
                             ("COLORTERM" . "truecolor")))
      (condition-case err
          (progn
            (tramp-qrsh-debug-log 'info "Starting qrsh session...")
            (termint-qrsh-session-start)
            (sleep-for 3)  ; Allow extra time for connection
            (let ((buffer (get-buffer "*qrsh-session*")))
              (if buffer
                  (progn
                    (tramp-qrsh-debug-log 'info "Successfully created qrsh session buffer")
                    buffer)
                (progn
                  (tramp-qrsh-debug-log 'error "Failed to create qrsh session buffer")
                  nil))))
        (error 
         (tramp-qrsh-debug-log 'error "Failed to start qrsh session: %s" err)
         (message "Failed to start qrsh session: %s" err)
         nil)))
     
     ((string= method "qrshmem")
      (termint-define "qrsh-highmem" full-command
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "eat-truecolor")
                             ("COLORTERM" . "truecolor")))
      (condition-case err
          (progn
            (tramp-qrsh-debug-log 'info "Starting qrsh highmem session...")
            (termint-qrsh-highmem-start)
            (sleep-for 3)  ; Allow extra time for queue allocation
            (let ((buffer (get-buffer "*qrsh-highmem*")))
              (if buffer
                  (progn
                    (tramp-qrsh-debug-log 'info "Successfully created qrsh highmem session buffer")
                    buffer)
                (progn
                  (tramp-qrsh-debug-log 'error "Failed to create qrsh highmem session buffer")
                  nil))))
        (error 
         (tramp-qrsh-debug-log 'error "Failed to start qrsh highmem session: %s" err)
         (message "Failed to start qrsh highmem session: %s" err)
         nil)))
     
     ((string= method "qrshnow")
      (termint-define "qrsh-now" full-command
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "eat-truecolor")
                             ("COLORTERM" . "truecolor")))
      (condition-case err
          (progn
            (tramp-qrsh-debug-log 'info "Starting qrsh now session...")
            (termint-qrsh-now-start)
            (sleep-for 2)  ; Shorter wait for immediate execution
            (let ((buffer (get-buffer "*qrsh-now*")))
              (if buffer
                  (progn
                    (tramp-qrsh-debug-log 'info "Successfully created qrsh now session buffer")
                    buffer)
                (progn
                  (tramp-qrsh-debug-log 'error "Failed to create qrsh now session buffer")
                  nil))))
        (error 
         (tramp-qrsh-debug-log 'error "Failed to start qrsh now session: %s" err)
         (message "Failed to start qrsh now session: %s" err)
         nil)))
     
     (t 
      (tramp-qrsh-debug-log 'error "Unknown qrsh method: %s" method)
      (error "Unknown qrsh method: %s" method)))))

;;; String Sending Function

(defun termint-qrsh-session-send-string (string)
  "Send STRING to the active qrsh session buffer.
Automatically detects which qrsh buffer is active and sends the string
using the appropriate termint method with bracketed paste support."
  (let ((active-buffer nil))
    ;; Find the active qrsh buffer (check in order of preference)
    (cond
     ((get-buffer "*qrsh-session*")
      (setq active-buffer (get-buffer "*qrsh-session*")))
     ((get-buffer "*qrsh-highmem*")
      (setq active-buffer (get-buffer "*qrsh-highmem*")))
     ((get-buffer "*qrsh-now*")
      (setq active-buffer (get-buffer "*qrsh-now*")))
     (t
      (error "No active qrsh session buffer found")))
    
    (unless active-buffer
      (error "Could not find active qrsh session buffer"))
    
    (tramp-qrsh-debug-log 'info "Sending string to buffer %s: %s" 
                          (buffer-name active-buffer)
                          (substring string 0 (min 50 (length string))))
    
    ;; Send string to the active buffer with bracketed paste
    (with-current-buffer active-buffer
      (let ((process (get-buffer-process (current-buffer))))
        (unless process
          (error "No process found in qrsh buffer %s" (buffer-name active-buffer)))
        
        ;; Check if process is running
        (unless (eq (process-status process) 'run)
          (error "qrsh process is not running in buffer %s" (buffer-name active-buffer)))
        
        ;; Send with bracketed paste support
        (tramp-qrsh-debug-log 'debug "Sending bracketed paste start sequence")
        (process-send-string process "\e[200~")  ; Begin bracketed paste
        
        (tramp-qrsh-debug-log 'debug "Sending actual string content")
        (process-send-string process string)
        
        (tramp-qrsh-debug-log 'debug "Sending bracketed paste end sequence")
        (process-send-string process "\e[201~")  ; End bracketed paste
        
        ;; Send newline to execute
        (process-send-string process "\n")
        
        (tramp-qrsh-debug-log 'info "Successfully sent string to qrsh session")
        t))))

;;; Utility Functions

(defun tramp-qrsh-cleanup ()
  "Clean up qrsh TRAMP connections."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (and (buffer-name buffer)
               (string-match-p "\\*qrsh.*\\*" (buffer-name buffer)))
      (kill-buffer buffer)))
  (tramp-cleanup-all-connections)
  (message "qrsh TRAMP connections cleaned up"))

(defun tramp-qrsh-status ()
  "Show status of qrsh connections."
  (interactive)
  (let ((qrsh-buffers (seq-filter 
                       (lambda (buf) 
                         (string-match-p "\\*qrsh.*\\*" (buffer-name buf)))
                       (buffer-list))))
    (if qrsh-buffers
        (message "Active qrsh sessions: %s" 
                 (mapconcat #'buffer-name qrsh-buffers ", "))
      (message "No active qrsh sessions"))))

(provide 'tramp-qrsh)

;;; tramp-qrsh.el ends here
