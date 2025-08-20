;;; tramp-wrds.el --- WRDS interactive sessions via TRAMP -*- lexical-binding: t; -*-

;; Author: Generated for WRDS integration
;; Description: Interactive WRDS sessions using sshx and qrsh

;;; Commentary:
;; This file configures TRAMP to work with WRDS (Wharton Research Data Services)
;; for interactive computing sessions. It uses sshx (which works better with WRDS)
;; and provides functions to launch interactive shells and statistical software.
;;
;; Main functions:
;;   M-x tramp-wrds           - Open interactive shell on WRDS compute node
;;   M-x tramp-wrds-python    - Launch Python on WRDS
;;   M-x tramp-wrds-r         - Launch R on WRDS  
;;   M-x tramp-wrds-sas       - Launch SAS on WRDS
;;
;; Usage paths for org-babel:
;;   /sshx:wrds|qrsh::/path/to/file        - Connect to WRDS compute node via qrsh

;;; Code:

(require 'tramp)
(require 'comint)

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

;;; Interactive Session Functions

(defun tramp-wrds-simple (&optional queue)
  "Open a simple shell that connects to WRDS via ssh + qrsh.
This is the most reliable method that works regardless of terminal backend."
  (interactive)
  (let* ((buffer-name "*WRDS Simple*")
         (qrsh-command (cond
                        ((equal queue "highmem") "qrsh -q highmem.q")
                        ((equal queue "now") "qrsh -now yes -q interactive.q")
                        (t "qrsh -q interactive.q"))))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (let ((shell-buffer (shell buffer-name)))
        (with-current-buffer shell-buffer
          (comint-send-string shell-buffer (format "ssh wrds && %s\n" qrsh-command)))
        shell-buffer))))

(defun tramp-wrds-shell (&optional queue)
  "Open an interactive shell on WRDS compute node via qrsh.
QUEUE can be 'highmem', 'now', or nil for default interactive queue."
  (interactive)
  (let* ((method (cond
                  ((equal queue "highmem") "qrshmem")
                  ((equal queue "now") "qrshnow")
                  (t "qrsh")))
         (buffer-name (format "*WRDS Shell (%s)*" method))
         (qrsh-command (cond
                        ((equal queue "highmem") "qrsh -q highmem.q")
                        ((equal queue "now") "qrsh -now yes -q interactive.q")
                        (t "qrsh -q interactive.q"))))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (let ((shell-buffer (shell buffer-name)))
        (with-current-buffer shell-buffer
          (setq-local tramp-wrds-session-type method)
          ;; Send commands to connect to WRDS and start qrsh
          (comint-send-string shell-buffer "ssh wrds\n")
          ;; Wait a moment then send qrsh command
          (run-with-timer 2 nil
                          (lambda ()
                            (when (buffer-live-p shell-buffer)
                              (with-current-buffer shell-buffer
                                (comint-send-string shell-buffer (concat qrsh-command "\n")))))))
        shell-buffer))))

(defun tramp-wrds-vterm (&optional queue)
  "Open a vterm session on WRDS compute node via qrsh.
QUEUE can be 'highmem', 'now', or nil for default interactive queue."
  (interactive)
  (unless (fboundp 'vterm)
    (error "vterm is not available. Install vterm or use tramp-wrds-shell instead"))
  (let* ((method (cond
                  ((equal queue "highmem") "qrshmem")
                  ((equal queue "now") "qrshnow")
                  (t "qrsh")))
         (buffer-name (format "*WRDS VTerm (%s)*" method))
         (qrsh-command (cond
                        ((equal queue "highmem") "qrsh -q highmem.q")
                        ((equal queue "now") "qrsh -now yes -q interactive.q")
                        (t "qrsh -q interactive.q"))))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (let ((vterm-buffer (vterm buffer-name)))
        (with-current-buffer vterm-buffer
          (setq-local tramp-wrds-session-type method)
          ;; Send commands to connect to WRDS and start qrsh
          (vterm-send-string "ssh wrds")
          (vterm-send-return)
          ;; Wait a moment then send qrsh command
          (run-with-timer 2 nil
                          (lambda ()
                            (when (buffer-live-p vterm-buffer)
                              (with-current-buffer vterm-buffer
                                (vterm-send-string qrsh-command)
                                (vterm-send-return))))))
        vterm-buffer))))

(defun tramp-wrds-eat (&optional queue)
  "Open an eat terminal session on WRDS compute node via qrsh.
QUEUE can be 'highmem', 'now', or nil for default interactive queue."
  (interactive)
  (unless (fboundp 'eat)
    (error "eat is not available. Install eat or use tramp-wrds-shell instead"))
  (let* ((method (cond
                  ((equal queue "highmem") "qrshmem")
                  ((equal queue "now") "qrshnow")
                  (t "qrsh")))
         (buffer-name (format "*WRDS Eat (%s)*" method))
         (qrsh-command (cond
                        ((equal queue "highmem") "qrsh -q highmem.q")
                        ((equal queue "now") "qrsh -now yes -q interactive.q")
                        (t "qrsh -q interactive.q"))))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (let ((eat-buffer (eat buffer-name)))
        (with-current-buffer eat-buffer
          (setq-local tramp-wrds-session-type method)
          ;; Send commands to connect to WRDS and start qrsh
          (comint-send-string eat-buffer "ssh wrds\n")
          ;; Wait a moment then send qrsh command
          (run-with-timer 2 nil
                          (lambda ()
                            (when (buffer-live-p eat-buffer)
                              (with-current-buffer eat-buffer
                                (comint-send-string eat-buffer (concat qrsh-command "\n")))))))
        eat-buffer))))

;;; Main Interactive Function

(defun tramp-wrds (&optional queue terminal-type)
  "Open an interactive session on WRDS compute node.
QUEUE can be 'highmem', 'now', or nil for default interactive queue.
TERMINAL-TYPE can be 'vterm', 'eat', or 'shell' (default based on +term-backend)."
  (interactive 
   (list 
    (when current-prefix-arg
      (completing-read "Queue: " '("interactive" "highmem" "now") nil t))
    (when (> (prefix-numeric-value current-prefix-arg) 4)
      (completing-read "Terminal: " '("vterm" "eat" "shell") nil t))))
  
  (let ((term-type (or terminal-type
                      (cond
                       ((and (eq +term-backend 'vterm) (fboundp 'vterm)) "vterm")
                       ((and (eq +term-backend 'eat) (fboundp 'eat)) "eat")
                       (t "shell")))))
    (condition-case err
        (cond
         ((string= term-type "vterm") (tramp-wrds-vterm queue))
         ((string= term-type "eat") (tramp-wrds-eat queue))
         (t (tramp-wrds-shell queue)))
      (error 
       (message "Terminal %s failed (%s), falling back to simple shell" term-type err)
       (tramp-wrds-simple queue)))))

;;; Statistical Software Launch Functions

(defun tramp-wrds-python (&optional queue)
  "Launch Python interactive session on WRDS compute node.
QUEUE can be 'highmem', 'now', or nil for default interactive queue."
  (interactive)
  (let ((buffer (tramp-wrds queue)))
    (with-current-buffer buffer
      (comint-send-string buffer "ipython3\n"))
    (switch-to-buffer buffer)))

(defun tramp-wrds-r (&optional queue)
  "Launch R interactive session on WRDS compute node.
QUEUE can be 'highmem', 'now', or nil for default interactive queue."
  (interactive)
  (let ((buffer (tramp-wrds queue)))
    (with-current-buffer buffer
      (comint-send-string buffer "R\n"))
    (switch-to-buffer buffer)))

(defun tramp-wrds-sas (&optional queue)
  "Launch SAS interactive session on WRDS compute node.
QUEUE can be 'highmem', 'now', or nil for default interactive queue."
  (interactive)
  (require 'wrds-debug)
  (require 'sas-console)
  
  (wrds-debug-log 'info "Starting WRDS SAS session with queue: %s" queue)
  
  ;; Determine the TRAMP path based on queue
  (let* ((method (cond
                  ((equal queue "highmem") "qrshmem")
                  ((equal queue "now") "qrshnow")
                  (t "qrsh")))
         (session-path (format "/sshx:wrds|%s::/" method)))
    
    (wrds-debug-log 'info "Using session path: %s" session-path)
    
    ;; Use sas-console to create the session
    (let ((buffer (sas-console-get-or-create session-path)))
      (if buffer
          (progn
            (switch-to-buffer buffer)
            (message "SAS session started on WRDS"))
        (error "Failed to start SAS session on WRDS")))))

(defun tramp-wrds-stata (&optional queue)
  "Launch Stata interactive session on WRDS compute node.
QUEUE can be 'highmem', 'now', or nil for default interactive queue."
  (interactive)
  (let ((buffer (tramp-wrds queue)))
    (with-current-buffer buffer
      (comint-send-string buffer "stata\n"))
    (switch-to-buffer buffer)))

;;; File Operations Helper Functions

(defun wrds-find-file (filename &optional queue)
  "Open a file on WRDS compute node via qrsh.
FILENAME is the path to the file on WRDS.
QUEUE can be 'highmem', 'now', or nil for default interactive queue."
  (interactive "FFile on WRDS: ")
  (let* ((method (cond
                  ((equal queue "highmem") "qrshmem")
                  ((equal queue "now") "qrshnow")
                  (t "qrsh")))
         (tramp-path (format "/sshx:wrds|%s::%s" method filename)))
    (find-file tramp-path)))

(defun wrds-dired (&optional queue)
  "Open dired on WRDS compute node via qrsh.
QUEUE can be 'highmem', 'now', or nil for default interactive queue."
  (interactive)
  (let* ((method (cond
                  ((equal queue "highmem") "qrshmem")
                  ((equal queue "now") "qrshnow")
                  (t "qrsh")))
         (tramp-path (format "/sshx:wrds|%s::/" method)))
    (dired tramp-path)))

;;; Utility Functions

(defun tramp-wrds-cleanup ()
  "Clean up WRDS TRAMP connections."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (and (buffer-name buffer)
               (string-match-p "\\*WRDS.*\\*" (buffer-name buffer)))
      (kill-buffer buffer)))
  (tramp-cleanup-all-connections)
  (message "WRDS TRAMP connections cleaned up"))

(defun tramp-wrds-status ()
  "Show status of WRDS connections."
  (interactive)
  (let ((wrds-buffers (seq-filter 
                       (lambda (buf) 
                         (string-match-p "\\*WRDS.*\\*" (buffer-name buf)))
                       (buffer-list))))
    (if wrds-buffers
        (message "Active WRDS sessions: %s" 
                 (mapconcat #'buffer-name wrds-buffers ", "))
      (message "No active WRDS sessions"))))

;;; Keybindings
;; Uncomment and modify as desired
;; (global-set-key (kbd "C-c w w") #'tramp-wrds)
;; (global-set-key (kbd "C-c w p") #'tramp-wrds-python)
;; (global-set-key (kbd "C-c w r") #'tramp-wrds-r)
;; (global-set-key (kbd "C-c w s") #'tramp-wrds-sas)
;; (global-set-key (kbd "C-c w f") #'wrds-find-file)
;; (global-set-key (kbd "C-c w d") #'wrds-dired)
;; (global-set-key (kbd "C-c w c") #'tramp-wrds-cleanup)

(provide 'tramp-wrds)

;;; tramp-wrds.el ends here