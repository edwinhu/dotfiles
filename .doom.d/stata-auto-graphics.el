;;; stata-auto-graphics.el --- Automatic Stata graphics display using chafa -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple, reliable automatic Stata graphics display system
;; Hooks into termint command execution and automatically displays PNG files using chafa
;; Based on user's proven working manual approach: ! chafa "/Users/vwh7mb/.stata_kernel_cache/graph0.png"

;;; Code:

(require 'termint nil t)

(defgroup stata-auto-graphics nil
  "Automatic Stata graphics display using chafa."
  :group 'org)

(defcustom stata-auto-graphics-cache-dir (expand-file-name "~/.stata_kernel_cache")
  "Directory where stata_kernel saves PNG graphics files."
  :type 'directory
  :group 'stata-auto-graphics)

(defvar stata-auto-graphics-debug-log-file (expand-file-name "stata-auto-graphics.log" "~/")
  "Log file for automatic Stata graphics debugging.")

(defvar stata-auto-graphics-last-file nil
  "Last PNG file that was automatically displayed.")

(defvar stata-auto-graphics-monitor-timer nil
  "Timer for monitoring PNG file creation.")

;;; Debugging and Logging

(defun stata-auto-graphics-debug-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to debug file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) stata-auto-graphics-debug-log-file))))

(defun stata-auto-graphics-clear-log ()
  "Clear the debug log file."
  (interactive)
  (when (file-exists-p stata-auto-graphics-debug-log-file)
    (delete-file stata-auto-graphics-debug-log-file))
  (stata-auto-graphics-debug-log 'info "=== Stata Auto Graphics System Started ==="))

;;; Graphics File Detection

(defun stata-auto-graphics-get-latest-png ()
  "Get the most recently created PNG file in the Stata cache directory."
  (when (file-directory-p stata-auto-graphics-cache-dir)
    (let ((png-files (directory-files stata-auto-graphics-cache-dir t "\\.png$")))
      (when png-files
        (car (sort png-files (lambda (a b)
                              (time-less-p (nth 5 (file-attributes b))
                                          (nth 5 (file-attributes a))))))))))

(defun stata-auto-graphics-new-png-created-p ()
  "Check if a new PNG file has been created since last check."
  (let ((latest-png (stata-auto-graphics-get-latest-png)))
    (when (and latest-png
               (not (string= latest-png stata-auto-graphics-last-file)))
      (stata-auto-graphics-debug-log 'info "New PNG detected: %s" latest-png)
      latest-png)))

;;; Automatic Graphics Display

(defun stata-auto-graphics-display-png (png-file)
  "Automatically display PNG-FILE in the euporie-stata console using chafa.
This replicates the user's working manual approach."
  (when (and png-file 
             (file-exists-p png-file)
             (get-buffer "*euporie-stata*"))
    
    (stata-auto-graphics-debug-log 'info "Auto-displaying PNG: %s" png-file)
    
    ;; Use the exact same chafa command that works manually for the user
    (let ((chafa-command (format "! chafa \"%s\"" png-file)))
      (stata-auto-graphics-debug-log 'info "Injecting chafa command: %s" chafa-command)
      
      ;; Send the chafa command to the euporie-stata console
      (condition-case err
          (progn
            (termint-euporie-stata-send-string chafa-command)
            (setq stata-auto-graphics-last-file png-file)
            (stata-auto-graphics-debug-log 'info "✓ Successfully displayed PNG via chafa"))
        (error
         (stata-auto-graphics-debug-log 'error "✗ Failed to send chafa command: %s" err))))))

;;; Command Execution Monitoring

(defvar stata-auto-graphics-graphics-commands
  '("histogram" "scatter" "twoway" "graph" "plot" "bar" "box" "line" "area")
  "Stata commands that typically generate graphics.")

(defun stata-auto-graphics-is-graphics-command-p (command)
  "Check if COMMAND is a graphics command that might generate a PNG."
  (let ((first-word (car (split-string (string-trim command)))))
    (member first-word stata-auto-graphics-graphics-commands)))

(defun stata-auto-graphics-start-monitoring ()
  "Start monitoring for new PNG files after a graphics command."
  (stata-auto-graphics-debug-log 'info "Starting PNG monitoring...")
  
  ;; Cancel any existing timer
  (when (and stata-auto-graphics-monitor-timer 
             (timerp stata-auto-graphics-monitor-timer))
    (cancel-timer stata-auto-graphics-monitor-timer))
  
  ;; Start a timer that checks for new PNGs for the next 10 seconds
  (setq stata-auto-graphics-monitor-timer
        (run-with-timer 0.5 0.5 'stata-auto-graphics-check-for-new-png 10)))

(defun stata-auto-graphics-check-for-new-png (remaining-checks)
  "Check for new PNG files. Stop after REMAINING-CHECKS attempts."
  (stata-auto-graphics-debug-log 'debug "Checking for new PNG (attempts left: %d)" remaining-checks)
  
  (let ((new-png (stata-auto-graphics-new-png-created-p)))
    (cond
     ;; Found a new PNG - display it and stop monitoring
     (new-png
      (stata-auto-graphics-display-png new-png)
      (when (timerp stata-auto-graphics-monitor-timer)
        (cancel-timer stata-auto-graphics-monitor-timer))
      (setq stata-auto-graphics-monitor-timer nil))
     
     ;; No more checks left - stop monitoring
     (<= remaining-checks 1)
     (progn
       (stata-auto-graphics-debug-log 'debug "PNG monitoring timeout - no new graphics found")
       (when (timerp stata-auto-graphics-monitor-timer)
         (cancel-timer stata-auto-graphics-monitor-timer))
       (setq stata-auto-graphics-monitor-timer nil))
     
     ;; Continue monitoring
     (t
      (setq stata-auto-graphics-monitor-timer
            (run-with-timer 0.5 0.5 'stata-auto-graphics-check-for-new-png (1- remaining-checks)))))))

;;; Termint Integration Hook

(defvar stata-auto-graphics-original-send-function nil
  "Original termint send function before our hook.")

(defun stata-auto-graphics-hook-send-function ()
  "Hook into the termint send function to monitor graphics commands."
  (when (fboundp 'termint-euporie-stata-send-string)
    ;; Save the original function
    (unless stata-auto-graphics-original-send-function
      (setq stata-auto-graphics-original-send-function 
            (symbol-function 'termint-euporie-stata-send-string)))
    
    ;; Create our wrapper function
    (defun termint-euporie-stata-send-string (command)
      "Wrapper for termint send that monitors for graphics commands."
      (stata-auto-graphics-debug-log 'info "Stata command sent: %s" command)
      
      ;; Call the original function first
      (funcall stata-auto-graphics-original-send-function command)
      
      ;; If this looks like a graphics command, start monitoring
      (when (stata-auto-graphics-is-graphics-command-p command)
        (stata-auto-graphics-debug-log 'info "Graphics command detected, starting monitoring...")
        (run-with-timer 1.0 nil 'stata-auto-graphics-start-monitoring)))
    
    (stata-auto-graphics-debug-log 'info "Successfully hooked termint-euporie-stata-send-string")))

;;; Manual Testing Functions

(defun stata-auto-graphics-test-chafa ()
  "Test if chafa works with the latest PNG file."
  (interactive)
  (let ((latest-png (stata-auto-graphics-get-latest-png)))
    (if latest-png
        (progn
          (message "Testing chafa with: %s" latest-png)
          (stata-auto-graphics-display-png latest-png))
      (message "No PNG files found in %s" stata-auto-graphics-cache-dir))))

(defun stata-auto-graphics-test-monitoring ()
  "Test the PNG monitoring system."
  (interactive)
  (stata-auto-graphics-debug-log 'info "=== MANUAL MONITORING TEST ===")
  (message "Starting PNG monitoring test - run a Stata graphics command now")
  (stata-auto-graphics-start-monitoring))

;;; System Status

(defun stata-auto-graphics-status ()
  "Show the current status of the automatic graphics system."
  (interactive)
  (let ((euporie-buffer-exists (get-buffer "*euporie-stata*"))
        (cache-dir-exists (file-directory-p stata-auto-graphics-cache-dir))
        (latest-png (stata-auto-graphics-get-latest-png))
        (hook-installed (not (eq (symbol-function 'termint-euporie-stata-send-string)
                                stata-auto-graphics-original-send-function))))
    
    (message "=== Stata Auto Graphics Status ===")
    (message "Euporie Stata buffer: %s" (if euporie-buffer-exists "✓ Present" "✗ Missing"))
    (message "Cache directory: %s" (if cache-dir-exists "✓ Present" "✗ Missing"))
    (message "Latest PNG: %s" (if latest-png (file-name-nondirectory latest-png) "None found"))
    (message "Hook installed: %s" (if hook-installed "✓ Active" "✗ Inactive"))
    (message "Monitor running: %s" (if (timerp stata-auto-graphics-monitor-timer) "✓ Active" "✗ Inactive"))
    (message "===========================")))

;;; Initialization

(defun stata-auto-graphics-setup ()
  "Set up the automatic Stata graphics system."
  (interactive)
  (stata-auto-graphics-clear-log)
  (stata-auto-graphics-debug-log 'info "Setting up automatic Stata graphics system...")
  
  ;; Ensure cache directory exists
  (unless (file-directory-p stata-auto-graphics-cache-dir)
    (stata-auto-graphics-debug-log 'warn "Cache directory does not exist: %s" stata-auto-graphics-cache-dir))
  
  ;; Hook into termint send function
  (stata-auto-graphics-hook-send-function)
  
  (stata-auto-graphics-debug-log 'info "✓ Automatic Stata graphics system ready")
  (message "Stata auto-graphics system initialized"))

;;; Auto-initialization when termint is loaded

(with-eval-after-load 'termint
  (stata-auto-graphics-setup))

;; Also try to set up immediately if termint is already loaded
(when (featurep 'termint)
  (stata-auto-graphics-setup))

(provide 'stata-auto-graphics)
;;; stata-auto-graphics.el ends here