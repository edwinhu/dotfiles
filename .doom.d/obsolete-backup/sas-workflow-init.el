;;; sas-workflow-init.el --- SAS workflow logging initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; This initializes comprehensive logging for the SAS workflow debugging.

;;; Code:

;;; SAS Workflow Logging Infrastructure
(defvar sas-workflow-debug-log-file (expand-file-name "sas-workflow-debug.log" "~/")
  "Log file for SAS workflow debugging information.")

(defun sas-workflow-debug-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to SAS workflow debug file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) sas-workflow-debug-log-file))))

(defun sas-workflow-clear-log ()
  "Clear the SAS workflow debug log file."
  (interactive)
  (when (file-exists-p sas-workflow-debug-log-file)
    (delete-file sas-workflow-debug-log-file))
  (sas-workflow-debug-log 'info "=== SAS Workflow Log Started ===")
  (message "SAS workflow log cleared and initialized"))

(defun sas-workflow-show-log ()
  "Display the SAS workflow debug log."
  (interactive)
  (if (file-exists-p sas-workflow-debug-log-file)
      (progn
        (find-file-other-window sas-workflow-debug-log-file)
        (goto-char (point-max))
        (message "SAS workflow log displayed"))
    (message "No SAS workflow log file found")))

(defun sas-workflow-test-logging ()
  "Test the logging system."
  (interactive)
  (sas-workflow-debug-log 'info "=== TESTING LOGGING SYSTEM ===")
  (sas-workflow-debug-log 'debug "This is a debug message")
  (sas-workflow-debug-log 'error "This is an error message")
  (sas-workflow-debug-log 'info "Logging test complete")
  (message "SAS workflow logging test complete - check log file"))

;; Initialize logging
(sas-workflow-clear-log)
(sas-workflow-debug-log 'info "SAS workflow logging system initialized")

;; Define key bindings
(global-set-key (kbd "C-c s l") #'sas-workflow-show-log)
(global-set-key (kbd "C-c s c") #'sas-workflow-clear-log)
(global-set-key (kbd "C-c s t") #'sas-workflow-test-logging)

(provide 'sas-workflow-init)
;;; sas-workflow-init.el ends here