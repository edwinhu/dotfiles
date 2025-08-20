;;; wrds-debug.el --- Simple debugging utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal debugging utilities for WRDS integration

;;; Code:

(defvar wrds-debug-log-file "~/wrds-debug.log"
  "Path to debug log file.")

(defun wrds-debug-log (level format-string &rest args)
  "Log a message with LEVEL to the debug file.
FORMAT-STRING and ARGS are passed to \`format'."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) wrds-debug-log-file))))

(defun wrds-debug-log-process (process status description)
  "Log process information."
  (wrds-debug-log 'info "Process %s %s: %s" description status process))

(defun wrds-debug-log-sas (action code buffer-name)
  "Log SAS action."
  (wrds-debug-log 'debug "SAS %s [%s]: %s" action buffer-name code))

(defun wrds-debug-monitor-buffer (buffer-name)
  "Start monitoring buffer."
  (wrds-debug-log 'debug "Monitoring buffer: %s" buffer-name))

(provide 'wrds-debug)
;;; wrds-debug.el ends here
