;;; test-euporie-ux-minimal.el --- Minimal UX validation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal user experience tests focusing on the core issue: counter message pollution

;;; Code:

(require 'ert)
(require 'euporie-termint nil t)

(defvar euporie-ux-log-file (expand-file-name "euporie-ux-minimal.log" "~/"))

(defun euporie-ux-log (level format-string &rest args)
  "Log UX test message."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-ux-log-file))))

(defun euporie-ux-cleanup ()
  "Clean up test buffers."
  (dolist (buffer-name '("*euporie-stata*" "*euporie-python*" "*euporie-r*"))
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))))

(defun euporie-ux-check-counter-messages (output description)
  "Check for unwanted counter messages in output."
  (let ((has-counters (and output (stringp output)
                          (or (string-match-p "global stata_kernel_graph_counter" output)
                              (string-match-p "stata_kernel_graph_counter.*=" output)))))
    (if has-counters
        (progn
          (euporie-ux-log 'error "%s: FAILED - Counter messages found" description)
          (euporie-ux-log 'error "Output: %S" output)
          nil)
      (progn
        (euporie-ux-log 'info "%s: PASSED - No counter messages" description)
        t))))

(ert-deftest euporie-ux/counter-message-validation ()
  "Test that scatter plot produces no counter messages."
  (euporie-ux-cleanup)
  (when (file-exists-p euporie-ux-log-file) (delete-file euporie-ux-log-file))
  (euporie-ux-log 'info "Starting counter message validation test")
  
  (unwind-protect
      (progn
        ;; Start euporie
        (euporie-stata-start)
        (sleep-for 15)
        (should (get-buffer "*euporie-stata*"))
        
        ;; Test scatter plot
        (with-current-buffer "*euporie-stata*"
          (let ((process (get-buffer-process (current-buffer))))
            (when process
              ;; Load data
              (let ((data-start (point-max)))
                (process-send-string process "sysuse auto\n")
                (sleep-for 5)
                (let ((data-output (buffer-substring-no-properties data-start (point-max))))
                  (should (euporie-ux-check-counter-messages data-output "Data loading"))))
              
              ;; Create plot
              (let ((plot-start (point-max)))
                (process-send-string process "scatter price mpg\n")
                (sleep-for 10)
                (let ((plot-output (buffer-substring-no-properties plot-start (point-max))))
                  (should (euporie-ux-check-counter-messages plot-output "Scatter plot"))
                  (should-not (string-match-p "global stata_kernel_graph_counter" (or plot-output ""))))))))
    
    (euporie-ux-cleanup)))

(defun euporie-ux-run-minimal-test ()
  "Run minimal UX validation test."
  (interactive)
  (ert-run-tests-interactively "euporie-ux/counter-message-validation"))

(provide 'test-euporie-ux-minimal)
;;; test-euporie-ux-minimal.el ends here