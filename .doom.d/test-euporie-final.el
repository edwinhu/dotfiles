;;; test-euporie-final.el --- Final comprehensive test suite -*- lexical-binding: t; -*-

(require 'ert)
(require 'euporie-termint nil t)

(defvar euporie-test-log (expand-file-name "euporie-final-test.log" "~/"))

(defun euporie-test-log-msg (level msg)
  "Log test message."
  (with-temp-buffer
    (insert (format "[%s] %s\n" level msg))
    (append-to-file (point-min) (point-max) euporie-test-log)))

(defun euporie-test-cleanup ()
  "Clean up test environment."
  (dolist (buf '("*euporie-stata*" "*euporie-python*" "*euporie-r*"))
    (when (get-buffer buf)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(defun euporie-test-check-clean (output desc)
  "Check if output is clean of counter messages."
  (let ((clean (not (and output 
                        (stringp output)
                        (string-match-p "stata_kernel_graph_counter" output)))))
    (euporie-test-log-msg (if clean "INFO" "ERROR") 
                         (format "%s: %s" desc (if clean "CLEAN" "POLLUTED")))
    clean))

(ert-deftest euporie-final/stata-scatter-clean ()
  "Test Stata scatter plot produces clean output."
  (when (file-exists-p euporie-test-log) (delete-file euporie-test-log))
  (euporie-test-log-msg "INFO" "Starting final Stata test")
  (euporie-test-cleanup)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (sleep-for 15)
        (should (get-buffer "*euporie-stata*"))
        
        (with-current-buffer "*euporie-stata*"
          (let ((proc (get-buffer-process (current-buffer))))
            (when proc
              (let ((start-pos (point-max)))
                (process-send-string proc "sysuse auto\n")
                (sleep-for 5)
                
                (let ((plot-start (point-max)))
                  (process-send-string proc "scatter price mpg\n")
                  (sleep-for 12)
                  
                  (let ((output (buffer-substring-no-properties plot-start (point-max))))
                    (should (euporie-test-check-clean output "Scatter plot"))
                    (should-not (string-match-p "global.*stata_kernel_graph_counter" (or output ""))))))))))
    
    (euporie-test-cleanup)))

(defun euporie-run-final-test ()
  "Run the final validation test."
  (interactive)
  (ert-run-tests-interactively "euporie-final/stata-scatter-clean"))

(provide 'test-euporie-final)
;;; test-euporie-final.el ends here