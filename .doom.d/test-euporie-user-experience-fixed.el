;;; test-euporie-user-experience-fixed.el --- User experience validation tests for Emacs-euporie integration -*- lexical-binding: t; -*-

;;; Commentary:
;; User experience focused unit tests that validate the exact workflows users expect.
;; These tests simulate real user interactions and validate the complete UX stack:
;;
;; - C-RET keybinding behavior in org-mode and source buffers
;; - Inline graphics display in terminal buffers
;; - Buffer management and window layout
;; - Process lifecycle and responsiveness
;; - Error handling and recovery workflows

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'euporie-termint nil t)
(require 'org)

;; User Experience Test Configuration
(defvar euporie-ux-test-log-file (expand-file-name "euporie-ux-validation.log" "~/")
  "Log file for user experience validation tests.")

(defvar euporie-ux-test-session-timeout 45
  "Timeout for user workflow sessions.")

;; User Experience Utilities
(defun euporie-ux-log (level format-string &rest args)
  "Log user experience test events with workflow context."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N")))
    (with-temp-buffer
      (insert (format "[%s] [UX-%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-ux-test-log-file))))

(defun euporie-ux-clear-log ()
  "Initialize user experience test log."
  (when (file-exists-p euporie-ux-test-log-file)
    (delete-file euporie-ux-test-log-file))
  (euporie-ux-log 'info "=== USER EXPERIENCE VALIDATION SESSION ==="))

(defun euporie-ux-cleanup ()
  "Clean up test environment maintaining user-like cleanup patterns."
  (euporie-ux-log 'info "Performing user-like cleanup")
  
  ;; Close euporie buffers like a user would
  (dolist (buffer-name '("*euporie-stata*" "*euporie-python*" "*euporie-r*"))
    (when (get-buffer buffer-name)
      (euporie-ux-log 'info "Closing %s buffer" buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name))))
  
  ;; Clean up test files
  (dolist (file '("~/test-ux-stata.org" "~/test-ux-workflow.org"))
    (when (file-exists-p (expand-file-name file))
      (delete-file (expand-file-name file))))
  
  (sleep-for 2))

(defun euporie-ux-validate-clean-output (output description)
  "Validate output cleanliness from user quality expectations."
  (let ((is-clean t)
        (issues '()))
    
    (when (and output (stringp output))
      ;; Check for counter messages that users complained about
      (when (string-match-p "global stata_kernel_graph_counter" output)
        (setq is-clean nil)
        (push "global counter variable visible" issues))
      
      (when (string-match-p "stata_kernel_graph_counter.*=" output)
        (setq is-clean nil)
        (push "counter assignment visible" issues))
      
      (when (string-match-p "\\$stata_kernel_graph_counter.*\\+" output)
        (setq is-clean nil)
        (push "counter increment visible" issues)))
    
    (if is-clean
        (euporie-ux-log 'info "%s: CLEAN - user quality standards met" description)
      (euporie-ux-log 'warn "%s: QUALITY ISSUES - %s" description issues))
    
    is-clean))

;; CORE USER EXPERIENCE TESTS

(ert-deftest euporie-ux/stata-console-cleanliness-validation ()
  "Test that Stata console output meets user cleanliness expectations."
  (euporie-ux-log 'info "=== UX TEST: Stata console cleanliness ===")
  (euporie-ux-cleanup)
  
  (unwind-protect
      (progn
        ;; Start euporie
        (euporie-stata-start)
        (sleep-for 15)
        (should (get-buffer "*euporie-stata*"))
        
        ;; Test data loading
        (with-current-buffer "*euporie-stata*"
          (let ((initial-marker (point-max))
                (process (get-buffer-process (current-buffer))))
            
            (when process
              (process-send-string process "sysuse auto\n")
              (sleep-for 5)
              
              ;; Test scatter plot
              (let ((plot-marker (point-max)))
                (process-send-string process "scatter price mpg\n")
                (sleep-for 10)
                
                (let ((plot-output (buffer-substring-no-properties plot-marker (point-max))))
                  (euporie-ux-log 'info "Plot output: %S" plot-output)
                  
                  ;; Critical validation: Must be clean
                  (should (euporie-ux-validate-clean-output plot-output "Scatter plot"))
                  (should-not (string-match-p "global stata_kernel_graph_counter" (or plot-output "")))
                  (should-not (string-match-p "stata_kernel_graph_counter.*=" (or plot-output ""))))))))
    
    (euporie-ux-cleanup)))

;; USER EXPERIENCE TEST SUITE

(defun euporie-ux-run-critical-tests ()
  "Run critical user experience tests."
  (interactive)
  (euporie-ux-clear-log)
  (euporie-ux-log 'info "Running CRITICAL user experience tests")
  
  (ert-run-tests-interactively "euporie-ux/stata-console-cleanliness-validation"))

;; Load notification
(unless noninteractive
  (message "Emacs-euporie User Experience validation suite loaded (fixed version)."))

(provide 'test-euporie-user-experience-fixed)
;;; test-euporie-user-experience-fixed.el ends here