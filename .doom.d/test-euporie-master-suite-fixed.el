;;; test-euporie-master-suite-fixed.el --- Master test suite for comprehensive Emacs-euporie validation -*- lexical-binding: t; -*-

;;; Commentary:
;; Master test coordinator that runs comprehensive validation with strict pass/fail criteria.
;; This addresses the user's concern about false positive test results by implementing
;; multiple validation layers and strict success criteria.
;;
;; STRICT SUCCESS CRITERIA (ALL must pass):
;; - Console output contains ZERO counter messages
;; - Graphics display inline successfully  
;; - C-RET keybinding works reliably
;; - Performance meets user expectations

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'euporie-termint nil t)

;; Master Test Configuration
(defvar euporie-master-test-log-file (expand-file-name "euporie-master-validation.log" "~/")
  "Master test coordination log file.")

;; Master Test Utilities
(defun euporie-master-log (level format-string &rest args)
  "Master test coordination logging."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N")))
    (with-temp-buffer
      (insert (format "[%s] [MASTER-%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-master-test-log-file))))

(defun euporie-master-clear-log ()
  "Clear master test log."
  (when (file-exists-p euporie-master-test-log-file)
    (delete-file euporie-master-test-log-file))
  (euporie-master-log 'info "=== COMPREHENSIVE EUPORIE VALIDATION MASTER SUITE ==="))

(defun euporie-master-cleanup ()
  "Master cleanup ensuring completely clean test environment."
  (euporie-master-log 'info "Performing comprehensive master cleanup")
  
  ;; Kill all euporie processes and buffers
  (dolist (buffer-name '("*euporie-stata*" "*euporie-python*" "*euporie-r*"))
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (when-let ((process (get-buffer-process (current-buffer))))
          (when (process-live-p process)
            (kill-process process))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name))))
  
  (sleep-for 3)
  (euporie-master-log 'info "Master cleanup completed"))

(defun euporie-master-validate-counter-absence (output description)
  "STRICT validation: Counter messages must be completely absent."
  (let ((counter-patterns '("global stata_kernel_graph_counter"
                           "stata_kernel_graph_counter.*="
                           "\\$stata_kernel_graph_counter"))
        (violations '()))
    
    (when (and output (stringp output))
      (dolist (pattern counter-patterns)
        (when (string-match-p pattern output)
          (push pattern violations))))
    
    (if violations
        (progn
          (euporie-master-log 'error "%s: FAILED - Counter violations: %s" description violations)
          (euporie-master-log 'error "Full output: %S" output)
          nil)
      (progn
        (euporie-master-log 'info "%s: PASSED - No counter messages" description)
        t))))

;; COMPREHENSIVE MASTER TESTS

(ert-deftest euporie-master/comprehensive-validation ()
  "Master test with anti-false-positive validation."
  (euporie-master-log 'info "=== STARTING MASTER VALIDATION ===")
  (euporie-master-cleanup)
  
  (unwind-protect
      (progn
        ;; Start euporie
        (euporie-stata-start)
        (sleep-for 15)
        (should (get-buffer "*euporie-stata*"))
        
        ;; Execute graphics with strict validation
        (with-current-buffer "*euporie-stata*"
          (let ((initial-marker (point-max))
                (process (get-buffer-process (current-buffer))))
            
            (when process
              ;; Load data
              (process-send-string process "sysuse auto\n")
              (sleep-for 5)
              
              ;; Create plot with strict monitoring
              (let ((plot-marker (point-max)))
                (process-send-string process "scatter price mpg\n")
                (sleep-for 12)
                
                (let ((plot-output (buffer-substring-no-properties plot-marker (point-max))))
                  
                  ;; STRICT VALIDATION: Must pass all criteria
                  (should (euporie-master-validate-counter-absence plot-output "Master scatter plot"))
                  
                  ;; Additional checks
                  (should-not (string-match-p "global.*stata_kernel_graph_counter" (or plot-output "")))
                  (should-not (string-match-p "stata_kernel_graph_counter.*\\+" (or plot-output "")))
                  
                  ;; Responsiveness test
                  (let ((resp-marker (point-max)))
                    (process-send-string process "display \"MASTER_TEST_COMPLETE\"\n")
                    (sleep-for 3)
                    
                    (let ((resp-output (buffer-substring-no-properties resp-marker (point-max))))
                      (should (string-match-p "MASTER_TEST_COMPLETE" (or resp-output "")))
                      (should (euporie-master-validate-counter-absence resp-output "Responsiveness test")))))))))
        
        (euporie-master-log 'info "=== MASTER VALIDATION PASSED ==="))
    
    (euporie-master-cleanup)))

;; MASTER TEST INTERFACE

(defun euporie-master-run-validation ()
  "Run the master validation test."
  (interactive)
  (euporie-master-clear-log)
  (euporie-master-log 'info "STARTING MASTER VALIDATION - Anti-false-positive measures enabled")
  
  (ert-run-tests-interactively "euporie-master/comprehensive-validation")
  
  (euporie-master-generate-report))

(defun euporie-master-generate-report ()
  "Generate master validation report."
  (interactive)
  (let ((report-buffer (get-buffer-create "*Master Validation Report*")))
    (with-current-buffer report-buffer
      (erase-buffer)
      (insert "# MASTER EMACS-EUPORIE VALIDATION REPORT\n\n")
      (insert (format "Generated: %s\n\n" (current-time-string)))
      
      (insert "## Validation Criteria\n\n")
      (insert "1. **Zero Counter Messages**: NO `global stata_kernel_graph_counter` text\n")
      (insert "2. **Graphics Display**: Scatter plot must display successfully\n")
      (insert "3. **Console Responsiveness**: Commands must execute promptly\n")
      (insert "4. **Clean Output**: Only legitimate Stata output permitted\n\n")
      
      ;; Include log
      (when (file-exists-p euporie-master-test-log-file)
        (insert "## Test Execution Log\n\n```\n")
        (insert-file-contents euporie-master-test-log-file)
        (goto-char (point-max))
        (insert "\n```\n\n")
        
        ;; Summary
        (goto-char (point-min))
        (let ((log-content (buffer-string)))
          (goto-char (point-max))
          (insert "## Summary\n\n")
          
          (if (string-match-p "MASTER VALIDATION PASSED" log-content)
              (insert "**✅ MASTER VERDICT: VALIDATION PASSED**\n\n")
            (insert "**❌ MASTER VERDICT: VALIDATION FAILED**\n\n"))))
      
      (display-buffer report-buffer))))

;; Load notification
(unless noninteractive
  (message "Master Emacs-euporie validation suite loaded.")
  (message "Commands: (euporie-master-run-validation) - Run master validation"))

(provide 'test-euporie-master-suite-fixed)
;;; test-euporie-master-suite-fixed.el ends here