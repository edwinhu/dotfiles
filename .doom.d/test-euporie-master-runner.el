;;; test-euporie-master-runner.el --- Master test runner for all euporie tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive test runner that executes all euporie integration tests
;; and provides detailed reporting on both clean console output and
;; inline graphics display requirements.

;;; Code:

(require 'ert)
(require 'test-euporie-dual-requirements nil t)
;; (require 'test-euporie-graphics-detection nil t)  ; Temporarily disabled 
(require 'test-euporie-visual-verification nil t)

(defvar euporie-master-test-log (expand-file-name "euporie-master-test-results.log" "~/"))
(defvar euporie-master-test-start-time nil)

(defun euporie-master-log (level format-string &rest args)
  "Log master test message with timestamp."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [MASTER] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-master-test-log))
    ;; Also display in messages
    (message "[%s] %s" (upcase (symbol-name level)) message)))

;;; Test Suite Organization

(defvar euporie-master-test-suites
  '((critical . '("euporie-dual/stata-console-and-graphics-integration"))
    (performance . '("euporie-dual/stata-multiple-plots-sustained-performance"
                    "euporie-visual/rapid-fire-graphics-test"))
    (workflow . '("euporie-dual/stata-user-workflow-simulation"
                 "euporie-visual/comprehensive-stata-workflow"))
    ;; (detection . '("euporie-graphics/sixel-detection-precision"
    ;;               "euporie-graphics/buffer-composition-analysis"))  ; Temporarily disabled
    (all . '("euporie-dual/stata-console-and-graphics-integration"
             "euporie-dual/stata-multiple-plots-sustained-performance"
             "euporie-dual/stata-user-workflow-simulation"
             "euporie-visual/comprehensive-stata-workflow"
             "euporie-visual/rapid-fire-graphics-test")))
  "Organized test suites for different testing scenarios.")

;;; Test Execution and Reporting

(defun euporie-master-run-test-suite (suite-name)
  "Run a specific test suite and return detailed results."
  (euporie-master-log 'info "=== RUNNING TEST SUITE: %s ===" (upcase (symbol-name suite-name)))
  
  (let ((test-names (cdr (assoc suite-name euporie-master-test-suites)))
        (results '())
        (start-time (current-time)))
    
    (unless test-names
      (error "Unknown test suite: %s" suite-name))
    
    (dolist (test-name test-names)
      (euporie-master-log 'info "Executing test: %s" test-name)
      
      (let* ((test-start (current-time))
             (test-result (ert-run-test (ert-get-test (intern test-name))))
             (test-duration (time-to-seconds (time-subtract (current-time) test-start)))
             (test-status (ert-test-result-type-of test-result)))
        
        (push (list :name test-name
                   :status test-status
                   :duration test-duration
                   :result test-result) results)
        
        (euporie-master-log (if (eq test-status :passed) 'info 'error)
                           "Test %s: %s (%.2fs)" test-name test-status test-duration)))
    
    (let ((suite-duration (time-to-seconds (time-subtract (current-time) start-time))))
      (euporie-master-log 'info "=== SUITE %s COMPLETED: %.2fs ===" 
                         (upcase (symbol-name suite-name)) suite-duration)
      
      (list :suite suite-name
            :duration suite-duration  
            :tests (reverse results)))))

(defun euporie-master-analyze-results (suite-results)
  "Analyze and report on test suite results."
  (let* ((tests (plist-get suite-results :tests))
         (total-tests (length tests))
         (passed-tests (length (seq-filter (lambda (test) (eq (plist-get test :status) :passed)) tests)))
         (failed-tests (length (seq-filter (lambda (test) (eq (plist-get test :status) :failed)) tests)))
         (error-tests (length (seq-filter (lambda (test) (eq (plist-get test :status) :error)) tests)))
         (total-duration (plist-get suite-results :duration))
         (suite-name (plist-get suite-results :suite)))
    
    (euporie-master-log 'info "")
    (euporie-master-log 'info "=== TEST SUITE ANALYSIS: %s ===" (upcase (symbol-name suite-name)))
    (euporie-master-log 'info "Total Tests: %d" total-tests)
    (euporie-master-log 'info "Passed: %d (%.1f%%)" passed-tests (* 100.0 (/ (float passed-tests) total-tests)))
    (euporie-master-log 'info "Failed: %d (%.1f%%)" failed-tests (* 100.0 (/ (float failed-tests) total-tests)))
    (euporie-master-log 'info "Errors: %d (%.1f%%)" error-tests (* 100.0 (/ (float error-tests) total-tests)))
    (euporie-master-log 'info "Total Duration: %.2f seconds" total-duration)
    (euporie-master-log 'info "")
    
    ;; Detailed failure analysis
    (when (> (+ failed-tests error-tests) 0)
      (euporie-master-log 'warn "=== FAILURE ANALYSIS ===")
      (dolist (test tests)
        (let ((status (plist-get test :status))
              (name (plist-get test :name)))
          (when (member status '(:failed :error))
            (euporie-master-log 'error "FAILED TEST: %s - Status: %s" name status)
            (let ((result (plist-get test :result)))
              (when (ert-test-result-with-condition-p result)
                (let ((condition (ert-test-result-with-condition-condition result)))
                  (euporie-master-log 'error "  Condition: %s" condition)
                  (when (and (listp condition) (> (length condition) 1))
                    (euporie-master-log 'error "  Details: %s" (cadr condition))))))))))
    
    ;; Return summary
    (list :total total-tests
          :passed passed-tests
          :failed failed-tests
          :errors error-tests
          :success-rate (if (> total-tests 0) (/ (float passed-tests) total-tests) 0)
          :duration total-duration)))

;;; Main Test Runners

(defun euporie-master-run-critical-tests ()
  "Run only the critical dual-requirements test to verify current failure."
  (interactive)
  (when (file-exists-p euporie-master-test-log) (delete-file euporie-master-test-log))
  (setq euporie-master-test-start-time (current-time))
  
  (euporie-master-log 'info "======================================")
  (euporie-master-log 'info "EUPORIE CRITICAL TEST EXECUTION")
  (euporie-master-log 'info "Expected Result: FAILURE (graphics broken)")
  (euporie-master-log 'info "======================================")
  
  (let* ((results (euporie-master-run-test-suite 'critical))
         (analysis (euporie-master-analyze-results results)))
    
    (euporie-master-log 'info "")
    (euporie-master-log 'info "=== CRITICAL TEST CONCLUSION ===")
    (if (< (plist-get analysis :success-rate) 0.5)
        (euporie-master-log 'warn "✓ EXPECTED FAILURE CONFIRMED: Graphics display is broken")
      (euporie-master-log 'error "✗ UNEXPECTED SUCCESS: Tests should fail with current implementation"))
    
    (euporie-master-log 'info "Total Execution Time: %.2fs" 
                       (time-to-seconds (time-subtract (current-time) euporie-master-test-start-time)))
    
    ;; Show log file location
    (euporie-master-log 'info "")
    (euporie-master-log 'info "Detailed results saved to: %s" euporie-master-test-log)
    (message "Critical test completed. Check log: %s" euporie-master-test-log)))

(defun euporie-master-run-all-tests ()
  "Run complete test suite for comprehensive validation."
  (interactive)
  (when (file-exists-p euporie-master-test-log) (delete-file euporie-master-test-log))
  (setq euporie-master-test-start-time (current-time))
  
  (euporie-master-log 'info "========================================")
  (euporie-master-log 'info "EUPORIE COMPREHENSIVE TEST EXECUTION") 
  (euporie-master-log 'info "Testing: Clean Output + Inline Graphics")
  (euporie-master-log 'info "========================================")
  
  (let* ((results (euporie-master-run-test-suite 'all))
         (analysis (euporie-master-analyze-results results)))
    
    (euporie-master-log 'info "")
    (euporie-master-log 'info "=== COMPREHENSIVE TEST CONCLUSION ===")
    (cond
     ((>= (plist-get analysis :success-rate) 0.8)
      (euporie-master-log 'info "✓ EXCELLENT: Both requirements working well"))
     ((>= (plist-get analysis :success-rate) 0.5) 
      (euporie-master-log 'warn "◐ PARTIAL: Some requirements working"))
     (t
      (euporie-master-log 'error "✗ FAILED: Major issues with dual requirements")))
    
    (euporie-master-log 'info "Overall Success Rate: %.1f%%" 
                       (* 100 (plist-get analysis :success-rate)))
    
    (euporie-master-log 'info "Total Execution Time: %.2fs"
                       (time-to-seconds (time-subtract (current-time) euporie-master-test-start-time)))
    
    (message "Comprehensive test completed. Success rate: %.1f%%. Check log: %s" 
             (* 100 (plist-get analysis :success-rate)) euporie-master-test-log)))

;;; Quick Test Functions

(defun euporie-master-quick-graphics-test ()
  "Quick test focusing only on graphics detection."
  (interactive)
  (when (file-exists-p euporie-master-test-log) (delete-file euporie-master-test-log))
  
  (euporie-master-log 'info "=== QUICK GRAPHICS DETECTION TEST ===")
  
  (let* ((results (euporie-master-run-test-suite 'detection))
         (analysis (euporie-master-analyze-results results)))
    
    (if (> (plist-get analysis :success-rate) 0.5)
        (euporie-master-log 'info "✓ Graphics detection working")
      (euporie-master-log 'warn "✗ Graphics detection failing"))
    
    (message "Graphics test completed. Check log: %s" euporie-master-test-log)))

;;; Utility Functions

(defun euporie-master-view-log ()
  "View the master test log file."
  (interactive)
  (if (file-exists-p euporie-master-test-log)
      (find-file euporie-master-test-log)
    (message "Master test log not found: %s" euporie-master-test-log)))

(defun euporie-master-cleanup-test-files ()
  "Clean up all test-related files."
  (interactive)
  (let ((files-to-clean '("euporie-master-test-results.log"
                         "euporie-dual-requirements-test.log"
                         "graphics-detection-test.log" 
                         "visual-verification-test.log")))
    (dolist (file files-to-clean)
      (let ((full-path (expand-file-name file "~/")))
        (when (file-exists-p full-path)
          (delete-file full-path)
          (message "Deleted: %s" full-path))))
    (message "Test files cleaned up.")))

(provide 'test-euporie-master-runner)
;;; test-euporie-master-runner.el ends here