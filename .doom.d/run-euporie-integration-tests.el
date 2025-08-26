;;; run-euporie-integration-tests.el --- Test runner for euporie integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; This script provides an easy way to run the euporie integration tests
;; either interactively or in batch mode.

;;; Code:

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Load required modules
(require 'ert)
(require 'cl-lib)

;; Load euporie integration if available
(condition-case nil
    (progn
      (require 'euporie-termint)
      (require 'termint-org-src))
  (error (message "WARNING: euporie-termint not available. Some tests may fail.")))

;; Load the test suite
(load-file (expand-file-name "tests/test-euporie-integration.el" 
                           (file-name-directory (or load-file-name buffer-file-name))))

(defun run-euporie-tests-batch ()
  "Run euporie integration tests in batch mode."
  (interactive)
  (let ((ert-batch-backtrace-right-margin 120)
        (ert-batch-print-level 10)
        (ert-batch-print-length 120))
    
    ;; Validate environment first
    (message "=== Validating Test Environment ===")
    (unless (euporie-integration-validate-test-environment)
      (message "WARNING: Test environment validation failed. Some tests may not work properly."))
    
    ;; Clear any existing log
    (euporie-integration-test-clear-log)
    
    ;; Run the tests
    (message "\n=== Running Euporie Integration Tests ===")
    (ert-run-tests-batch-and-exit "euporie-integration/")))

(defun run-euporie-tests-interactive ()
  "Run euporie integration tests interactively."
  (interactive)
  ;; Validate environment first
  (message "=== Validating Test Environment ===")
  (unless (euporie-integration-validate-test-environment)
    (message "WARNING: Test environment validation failed. Some tests may not work properly."))
  
  ;; Clear any existing log
  (euporie-integration-test-clear-log)
  
  ;; Run tests interactively
  (message "\n=== Running Euporie Integration Tests (Interactive) ===")
  (ert-run-tests-interactively "euporie-integration/"))

(defun run-euporie-automatic-graphics-tests ()
  "Run only the critical automatic graphics tests."
  (interactive)
  ;; Clear any existing log
  (euporie-integration-test-clear-log)
  
  ;; Run automatic graphics tests
  (message "\n=== Running Automatic Graphics Tests ===")
  (ert-run-tests-interactively 
   '(or "euporie-integration/python-automatic-graphics-display"
        "euporie-integration/r-automatic-graphics-display" 
        "euporie-integration/stata-automatic-graphics-display"
        "euporie-integration/python-matplotlib-function-override"
        "euporie-integration/r-ggplot-function-override")))

;; Check if running in batch mode
(when (and noninteractive (member "--run-tests" command-line-args))
  (run-euporie-tests-batch))

;; Provide helpful message when loaded interactively
(unless noninteractive
  (message "Euporie integration test suite loaded. Available commands:")
  (message "- (run-euporie-tests-interactive) - Run all tests interactively")  
  (message "- (run-euporie-automatic-graphics-tests) - Run automatic graphics tests")
  (message "- (euporie-integration-validate-test-environment) - Check test environment")
  (message "- (euporie-integration-analyze-test-failures) - Analyze test failures"))

(provide 'run-euporie-integration-tests)
;;; run-euporie-integration-tests.el ends here