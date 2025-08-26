;;; run-euporie-tests.el --- Test runner for Euporie integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple test runner script for the Euporie integration test suite.
;; Can be run interactively in Emacs or via command line with emacsclient.

;;; Code:

(require 'ert)

;; Add test directory to load path
(add-to-list 'load-path (expand-file-name "tests" (file-name-directory load-file-name)))

;; Load test files
(require 'test-euporie-integration)

(defun run-euporie-tests-interactive ()
  "Run euporie integration tests interactively with detailed output."
  (interactive)
  (message "Starting Euporie Integration Test Suite...")
  (message "===========================================")
  
  ;; Clear any existing test buffers
  (when (fboundp 'euporie-integration-test-cleanup-buffers)
    (euporie-integration-test-cleanup-buffers))
  
  ;; Run tests with interactive output
  (let ((ert-batch-mode nil))
    (ert-run-tests-interactively "^euporie-integration/")))

(defun run-euporie-tests-batch ()
  "Run euporie integration tests in batch mode for automation."
  (interactive)
  (message "Running Euporie Integration Tests in batch mode...")
  
  ;; Clean up first
  (when (fboundp 'euporie-integration-test-cleanup-buffers)
    (euporie-integration-test-cleanup-buffers))
  
  ;; Run tests
  (ert-run-tests-batch "^euporie-integration/"))

(defun run-euporie-quick-tests ()
  "Run a subset of quick euporie tests for development."
  (interactive)
  (message "Running Quick Euporie Tests...")
  
  ;; Clean up first
  (when (fboundp 'euporie-integration-test-cleanup-buffers)
    (euporie-integration-test-cleanup-buffers))
  
  ;; Run just the core functionality tests
  (ert-run-tests-interactively "euporie-integration/kernel-detection\\|euporie-integration/c-ret-keybinding"))

(defun run-euporie-graphics-tests ()
  "Run graphics-specific euporie tests."
  (interactive)
  (message "Running Euporie Graphics Tests...")
  
  ;; Clean up first
  (when (fboundp 'euporie-integration-test-cleanup-buffers)
    (euporie-integration-test-cleanup-buffers))
  
  ;; Run graphics tests
  (ert-run-tests-interactively "euporie-integration/.*graphics\\|euporie-integration/complete.*workflow"))

(defun run-euporie-layout-tests ()
  "Run window layout-specific euporie tests."
  (interactive)
  (message "Running Euporie Layout Tests...")
  
  ;; Clean up first
  (when (fboundp 'euporie-integration-test-cleanup-buffers)
    (euporie-integration-test-cleanup-buffers))
  
  ;; Run layout tests
  (ert-run-tests-interactively "euporie-integration/split-window-layout"))

;; Command line interface functions
(defun euporie-tests-check-prerequisites ()
  "Check if all prerequisites for euporie tests are available."
  (interactive)
  (let ((pixi-available (executable-find "pixi"))
        (termint-available (featurep 'termint))
        (euporie-available (featurep 'euporie-termint)))
    
    (message "=== Euporie Test Prerequisites ===")
    (message "pixi executable: %s" (if pixi-available "✓ Available" "✗ Missing"))
    (message "termint.el: %s" (if termint-available "✓ Loaded" "✗ Missing"))  
    (message "euporie-termint.el: %s" (if euporie-available "✓ Loaded" "✗ Missing"))
    
    (if (and pixi-available termint-available euporie-available)
        (message "✓ All prerequisites met - ready to run tests")
      (message "✗ Prerequisites missing - tests may fail"))))

(defun euporie-tests-show-help ()
  "Show help for euporie test runner functions."
  (interactive)
  (message "=== Euporie Test Runner Help ===")
  (message "Available test functions:")
  (message "  run-euporie-tests-interactive    - Full interactive test suite")
  (message "  run-euporie-tests-batch         - Batch mode for automation")
  (message "  run-euporie-quick-tests         - Quick core functionality tests")
  (message "  run-euporie-graphics-tests      - Graphics display tests only")
  (message "  run-euporie-layout-tests        - Window layout tests only")
  (message "  euporie-tests-check-prerequisites - Check system requirements")
  (message "  euporie-tests-show-help         - Show this help")
  (message "")
  (message "Log file: ~/euporie-integration-test.log")
  (message "Test specification: ~/.doom.d/EUPORIE_TEST_SPECIFICATION.md"))

;; Auto-run help on load if not in batch mode
(unless (bound-and-true-p ert-batch-mode)
  (run-with-idle-timer 1 nil #'euporie-tests-show-help))

(provide 'run-euporie-tests)
;;; run-euporie-tests.el ends here