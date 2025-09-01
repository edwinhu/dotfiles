;;; run-sas-tests.el --- Helper script to run SAS workflow tests -*- lexical-binding: t; -*-

;; This file provides a convenient way to run the SAS workflow tests
;; Can be executed directly or called from other scripts

(add-to-list 'load-path (expand-file-name "~/.doom.d/"))

;; Load the test framework
(require 'sas-workflow-tests)

;; Run the tests
(defun demo-sas-tests ()
  "Run a demonstration of the SAS workflow tests."
  (interactive)
  (message "=== SAS WORKFLOW TEST DEMONSTRATION ===")
  (message "Running comprehensive test suite...")
  
  ;; Run all tests
  (run-all-sas-workflow-tests)
  
  ;; Display results
  (message "Test suite completed. Check *SAS-Test-Results* buffer for details.")
  (when (get-buffer "*SAS-Test-Results*")
    (display-buffer "*SAS-Test-Results*")))

;; For non-interactive use
(when noninteractive
  (demo-sas-tests))

(provide 'run-sas-tests)
;;; run-sas-tests.el ends here