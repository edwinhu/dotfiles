;;; minimal-error-test.el --- DEPRECATED - Use euporie-qrsh-integration-tests.el instead

;; *** DEPRECATED ***
;; This minimal test file is no longer needed as the error it was designed to reproduce has been fixed.
;; The comprehensive QRSH testing is now handled by euporie-qrsh-integration-tests.el
;;
;; NEW TEST SUITE: euporie-qrsh-integration-tests.el
;; - Comprehensive QRSH detection and path format testing
;; - Cross-kernel compatibility testing (Python/R/Stata/SAS)
;; - Enhanced org-babel integration testing
;; - Robust session management testing
;;
;; This file can be safely removed after confirming QRSH integration works.

(require 'euporie-termint)

(defun minimal-test-direct-call ()
  "Test calling euporie-termint-start-remote-universal directly."
  (interactive)
  (message "=== Testing direct call ===")
  (condition-case err
      (progn
        (euporie-termint-start-remote-universal 
         "sas" 
         "/sshx:wrds-cloud:/home/nyu/eddyhu/project/|qrsh::/home/nyu/eddyhu/project/")
        (message "✓ Direct call succeeded"))
    (error
     (message "✗ Direct call failed: %s" err)
     err)))

(defun minimal-test-send-code ()
  "Test calling euporie-termint-send-code (the actual failing call)."
  (interactive)
  (message "=== Testing send-code call ===")
  (condition-case err
      (progn
        (euporie-termint-send-code 
         "sas" 
         "data _null_; put 'test'; run;" 
         "/sshx:wrds-cloud:/home/nyu/eddyhu/project/|qrsh::/home/nyu/eddyhu/project/")
        (message "✓ Send-code call succeeded"))
    (error
     (message "✗ Send-code call failed: %s" err)
     err)))

(defun minimal-test-get-buffer ()
  "Test calling euporie-termint-get-or-create-buffer (intermediate step)."
  (interactive)
  (message "=== Testing get-buffer call ===")
  (condition-case err
      (progn
        (euporie-termint-get-or-create-buffer 
         "sas" 
         "/sshx:wrds-cloud:/home/nyu/eddyhu/project/|qrsh::/home/nyu/eddyhu/project/")
        (message "✓ Get-buffer call succeeded"))
    (error
     (message "✗ Get-buffer call failed: %s" err)
     err)))

(defun minimal-test-all ()
  "DEPRECATED: Use euporie-qrsh-run-all-tests from euporie-qrsh-integration-tests.el instead."
  (interactive)
  (message "*** DEPRECATED FUNCTION ***")
  (message "This test suite has been replaced by euporie-qrsh-integration-tests.el")
  (message "The error these tests checked for has been resolved.")
  (message "Please use: M-x euporie-qrsh-run-all-tests")
  (when (not (y-or-n-p "Continue with deprecated minimal tests anyway? "))
    (message "Cancelled. Use the new comprehensive test suite.")
    (return nil))
  (message "========================================")
  (message "DEPRECATED: MINIMAL ERROR ISOLATION TESTS")
  (message "Use: euporie-qrsh-integration-tests.el instead")
  (message "========================================")
  
  (let ((results '()))
    
    ;; Test 1: Direct call
    (push (cons "direct-call" (minimal-test-direct-call)) results)
    (sleep-for 2)
    
    ;; Test 2: Get buffer call
    (push (cons "get-buffer" (minimal-test-get-buffer)) results)
    (sleep-for 2)
    
    ;; Test 3: Send code call (the one that should fail)
    (push (cons "send-code" (minimal-test-send-code)) results)
    
    ;; Print summary
    (message "========================================")
    (message "MINIMAL TEST RESULTS:")
    (dolist (result results)
      (if (stringp (cdr result))
          (message "✓ %s: %s" (car result) (cdr result))
        (message "✗ %s: %s" (car result) (cdr result))))
    (message "========================================")
    
    results))

(provide 'minimal-error-test)
;;; minimal-error-test.el ends here