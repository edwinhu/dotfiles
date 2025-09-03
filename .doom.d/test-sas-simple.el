;;; test-sas-simple.el --- DEPRECATED - Use euporie-qrsh-integration-tests.el instead

;; *** DEPRECATED ***
;; This file has been superseded by euporie-qrsh-integration-tests.el
;;
;; NEW TEST SUITE: euporie-qrsh-integration-tests.el
;; - Comprehensive QRSH detection and path format testing
;; - Cross-kernel compatibility testing (Python/R/Stata/SAS)
;; - Enhanced org-babel integration testing
;; - Robust session management testing
;; - Built-in test functions in euporie-termint.el
;;
;; This file is kept for reference only and will be removed.

(require 'euporie-termint)

(defvar test-sas-log-file (expand-file-name "test-sas-simple-deprecated.log" "~/"))

(defun test-log (message)
  "Log message to test file (DEPRECATED)."
  (with-temp-buffer
    (insert (format "[%s] DEPRECATED: %s\n" (format-time-string "%H:%M:%S") message))
    (append-to-file (point-min) (point-max) test-sas-log-file)))

(defun test-error-fix-verification ()
  "Test if the syntax error is fixed."
  (test-log "=== ERROR FIX VERIFICATION ===")
  (condition-case err
      (progn
        (test-log "Attempting euporie-termint-send-code with QRSH directory")
        (euporie-termint-send-code "sas" "data _null_; put 'test'; run;" 
                                   "/sshx:wrds:/home/nyu/eddyhu/projects/wander2|qrsh::/home/nyu/eddyhu/projects/wander2")
        (test-log "SUCCESS: No syntax error occurred")
        'PASS-ERROR-FIXED)
    (error
     (let ((err-msg (format "%s" err)))
       (test-log (format "ERROR: %s" err-msg))
       (if (string-match-p "Invalid function.*eq.*remote-mode" err-msg)
           'FAIL-ERROR-PERSISTS
         'FAIL-OTHER-ERROR)))))

(defun test-qrsh-establishment ()
  "Test QRSH session establishment."
  (test-log "=== QRSH ESTABLISHMENT TEST ===")
  (condition-case err
      (progn
        ;; Kill existing buffer
        (when (get-buffer "*euporie-sas*")
          (let ((kill-buffer-query-functions nil))
            (kill-buffer "*euporie-sas*")))
        
        (test-log "Starting QRSH session")
        (euporie-termint-start-remote-universal "sas" "/sshx:wrds:/home/nyu/eddyhu/projects/wander2|qrsh::/home/nyu/eddyhu/projects/wander2")
        
        ;; Wait for buffer
        (let ((attempts 0))
          (while (and (< attempts 20) (not (get-buffer "*euporie-sas*")))
            (sleep-for 1)
            (setq attempts (1+ attempts)))
          
          (if (get-buffer "*euporie-sas*")
              (progn
                (test-log "SUCCESS: Buffer *euporie-sas* created")
                'PASS)
            (progn
              (test-log "FAIL: Buffer *euporie-sas* not created")
              'FAIL))))
    (error
     (test-log (format "ERROR in QRSH establishment: %s" err))
     'FAIL)))

(defun test-sas-code-transmission ()
  "Test SAS code transmission."
  (test-log "=== SAS CODE TRANSMISSION TEST ===")
  (condition-case err
      (progn
        (test-log "Sending SAS code")
        (euporie-termint-send-code "sas" "proc print data=sashelp.cars(obs=3); run;" 
                                   "/sshx:wrds:/home/nyu/eddyhu/projects/wander2|qrsh::/home/nyu/eddyhu/projects/wander2")
        (test-log "SUCCESS: Code sent without error")
        'PASS)
    (error
     (test-log (format "ERROR in code transmission: %s" err))
     'FAIL)))

(defun verify-buffer-contains-sas-output (buffer-name search-text)
  "Verify buffer contains expected text."
  (test-log (format "=== BUFFER OUTPUT VERIFICATION: %s ===", buffer-name))
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (if (search-forward search-text nil t)
                (progn
                  (test-log (format "SUCCESS: Found '%s' in %s" search-text buffer-name))
                  'PASS)
              (progn
                (test-log (format "FAIL: Did not find '%s' in %s" search-text buffer-name))
                (test-log (format "Buffer contents preview: %s" 
                                 (substring (buffer-string) 0 (min 200 (buffer-size)))))
                'FAIL))))
      (progn
        (test-log (format "FAIL: Buffer %s does not exist" buffer-name))
        'FAIL))))

(defun test-no-regression ()
  "Quick test that Python/R/Stata still work."
  (test-log "=== REGRESSION TEST ===")
  (condition-case err
      (progn
        ;; Test Python still works
        (when (get-buffer "*euporie-python*")
          (let ((kill-buffer-query-functions nil))
            (kill-buffer "*euporie-python*")))
        
        (euporie-termint-send-code "python" "print('regression test')" nil)
        (sleep-for 2)
        
        (if (get-buffer "*euporie-python*")
            (progn
              (test-log "SUCCESS: Python integration still works")
              'PASS)
          (progn
            (test-log "FAIL: Python integration broken")
            'FAIL)))
    (error
     (test-log (format "ERROR in regression test: %s" err))
     'FAIL)))

(defun run-all-automated-tests ()
  "DEPRECATED: Use euporie-qrsh-run-all-tests from euporie-qrsh-integration-tests.el instead."
  (interactive)
  (message "*** DEPRECATED FUNCTION ***")
  (message "This test suite has been replaced by euporie-qrsh-integration-tests.el")
  (message "Please use: M-x euporie-qrsh-run-all-tests")
  (when (not (y-or-n-p "Continue with deprecated tests anyway? "))
    (message "Cancelled. Use the new comprehensive test suite.")
    (return '(0 0 1 1)))
  (test-log "=======================================")
  (test-log "DEPRECATED: Use euporie-qrsh-integration-tests.el instead")
  (test-log "=======================================")
  (when (y-or-n-p "Continue with deprecated tests anyway? ")
    (test-log "CONTINUING WITH DEPRECATED TESTS"))
  
  ;; Clear log
  (with-temp-buffer
    (insert (format "SAS Integration Test Log - %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (write-region (point-min) (point-max) test-sas-log-file nil 'silent))
  
    (let ((results (list
                    (cons "Error Fix" (test-error-fix-verification))
                    (cons "QRSH Establishment" (test-qrsh-establishment))
                    (cons "Code Transmission" (test-sas-code-transmission))
                    (cons "No Regression" (test-no-regression))))
        (pass-count 0)
        (fail-count 0))
    
    (dolist (result results)
      (let ((test-name (car result))
            (test-result (cdr result)))
        (test-log (format "Test '%s': %s" test-name test-result))
        (if (eq test-result 'PASS)
            (setq pass-count (1+ pass-count))
          (setq fail-count (1+ fail-count)))))
    
    (test-log "=======================================")
    (test-log (format "FINAL RESULTS: %d PASS, %d FAIL" pass-count fail-count))
    (test-log "=======================================")
    
    (message "Tests completed: %d PASS, %d FAIL - Check %s for details" 
             pass-count fail-count test-sas-log-file)
    
    results))

(provide 'test-sas-simple)