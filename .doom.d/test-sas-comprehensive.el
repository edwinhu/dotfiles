;;; test-sas-comprehensive.el --- DEPRECATED - Use euporie-qrsh-integration-tests.el instead

;; *** DEPRECATED ***
;; This file has been superseded by euporie-qrsh-integration-tests.el
;; 
;; NEW TEST SUITE: euporie-qrsh-integration-tests.el provides:
;; - Comprehensive QRSH detection and path format testing
;; - Cross-kernel compatibility testing (Python/R/Stata/SAS)
;; - Enhanced org-babel integration testing
;; - Robust session management testing
;; - Deterministic test verification with logging
;; - Screenshot capture for visual verification
;;
;; Created: 2025-01-17 (EST) - DEPRECATED: 2025-01-19 (EST)
;; Purpose: Legacy SAS integration testing (SUPERSEDED)

(require 'euporie-termint)

;;; Test Configuration

(defvar test-sas-comprehensive-debug-log-file 
  (expand-file-name "test-sas-comprehensive-deprecated.log" "~/")
  "Debug log file for comprehensive SAS testing (DEPRECATED).")

(defvar test-sas-comprehensive-test-timeout 30
  "Timeout in seconds for individual tests.")

(defvar test-sas-comprehensive-qrsh-test-dir
  "/sshx:wrds-cloud:/home/nyu/eddyhu/project/|qrsh::/home/nyu/eddyhu/project/"
  "Test QRSH directory for remote SAS testing.")

;;; Logging System

(defun test-sas-comprehensive-log (level format-string &rest args)
  "Log message with timestamp to comprehensive test log file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S EST")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) test-sas-comprehensive-debug-log-file))))

;;; Helper Functions

(defun test-sas-comprehensive-kill-all-buffers ()
  "Kill all SAS-related buffers to start with clean state."
  (test-sas-comprehensive-log 'info "Killing all SAS-related buffers")
  (dolist (buf-name '("*euporie-sas*" "*qrsh-session*" "*euporie-sas-remote*"))
    (when (get-buffer buf-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf-name))
      (test-sas-comprehensive-log 'debug "Killed buffer: %s" buf-name))))

(defun test-sas-comprehensive-wait-for-buffer (buffer-name timeout)
  "Wait for BUFFER-NAME to exist with timeout TIMEOUT seconds.
Returns buffer if found, nil if timeout."
  (test-sas-comprehensive-log 'debug "Waiting for buffer %s (timeout: %ds)" buffer-name timeout)
  (let ((start-time (current-time))
        (buffer nil))
    (while (and (not buffer) 
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (setq buffer (get-buffer buffer-name))
      (unless buffer
        (sleep-for 0.5)))
    (if buffer
        (test-sas-comprehensive-log 'info "Buffer %s found after %.1fs" 
                                   buffer-name 
                                   (float-time (time-subtract (current-time) start-time)))
      (test-sas-comprehensive-log 'error "Buffer %s NOT FOUND after %ds timeout" buffer-name timeout))
    buffer))

(defun test-sas-comprehensive-wait-for-text-in-buffer (buffer text timeout)
  "Wait for TEXT to appear in BUFFER within TIMEOUT seconds.
Returns t if found, nil if timeout."
  (test-sas-comprehensive-log 'debug "Waiting for text '%s' in buffer %s (timeout: %ds)" 
                             (substring text 0 (min 50 (length text))) 
                             (buffer-name buffer) 
                             timeout)
  (let ((start-time (current-time))
        (found nil))
    (while (and (not found) 
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (when (search-forward text nil t)
            (setq found t))))
      (unless found
        (sleep-for 1)))
    (if found
        (test-sas-comprehensive-log 'info "Text '%s' found in buffer after %.1fs" 
                                   (substring text 0 (min 50 (length text)))
                                   (float-time (time-subtract (current-time) start-time)))
      (test-sas-comprehensive-log 'error "Text '%s' NOT FOUND in buffer after %ds timeout" 
                                 (substring text 0 (min 50 (length text))) timeout))
    found))

;;; Core Test Functions

(defun test-error-reproduction ()
  "Reproduce the 'Invalid function: (eq remote-mode 'standard-tramp)' error.
Returns: 'PASS if error is fixed, 'FAIL if error reproduces, 'ERROR if unexpected."
  (test-sas-comprehensive-log 'info "=== STARTING test-error-reproduction ===")
  
  ;; Clean slate
  (test-sas-comprehensive-kill-all-buffers)
  
  ;; Enable full debugging to get stack trace
  (let ((debug-on-error t)
        (debug-on-signal nil))
    (condition-case err
        (progn
          ;; Try to reproduce the error scenario
          (test-sas-comprehensive-log 'info "Attempting to trigger error with QRSH remote directory")
          (test-sas-comprehensive-log 'debug "Setting debug-on-error to capture stack trace")
          (euporie-termint-send-code "sas" "data _null_; put 'Error reproduction test'; run;" 
                                    test-sas-comprehensive-qrsh-test-dir)
          
          ;; If we get here without error, the bug is fixed
          (test-sas-comprehensive-log 'info "✓ No 'Invalid function' error occurred - BUG APPEARS FIXED")
          'PASS)
    
    ;; Check for specific error we're trying to fix
    ((error invalid-function)
     (let ((error-msg (format "%s" err)))
       (if (string-match-p "Invalid function.*eq.*remote-mode.*standard-tramp" error-msg)
           (progn
             (test-sas-comprehensive-log 'error "✗ ORIGINAL ERROR STILL OCCURS: %s" error-msg)
             'FAIL)
         (progn
           (test-sas-comprehensive-log 'error "✗ DIFFERENT ERROR OCCURRED: %s" error-msg)
           'ERROR))))
    
    ;; Any other error
    (error
     (test-sas-comprehensive-log 'error "✗ UNEXPECTED ERROR: %s" err)
     'ERROR)))

(defun test-qrsh-establishment ()
  "Test QRSH session establishment without code transmission.
Returns: 'PASS if session created, 'FAIL if failed, 'TIMEOUT if timeout."
  (test-sas-comprehensive-log 'info "=== STARTING test-qrsh-establishment ===")
  
  ;; Clean slate
  (test-sas-comprehensive-kill-all-buffers)
  
  (condition-case err
      (progn
        ;; Try to establish QRSH session
        (test-sas-comprehensive-log 'info "Attempting QRSH SAS session establishment")
        (euporie-termint-start-remote-universal "sas" test-sas-comprehensive-qrsh-test-dir)
        
        ;; Wait for buffer creation
        (let ((buffer (test-sas-comprehensive-wait-for-buffer "*euporie-sas*" test-sas-comprehensive-test-timeout)))
          (if buffer
              (progn
                (test-sas-comprehensive-log 'info "✓ QRSH session buffer created successfully")
                ;; Check if process is alive
                (let ((process (get-buffer-process buffer)))
                  (if (and process (process-live-p process))
                      (progn
                        (test-sas-comprehensive-log 'info "✓ QRSH session process is alive")
                        'PASS)
                    (progn
                      (test-sas-comprehensive-log 'error "✗ QRSH session buffer exists but process is dead")
                      'FAIL))))
            (progn
              (test-sas-comprehensive-log 'error "✗ QRSH session buffer not created within timeout")
              'TIMEOUT))))
    
    (error
     (test-sas-comprehensive-log 'error "✗ QRSH establishment failed: %s" err)
     'FAIL)))

(defun test-sas-code-transmission ()
  "Test SAS code transmission to established QRSH session.
Returns: 'PASS if code sent successfully, 'FAIL if failed, 'TIMEOUT if timeout."
  (test-sas-comprehensive-log 'info "=== STARTING test-sas-code-transmission ===")
  
  ;; Clean slate
  (test-sas-comprehensive-kill-all-buffers)
  
  (condition-case err
      (progn
        ;; First establish session
        (test-sas-comprehensive-log 'info "Step 1: Establishing QRSH SAS session")
        (euporie-termint-start-remote-universal "sas" test-sas-comprehensive-qrsh-test-dir)
        
        ;; Wait for buffer
        (let ((buffer (test-sas-comprehensive-wait-for-buffer "*euporie-sas*" test-sas-comprehensive-test-timeout)))
          (if buffer
              (progn
                (test-sas-comprehensive-log 'info "Step 2: Sending test SAS code")
                ;; Send actual SAS code
                (euporie-termint-send-code "sas" 
                                          "data _null_; put 'SAS CODE TRANSMISSION TEST SUCCESSFUL'; run;" 
                                          test-sas-comprehensive-qrsh-test-dir)
                
                ;; Check if code was sent (no error occurred)
                (test-sas-comprehensive-log 'info "✓ SAS code transmission completed without error")
                'PASS)
            (progn
              (test-sas-comprehensive-log 'error "✗ Could not establish session for code transmission test")
              'FAIL))))
    
    (error
     (test-sas-comprehensive-log 'error "✗ SAS code transmission failed: %s" err)
     'FAIL)))

(defun verify-buffer-contains-sas-output ()
  "Verify that *euporie-sas* buffer contains expected SAS output.
Returns: 'PASS if output found, 'FAIL if not found, 'TIMEOUT if timeout."
  (test-sas-comprehensive-log 'info "=== STARTING verify-buffer-contains-sas-output ===")
  
  ;; Clean slate
  (test-sas-comprehensive-kill-all-buffers)
  
  (condition-case err
      (progn
        ;; Establish session and send code
        (test-sas-comprehensive-log 'info "Step 1: Establishing session and sending PROC PRINT test")
        (euporie-termint-start-remote-universal "sas" test-sas-comprehensive-qrsh-test-dir)
        
        ;; Wait for buffer
        (let ((buffer (test-sas-comprehensive-wait-for-buffer "*euporie-sas*" test-sas-comprehensive-test-timeout)))
          (if buffer
              (progn
                ;; Send PROC PRINT command to verify output
                (euporie-termint-send-code "sas" 
                                          "proc print data=sashelp.cars (obs=5); title 'AUTOMATED TEST OUTPUT VERIFICATION'; run;" 
                                          test-sas-comprehensive-qrsh-test-dir)
                
                ;; Wait for output to appear
                (if (test-sas-comprehensive-wait-for-text-in-buffer buffer "AUTOMATED TEST OUTPUT VERIFICATION" 
                                                                   (+ test-sas-comprehensive-test-timeout 10))
                    (progn
                      (test-sas-comprehensive-log 'info "✓ SAS PROC PRINT output found in buffer")
                      'PASS)
                  (progn
                    ;; Log buffer contents for debugging
                    (with-current-buffer buffer
                      (test-sas-comprehensive-log 'debug "Buffer contents: %s" 
                                                 (substring (buffer-string) 
                                                           (max 0 (- (point-max) 500)) 
                                                           (point-max))))
                    (test-sas-comprehensive-log 'error "✗ Expected SAS output not found in buffer")
                    'TIMEOUT)))
            (progn
              (test-sas-comprehensive-log 'error "✗ Could not establish session for output verification")
              'FAIL))))
    
    (error
     (test-sas-comprehensive-log 'error "✗ Output verification failed: %s" err)
     'FAIL)))

;;; Master Test Runner

(defun run-all-automated-tests ()
  "DEPRECATED: Use euporie-qrsh-run-all-tests from euporie-qrsh-integration-tests.el instead."
  (interactive)
  (message "*** DEPRECATED FUNCTION ***")
  (message "This test suite has been replaced by euporie-qrsh-integration-tests.el")
  (message "Please use: M-x euporie-qrsh-run-all-tests")
  (when (not (y-or-n-p "Continue with deprecated tests anyway? "))
    (message "Cancelled. Use the new comprehensive test suite.")
    (return '(0 0 1 1)))
  (test-sas-comprehensive-log 'info "========================================")
  (test-sas-comprehensive-log 'info "DEPRECATED: STARTING OLD COMPREHENSIVE SAS TEST SUITE")
  (test-sas-comprehensive-log 'info "NEW TEST SUITE: Use euporie-qrsh-integration-tests.el")
  (test-sas-comprehensive-log 'info "Timestamp: %s" (format-time-string "%Y-%m-%d %H:%M:%S EST"))
  (test-sas-comprehensive-log 'info "========================================")
  
  ;; Clear previous log content
  (with-temp-buffer
    (insert (format "=== SAS COMPREHENSIVE TEST LOG - %s ===\n\n" 
                    (format-time-string "%Y-%m-%d %H:%M:%S EST")))
    (write-region (point-min) (point-max) test-sas-comprehensive-debug-log-file nil 'silent))
  
  (let ((tests '(("Error Reproduction Fix" test-error-reproduction)
                 ("QRSH Session Establishment" test-qrsh-establishment)
                 ("SAS Code Transmission" test-sas-code-transmission)
                 ("SAS Output Verification" verify-buffer-contains-sas-output)))
        (results '())
        (pass-count 0)
        (fail-count 0)
        (error-count 0)
        (timeout-count 0))
    
    ;; Run each test
    (dolist (test tests)
      (let* ((test-name (car test))
             (test-function (cadr test))
             (result (funcall test-function)))
        (push (cons test-name result) results)
        
        (cond
         ((eq result 'PASS) (setq pass-count (1+ pass-count)))
         ((eq result 'FAIL) (setq fail-count (1+ fail-count)))
         ((eq result 'TIMEOUT) (setq timeout-count (1+ timeout-count)))
         (t (setq error-count (1+ error-count))))
        
        (test-sas-comprehensive-log 'info "Test '%s': %s" test-name result)
        
        ;; Small delay between tests
        (sleep-for 2)))
    
    ;; Final cleanup
    (test-sas-comprehensive-kill-all-buffers)
    
    ;; Calculate totals
    (let ((total-count (length tests)))
      
      ;; Log summary
      (test-sas-comprehensive-log 'info "========================================")
      (test-sas-comprehensive-log 'info "COMPREHENSIVE TEST SUITE RESULTS")
      (test-sas-comprehensive-log 'info "========================================")
      (test-sas-comprehensive-log 'info "PASSED:   %d/%d" pass-count total-count)
      (test-sas-comprehensive-log 'info "FAILED:   %d/%d" fail-count total-count)
      (test-sas-comprehensive-log 'info "TIMEOUT:  %d/%d" timeout-count total-count)
      (test-sas-comprehensive-log 'info "ERROR:    %d/%d" error-count total-count)
      (test-sas-comprehensive-log 'info "TOTAL:    %d" total-count)
      
      ;; Print to minibuffer with deprecation warning
      (if (= pass-count total-count)
          (message "🎉 ALL DEPRECATED TESTS PASSED! (%d/%d) - Check %s - MIGRATE TO euporie-qrsh-integration-tests.el" 
                   pass-count total-count test-sas-comprehensive-debug-log-file)
        (message "⚠️  DEPRECATED Tests: %d PASS, %d FAIL, %d TIMEOUT, %d ERROR - Check %s - MIGRATE TO NEW SUITE" 
                 pass-count fail-count timeout-count error-count test-sas-comprehensive-debug-log-file))
      
      ;; Return detailed results
      (list pass-count fail-count (+ error-count timeout-count) total-count results))))

;;; Quick Individual Test Functions

(defun test-sas-error-fix-only ()
  "Run only the error reproduction test to verify the fix."
  (interactive)
  (test-sas-comprehensive-log 'info "Running quick error fix verification...")
  (let ((result (test-error-reproduction)))
    (message "Error fix test: %s" result)
    result))

(defun test-sas-basic-functionality ()
  "Run basic functionality tests (session + transmission)."
  (interactive)
  (test-sas-comprehensive-log 'info "Running basic functionality tests...")
  (let ((session-result (test-qrsh-establishment))
        (transmission-result (test-sas-code-transmission)))
    (message "Session: %s, Transmission: %s" session-result transmission-result)
    (list session-result transmission-result)))

;;; Debug Helper Functions

(defun test-sas-show-buffer-contents ()
  "Show contents of *euporie-sas* buffer for debugging."
  (interactive)
  (let ((buffer (get-buffer "*euporie-sas*")))
    (if buffer
        (with-current-buffer buffer
          (test-sas-comprehensive-log 'debug "Buffer *euporie-sas* contents:\n%s" (buffer-string))
          (message "Buffer contents logged to %s" test-sas-comprehensive-debug-log-file))
      (message "Buffer *euporie-sas* does not exist"))))

(defun test-sas-show-log-tail ()
  "Show tail of comprehensive test log."
  (interactive)
  (when (file-exists-p test-sas-comprehensive-debug-log-file)
    (with-temp-buffer
      (insert-file-contents test-sas-comprehensive-debug-log-file)
      (goto-char (max (point-min) (- (point-max) 2000)))
      (message "Test log tail:\n%s" (buffer-substring (point) (point-max))))))

(provide 'test-sas-comprehensive)
;;; test-sas-comprehensive.el ends here
