;;; reproduce-error.el --- Automated Error Reproduction for SAS Integration

;; EUPORIE-DEVELOPER AGENT - ERROR REPRODUCTION SYSTEM
;; Created: 2025-01-17 (EST)
;; Purpose: Reproduce "Invalid function: (eq remote-mode 'standard-tramp)" error

(require 'euporie-termint)

;;; Configuration

(defvar reproduce-error-log-file 
  (expand-file-name "reproduce-error.log" "~/")
  "Log file for error reproduction attempts.")

(defvar reproduce-error-test-scenarios
  '(("/sshx:wrds-cloud:/home/nyu/eddyhu/project/|qrsh::/home/nyu/eddyhu/project/" . "QRSH TRAMP")
    ("/sshx:wrds-cloud:/home/nyu/eddyhu/project/" . "Standard TRAMP")
    (nil . "Nil directory")
    ("" . "Empty string directory")
    ("/invalid/path" . "Invalid path"))
  "Test scenarios that might trigger the error.")

;;; Logging System

(defun reproduce-error-log (level format-string &rest args)
  "Log message with timestamp to error reproduction log file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S EST")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) reproduce-error-log-file))))

;;; Error Reproduction Functions

(defun reproduce-error-test-scenario (test-dir description)
  "Test a specific directory scenario that might trigger the error.
Returns: 'SUCCESS if no error, 'TARGET-ERROR if target error found, 'OTHER-ERROR if different error."
  (reproduce-error-log 'info "Testing scenario: %s - %s" description test-dir)
  
  ;; Clean up first
  (dolist (buf-name '("*euporie-sas*" "*qrsh-session*" "*euporie-sas-remote*"))
    (when (get-buffer buf-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf-name))))
  
  (condition-case err
      (progn
        ;; Try the operation that was failing
        (reproduce-error-log 'debug "Attempting euporie-termint-send-code with dir: %s" test-dir)
        (euporie-termint-send-code "sas" "data _null_; put 'test'; run;" test-dir)
        
        ;; If we get here, no error occurred
        (reproduce-error-log 'info "✓ No error occurred for scenario: %s" description)
        'SUCCESS)
    
    ;; Check for the specific error we're hunting
    ((error invalid-function)
     (let ((error-msg (format "%s" err)))
       (reproduce-error-log 'error "Error occurred: %s" error-msg)
       (if (string-match-p "Invalid function.*eq.*remote-mode.*standard-tramp" error-msg)
           (progn
             (reproduce-error-log 'error "🎯 TARGET ERROR REPRODUCED: %s" error-msg)
             'TARGET-ERROR)
         (progn
           (reproduce-error-log 'error "Different error: %s" error-msg)
           'OTHER-ERROR))))
    
    (error
     (reproduce-error-log 'error "Other error type: %s" err)
     'OTHER-ERROR)))

(defun reproduce-error-comprehensive ()
  "Run comprehensive error reproduction across all scenarios.
Returns: summary of results."
  (interactive)
  (reproduce-error-log 'info "========================================")
  (reproduce-error-log 'info "COMPREHENSIVE ERROR REPRODUCTION TEST")
  (reproduce-error-log 'info "Timestamp: %s" (format-time-string "%Y-%m-%d %H:%M:%S EST"))
  (reproduce-error-log 'info "========================================")
  
  ;; Clear previous log
  (with-temp-buffer
    (insert (format "=== ERROR REPRODUCTION LOG - %s ===\n\n" 
                    (format-time-string "%Y-%m-%d %H:%M:%S EST")))
    (write-region (point-min) (point-max) reproduce-error-log-file nil 'silent))
  
  (let ((results '())
        (success-count 0)
        (target-error-count 0)
        (other-error-count 0))
    
    ;; Test each scenario
    (dolist (scenario reproduce-error-test-scenarios)
      (let* ((test-dir (car scenario))
             (description (cdr scenario))
             (result (reproduce-error-test-scenario test-dir description)))
        
        (push (list description result) results)
        
        (cond
         ((eq result 'SUCCESS) (setq success-count (1+ success-count)))
         ((eq result 'TARGET-ERROR) (setq target-error-count (1+ target-error-count)))
         (t (setq other-error-count (1+ other-error-count))))
        
        ;; Small delay between tests
        (sleep-for 1)))
    
    ;; Summary
    (reproduce-error-log 'info "========================================")
    (reproduce-error-log 'info "ERROR REPRODUCTION RESULTS")
    (reproduce-error-log 'info "========================================")
    (reproduce-error-log 'info "SUCCESS (no error):     %d" success-count)
    (reproduce-error-log 'info "TARGET ERROR found:     %d" target-error-count)
    (reproduce-error-log 'info "OTHER ERRORS:           %d" other-error-count)
    (reproduce-error-log 'info "TOTAL SCENARIOS:        %d" (length reproduce-error-test-scenarios))
    
    ;; Detailed results
    (dolist (result results)
      (reproduce-error-log 'info "  %s: %s" (car result) (cadr result)))
    
    ;; Print summary
    (if (> target-error-count 0)
        (message "🔍 TARGET ERROR REPRODUCED in %d scenarios! Check %s" 
                 target-error-count reproduce-error-log-file)
      (message "✅ No target errors found (%d success, %d other errors). Check %s" 
               success-count other-error-count reproduce-error-log-file))
    
    ;; Return results
    (list :success success-count 
          :target-error target-error-count 
          :other-error other-error-count 
          :details results)))

(defun reproduce-error-quick-test ()
  "Quick test with the most likely scenario to trigger the error."
  (interactive)
  (reproduce-error-log 'info "Running quick error reproduction test...")
  (let ((result (reproduce-error-test-scenario 
                 "/sshx:wrds-cloud:/home/nyu/eddyhu/project/|qrsh::/home/nyu/eddyhu/project/" 
                 "QRSH TRAMP (most likely)")))
    (message "Quick test result: %s" result)
    result))

;;; Debug Functions

(defun reproduce-error-test-remote-detection (test-dir)
  "Test remote detection functions directly on TEST-DIR.
This helps isolate where the error might be occurring."
  (interactive "sTest directory: ")
  (reproduce-error-log 'info "=== TESTING REMOTE DETECTION FOR: %s ===" test-dir)
  
  (condition-case err
      (progn
        ;; Test each step of remote detection
        (reproduce-error-log 'debug "Step 1: Testing file-remote-p")
        (let ((remote-p (file-remote-p test-dir)))
          (reproduce-error-log 'debug "  file-remote-p result: %s" remote-p))
        
        (reproduce-error-log 'debug "Step 2: Testing euporie-termint--detect-remote-mode")
        (let ((remote-mode (euporie-termint--detect-remote-mode test-dir)))
          (reproduce-error-log 'debug "  detect-remote-mode result: %s" remote-mode))
        
        (reproduce-error-log 'debug "Step 3: Testing euporie-termint--get-remote-config")
        (let ((remote-config (euporie-termint--get-remote-config "sas" test-dir)))
          (reproduce-error-log 'debug "  get-remote-config result: %s" remote-config)
          (when remote-config
            (reproduce-error-log 'debug "  remote-mode from config: %s" (car remote-config))
            (reproduce-error-log 'debug "  connection-info: %s" (cdr remote-config))))
        
        (reproduce-error-log 'info "✓ Remote detection test completed successfully")
        'SUCCESS)
    
    (error
     (reproduce-error-log 'error "❌ Remote detection test failed: %s" err)
     'ERROR)))

(defun reproduce-error-show-log ()
  "Display the error reproduction log."
  (interactive)
  (if (file-exists-p reproduce-error-log-file)
      (find-file reproduce-error-log-file)
    (message "No error reproduction log found at %s" reproduce-error-log-file)))

(provide 'reproduce-error)
;;; reproduce-error.el ends here