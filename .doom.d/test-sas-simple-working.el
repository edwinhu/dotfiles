;;; test-sas-simple-working.el --- Simple Working SAS Test -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple working SAS test that verifies cars data output
;; Executes: proc print data=sashelp.cars(obs=5);run;
;; Checks for expected output patterns

;;; Code:

(require 'euporie-termint nil t)

(defvar sas-test-results-file (expand-file-name "sas-test-results.log" "~/"))

(defun sas-test-log (level message)
  "Log MESSAGE at LEVEL to test results file."
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S EST"))
        (entry (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message)))
    (with-temp-buffer
      (insert entry)
      (append-to-file (point-min) (point-max) sas-test-results-file))))

(defun test-sas-output-verification ()
  "Test SAS output verification function.
Executes SAS code and checks for expected cars table output."
  (interactive)
  
  (sas-test-log 'info "=== Starting SAS Output Verification Test ===")
  
  ;; Clear log
  (with-temp-buffer
    (insert (format "=== SAS OUTPUT VERIFICATION TEST - %s ===\n\n" 
                    (format-time-string "%Y-%m-%d %H:%M:%S EST")))
    (write-region (point-min) (point-max) sas-test-results-file nil 'silent))
  
  ;; Clean up buffers
  (dolist (buf '("*euporie-sas*" "*wrds-qrsh*"))
    (when (get-buffer buf)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))))
  
  (let ((test-code "proc print data=sashelp.cars(obs=5);run;")
        (results '())
        (passed-tests 0)
        (total-tests 0))
    
    (sas-test-log 'info (format "Test code: %s" test-code))
    
    (condition-case err
        (progn
          ;; Start SAS
          (sas-test-log 'info "Starting SAS session...")
          (let ((buffer (euporie-sas-start)))
            
            (if buffer
                (progn
                  (sas-test-log 'info (format "SAS buffer created: %s" (buffer-name buffer)))
                  (sleep-for 3)  ; Wait for initialization
                  
                  ;; Send code
                  (sas-test-log 'info "Sending test code...")
                  (euporie-termint-send-code "sas" test-code)
                  (sleep-for 5)  ; Wait for execution
                  
                  ;; Check results
                  (with-current-buffer buffer
                    (let ((content (buffer-string)))
                      (sas-test-log 'debug (format "Buffer content (%d chars)" (length content)))
                      
                      ;; Test 1: Connection established
                      (setq total-tests (1+ total-tests))
                      (if (or (string-match-p "SAS Connection established" content)
                              (string-match-p "Welcome to SAS" content)
                              (string-match-p "subprocess.*sas" content))
                          (progn
                            (sas-test-log 'info "✓ SAS connection detected")
                            (setq passed-tests (1+ passed-tests))
                            (push '(connection . pass) results))
                        (progn
                          (sas-test-log 'error "✗ SAS connection NOT detected")
                          (push '(connection . fail) results)))
                      
                      ;; Test 2: Column headers present  
                      (setq total-tests (1+ total-tests))
                      (let ((columns '("Obs" "Make" "Model" "MSRP"))
                            (found 0))
                        (dolist (col columns)
                          (when (string-match-p col content)
                            (setq found (1+ found))))
                        (if (>= found 3)
                            (progn
                              (sas-test-log 'info (format "✓ Column headers found (%d/4)" found))
                              (setq passed-tests (1+ passed-tests))
                              (push '(columns . pass) results))
                          (progn
                            (sas-test-log 'error (format "✗ Insufficient columns (%d/4)" found))
                            (push '(columns . fail) results))))
                      
                      ;; Test 3: Sample data (Acura)
                      (setq total-tests (1+ total-tests))
                      (if (string-match-p "Acura" content)
                          (progn
                            (sas-test-log 'info "✓ Sample data (Acura) found")
                            (setq passed-tests (1+ passed-tests))
                            (push '(data . pass) results))
                        (progn
                          (sas-test-log 'error "✗ Sample data (Acura) NOT found")
                          (push '(data . fail) results)))
                      
                      ;; Test 4: No critical errors
                      (setq total-tests (1+ total-tests))
                      (if (string-match-p "ERROR\\|Error.*failed" content)
                          (progn
                            (sas-test-log 'error "✗ Errors found in output")
                            (push '(no-errors . fail) results))
                        (progn
                          (sas-test-log 'info "✓ No critical errors")
                          (setq passed-tests (1+ passed-tests))
                          (push '(no-errors . pass) results))))))
              (progn
                (sas-test-log 'error "Failed to create SAS buffer")
                (setq total-tests 1)
                (push '(buffer-creation . fail) results)))))
      
      (error
       (sas-test-log 'error (format "Test failed: %s" err))
       (setq total-tests 1)
       (push '(test-execution . error) results)))
    
    ;; Final results
    (let ((overall-pass (= passed-tests total-tests)))
      (sas-test-log 'info (format "=== Test Complete: %s (%d/%d passed) ===" 
                                  (if overall-pass "PASSED" "FAILED")
                                  passed-tests total-tests))
      (message "SAS Output Verification: %s (%d/%d tests passed)" 
               (if overall-pass "PASSED" "FAILED") passed-tests total-tests)
      
      ;; Return structured results
      `((overall . ,(if overall-pass 'pass 'fail))
        (passed . ,passed-tests)
        (total . ,total-tests)
        (results . ,(reverse results))
        (timestamp . ,(current-time-string))))))

(provide 'test-sas-simple-working)
;;; test-sas-simple-working.el ends here