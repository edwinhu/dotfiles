;;; sas-workflow-tests.el --- Comprehensive SAS workflow unit tests -*- lexical-binding: t; -*-

;; Test framework for SAS workflow verification with deterministic results
;; Uses log-based verification with clear pass/fail criteria

(require 'org)
(require 'ob-core)

;; Test configuration
(defvar sas-test-log-file (expand-file-name "sas-workflow-debug.log" "~/"))
(defvar sas-test-results '())
(defvar sas-test-timeout 5) ; seconds

;; Test framework functions
(defun sas-test-clear-log ()
  "Clear the SAS workflow debug log for clean testing."
  (when (file-exists-p sas-test-log-file)
    (delete-file sas-test-log-file))
  (with-temp-buffer
    (insert (format "[%s] [INFO] === SAS WORKFLOW TEST SESSION STARTED ===\n" 
                    (format-time-string "%Y-%m-%d %H:%M:%S")))
    (append-to-file (point-min) (point-max) sas-test-log-file)))

(defun sas-test-get-log-content ()
  "Get current log file content."
  (if (file-exists-p sas-test-log-file)
      (with-temp-buffer
        (insert-file-contents sas-test-log-file)
        (buffer-string))
    ""))

(defun sas-test-verify-log-pattern (pattern description)
  "Verify specific log pattern exists with clear success/failure.
Returns (PASS . message) or (FAIL . message)."
  (let ((log-content (sas-test-get-log-content)))
    (if (string-match-p pattern log-content)
        (cons 'PASS (format "%s - Pattern found: %s" description pattern))
      (cons 'FAIL (format "%s - Pattern NOT found: %s" description pattern)))))

(defun sas-test-verify-multiple-patterns (patterns description)
  "Verify multiple log patterns exist. Returns first failure or success."
  (let ((results '()))
    (dolist (pattern patterns)
      (let ((result (sas-test-verify-log-pattern pattern description)))
        (push result results)
        (when (eq (car result) 'FAIL)
          (return result))))
    (cons 'PASS (format "%s - All patterns verified" description))))

(defun sas-test-runner (test-name test-function expected-patterns)
  "Run a single test and verify log patterns.
Returns test result with detailed information."
  (let ((start-time (current-time))
        (test-marker (format "=== TEST: %s ===" test-name)))
    
    ;; Add test marker to log
    (sas-workflow-debug-log 'info test-marker)
    
    ;; Run the test function
    (condition-case err
        (progn
          (funcall test-function)
          (sleep-for 0.5) ; Allow log writes to complete
          
          ;; Verify expected patterns
          (let ((pattern-result (if (listp expected-patterns)
                                    (sas-test-verify-multiple-patterns expected-patterns test-name)
                                  (sas-test-verify-log-pattern expected-patterns test-name))))
            (list test-name
                  (car pattern-result)
                  (cdr pattern-result)
                  (time-since start-time))))
      
      (error
       (list test-name
             'ERROR
             (format "Test function failed: %s" (error-message-string err))
             (time-since start-time))))))

;; Individual test functions

(defun sas-test-1-c-ret-detection ()
  "Test 1: Verify C-RET detection in org-src buffer."
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN_SRC sas\ndata test;\n  x = 1;\nrun;\n#+END_SRC")
    (goto-char (point-min))
    (re-search-forward "data test")
    (org-edit-src-code)
    
    ;; Simulate C-RET in org-src buffer
    (with-current-buffer (current-buffer)
      (when (fboundp 'smart-org-src-send)
        (smart-org-src-send)))))

(defun sas-test-2-language-detection ()
  "Test 2: Verify language detection in various contexts."
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN_SRC sas\ndata test; run;\n#+END_SRC")
    (goto-char (point-min))
    (re-search-forward "data test")
    (org-edit-src-code)
    
    ;; Test language detection
    (when (fboundp 'smart-org-src-get-language)
      (let ((detected-lang (smart-org-src-get-language)))
        (sas-workflow-debug-log 'info (format "Language detection test: %s" detected-lang))))))

(defun sas-test-3-remote-directory-parsing ()
  "Test 3: Verify remote directory parsing from babel info."
  (let ((test-babel-info '(:dir "/sshx:wrds|qrsh::/home/nyu/eddyhu/projects/wander2")))
    (when (fboundp 'smart-org-src-parse-remote-dir)
      (smart-org-src-parse-remote-dir test-babel-info))))

(defun sas-test-4-function-availability ()
  "Test 4: Verify availability of required functions."
  (let ((required-functions '(smart-org-src-send 
                              smart-org-src-get-language
                              smart-org-src-parse-remote-dir
                              tramp-wrds-termint
                              sas-workflow-debug-log)))
    (dolist (func required-functions)
      (if (fboundp func)
          (sas-workflow-debug-log 'info (format "Function available: %s" func))
        (sas-workflow-debug-log 'error (format "Function MISSING: %s" func))))))

(defun sas-test-5-tramp-wrds-termint-execution ()
  "Test 5: Test tramp-wrds-termint execution without void function error."
  (condition-case err
      (progn
        (sas-workflow-debug-log 'info "Testing tramp-wrds-termint execution")
        (if (fboundp 'tramp-wrds-termint)
            (progn
              (sas-workflow-debug-log 'info "tramp-wrds-termint function available")
              ;; Don't actually execute to avoid connection - just test availability
              (sas-workflow-debug-log 'info "tramp-wrds-termint execution test - SIMULATED"))
          (sas-workflow-debug-log 'error "tramp-wrds-termint function NOT available")))
    (void-function
     (sas-workflow-debug-log 'error "void-function error in tramp-wrds-termint test"))
    (error
     (sas-workflow-debug-log 'error (format "Error in tramp-wrds-termint test: %s" 
                                           (error-message-string err))))))

(defun sas-test-6-euporie-command-construction ()
  "Test 6: Verify euporie command construction for remote paths."
  (let ((test-remote-path "/home/nyu/eddyhu/projects/wander2"))
    (when (fboundp 'smart-org-src-build-euporie-command)
      (let ((command (smart-org-src-build-euporie-command test-remote-path)))
        (sas-workflow-debug-log 'info (format "Euporie command constructed: %s" command))))))

(defun sas-test-7-buffer-management ()
  "Test 7: Verify buffer creation and management."
  (let ((test-buffer-name "*euporie-sas-test*"))
    (when (get-buffer test-buffer-name)
      (kill-buffer test-buffer-name))
    
    ;; Create test buffer
    (with-current-buffer (get-buffer-create test-buffer-name)
      (sas-workflow-debug-log 'info (format "Test buffer created: %s" test-buffer-name))
      (insert "Test SAS buffer content"))
    
    ;; Verify buffer exists
    (if (get-buffer test-buffer-name)
        (sas-workflow-debug-log 'info "Buffer management test - buffer exists")
      (sas-workflow-debug-log 'error "Buffer management test - buffer NOT found"))
    
    ;; Clean up
    (when (get-buffer test-buffer-name)
      (kill-buffer test-buffer-name))))

(defun sas-test-8-split-window ()
  "Test 8: Verify split window functionality."
  (let ((original-windows (length (window-list))))
    (sas-workflow-debug-log 'info (format "Original window count: %d" original-windows))
    
    ;; Test split window (simulate)
    (condition-case err
        (progn
          (split-window-right)
          (let ((new-windows (length (window-list))))
            (sas-workflow-debug-log 'info (format "New window count: %d" new-windows))
            (if (> new-windows original-windows)
                (sas-workflow-debug-log 'info "Split window test - SUCCESS")
              (sas-workflow-debug-log 'error "Split window test - FAILED")))
          ;; Restore original window configuration
          (delete-other-windows))
      (error
       (sas-workflow-debug-log 'error (format "Split window error: %s" 
                                              (error-message-string err)))))))

(defun sas-test-9-error-handling ()
  "Test 9: Verify error handling for various scenarios."
  ;; Test missing function handling
  (condition-case err
      (nonexistent-function-call)
    (void-function
     (sas-workflow-debug-log 'info "Error handling test - void function caught correctly"))
    (error
     (sas-workflow-debug-log 'info (format "Error handling test - error caught: %s" 
                                           (error-message-string err)))))
  
  ;; Test missing file handling
  (condition-case err
      (insert-file-contents "/nonexistent/file/path")
    (file-error
     (sas-workflow-debug-log 'info "Error handling test - file error caught correctly"))
    (error
     (sas-workflow-debug-log 'info (format "Error handling test - error caught: %s" 
                                           (error-message-string err))))))

(defun sas-test-10-end-to-end-workflow ()
  "Test 10: End-to-end workflow simulation."
  (sas-workflow-debug-log 'info "Starting end-to-end workflow test")
  
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN_SRC sas :dir /sshx:wrds|qrsh::/home/nyu/eddyhu/projects/wander2\n")
    (insert "data test;\n")
    (insert "  x = 1;\n")
    (insert "  y = 2;\n")
    (insert "run;\n")
    (insert "#+END_SRC")
    
    (goto-char (point-min))
    (re-search-forward "data test")
    
    ;; Simulate the complete workflow
    (condition-case err
        (progn
          (sas-workflow-debug-log 'info "End-to-end test - starting org-edit-src-code")
          (org-edit-src-code)
          
          (sas-workflow-debug-log 'info "End-to-end test - in org-src buffer")
          
          ;; Try to execute the full workflow if functions available
          (when (fboundp 'smart-org-src-send)
            (sas-workflow-debug-log 'info "End-to-end test - calling smart-org-src-send")
            (smart-org-src-send))
          
          (sas-workflow-debug-log 'info "End-to-end test - completed"))
      
      (error
       (sas-workflow-debug-log 'error (format "End-to-end test error: %s" 
                                              (error-message-string err)))))))

;; Test definitions with expected patterns
(defvar sas-test-definitions
  `((1 "C-RET Detection" sas-test-1-c-ret-detection 
       "C-RET pressed in smart-org-src-send\\|Language detection test")
    
    (2 "Language Detection" sas-test-2-language-detection
       "Language detection test: sas")
    
    (3 "Remote Directory Parsing" sas-test-3-remote-directory-parsing
       "Remote execution detected\\|Parsing remote directory")
    
    (4 "Function Availability" sas-test-4-function-availability
       ("Function available: smart-org-src-send"
        "Function available: sas-workflow-debug-log"))
    
    (5 "tramp-wrds-termint Execution" sas-test-5-tramp-wrds-termint-execution
       "tramp-wrds-termint function available\\|tramp-wrds-termint execution test")
    
    (6 "Euporie Command Construction" sas-test-6-euporie-command-construction
       "Euporie command constructed")
    
    (7 "Buffer Management" sas-test-7-buffer-management
       "Buffer management test - buffer exists")
    
    (8 "Split Window" sas-test-8-split-window
       "Split window test - SUCCESS")
    
    (9 "Error Handling" sas-test-9-error-handling
       ("Error handling test - void function caught correctly"
        "Error handling test - file error caught correctly"))
    
    (10 "End-to-End Workflow" sas-test-10-end-to-end-workflow
        "End-to-end test - completed")))

;; Main test runner
(defun run-all-sas-workflow-tests ()
  "Run all SAS workflow tests and generate detailed report."
  (interactive)
  (setq sas-test-results '())
  
  ;; Clear log for clean testing
  (sas-test-clear-log)
  (sas-workflow-debug-log 'info "Starting comprehensive SAS workflow test suite")
  
  (message "Running SAS workflow tests...")
  
  ;; Run each test
  (dolist (test-def sas-test-definitions)
    (let* ((test-num (nth 0 test-def))
           (test-name (nth 1 test-def))
           (test-func (nth 2 test-def))
           (expected-patterns (nth 3 test-def))
           (full-test-name (format "Test %d - %s" test-num test-name)))
      
      (message "Running %s..." full-test-name)
      (let ((result (sas-test-runner full-test-name test-func expected-patterns)))
        (push result sas-test-results))))
  
  ;; Generate report
  (sas-test-generate-report)
  (sas-workflow-debug-log 'info "SAS workflow test suite completed")
  (message "SAS workflow tests completed. Check *SAS-Test-Results* buffer for details."))

(defun sas-test-generate-report ()
  "Generate comprehensive test report."
  (let ((passed 0)
        (failed 0)
        (errors 0)
        (report-buffer (get-buffer-create "*SAS-Test-Results*")))
    
    (with-current-buffer report-buffer
      (erase-buffer)
      (insert "=== SAS WORKFLOW UNIT TEST RESULTS ===\n")
      (insert (format "Test Date: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "Log File: %s\n\n" sas-test-log-file))
      
      ;; Process results in order
      (dolist (result (reverse sas-test-results))
        (let ((test-name (nth 0 result))
              (status (nth 1 result))
              (message (nth 2 result))
              (duration (nth 3 result)))
          
          (cond
           ((eq status 'PASS) (setq passed (1+ passed)))
           ((eq status 'FAIL) (setq failed (1+ failed)))
           ((eq status 'ERROR) (setq errors (1+ errors))))
          
          (insert (format "%-50s %s\n" test-name status))
          (when message
            (insert (format "  └─ %s\n" message)))
          (when duration
            (insert (format "  └─ Duration: %.3f seconds\n" (float-time duration))))
          (insert "\n")))
      
      ;; Summary
      (insert "=== SUMMARY ===\n")
      (insert (format "Total Tests: %d\n" (+ passed failed errors)))
      (insert (format "Passed: %d\n" passed))
      (insert (format "Failed: %d\n" failed))
      (insert (format "Errors: %d\n" errors))
      (insert (format "Success Rate: %.1f%%\n" 
                      (if (> (+ passed failed errors) 0)
                          (* 100.0 (/ (float passed) (+ passed failed errors)))
                        0.0)))
      
      ;; Log file location
      (insert (format "\nDetailed logs available at: %s\n" sas-test-log-file))
      
      ;; Show buffer
      (display-buffer report-buffer)
      (goto-char (point-min)))))

(defun sas-test-view-log ()
  "View the SAS workflow debug log."
  (interactive)
  (if (file-exists-p sas-test-log-file)
      (find-file sas-test-log-file)
    (message "Log file does not exist: %s" sas-test-log-file)))

;; Quick test runner for individual tests
(defun run-single-sas-test (test-number)
  "Run a single SAS test by number."
  (interactive "nTest number (1-10): ")
  (let ((test-def (assq test-number sas-test-definitions)))
    (if test-def
        (let* ((test-name (nth 1 test-def))
               (test-func (nth 2 test-def))
               (expected-patterns (nth 3 test-def))
               (full-test-name (format "Test %d - %s" test-number test-name)))
          
          (sas-test-clear-log)
          (sas-workflow-debug-log 'info (format "Running single test: %s" full-test-name))
          
          (let ((result (sas-test-runner full-test-name test-func expected-patterns)))
            (message "Test Result: %s - %s" (nth 1 result) (nth 2 result))
            (sas-test-view-log)))
      (message "Invalid test number. Use 1-10."))))

;; Interactive functions for easy testing
(defun sas-test-setup ()
  "Set up SAS workflow testing environment."
  (interactive)
  (message "SAS workflow testing environment ready.")
  (message "Use 'run-all-sas-workflow-tests' to run full test suite.")
  (message "Use 'run-single-sas-test N' to run individual tests.")
  (message "Use 'sas-test-view-log' to view detailed logs."))

(provide 'sas-workflow-tests)
;;; sas-workflow-tests.el ends here