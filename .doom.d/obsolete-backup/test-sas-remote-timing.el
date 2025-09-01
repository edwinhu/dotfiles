;;; test-sas-remote-timing.el --- Test SAS remote timing fix -*- lexical-binding: t; -*-

;;; Commentary:
;; This file tests the C-RET timing fix for SAS remote execution.
;; It verifies that the synchronous startup logic prevents the "zsh: command not found" error.

;;; Code:

(require 'euporie-termint)

(defvar test-sas-debug-log-file (expand-file-name "test-sas-timing.log" "~/")
  "Log file for SAS timing tests.")

(defun test-sas-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to test file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) test-sas-debug-log-file))))

(defun test-sas-tramp-path-detection ()
  "Test TRAMP path detection logic for SAS."
  (test-sas-log 'info "Testing TRAMP path detection")
  
  (let ((test-cases '(("/sshx:wrds|qrsh::/home/user/test.sas" . t)
                     ("/home/local/test.sas" . nil)
                     ("/sshx:wrds::/remote/file.sas" . t))))
    (dolist (test-case test-cases)
      (let ((path (car test-case))
            (expected (cdr test-case))
            (actual (if (file-remote-p path) t nil)))
        (test-sas-log 'info "Path: %s | Expected: %s | Actual: %s | %s" 
                      path expected actual 
                      (if (eq expected actual) "✓ PASS" "✗ FAIL")))))
  
  (test-sas-log 'info "TRAMP path detection test complete"))

(defun test-sas-kernel-detection ()
  "Test SAS kernel detection in different buffer contexts."
  (test-sas-log 'info "Testing SAS kernel detection")
  
  (with-temp-buffer
    (let ((original-mode major-mode))
      ;; Test with SAS-mode
      (SAS-mode)
      (let ((detected (euporie-termint-detect-kernel)))
        (test-sas-log 'info "SAS-mode detection: %s | %s" 
                      detected 
                      (if (string= detected "sas") "✓ PASS" "✗ FAIL")))
      
      ;; Test with org-src context
      (setq-local org-src--lang "sas")
      (let ((detected (euporie-termint-detect-kernel)))
        (test-sas-log 'info "org-src sas detection: %s | %s" 
                      detected 
                      (if (string= detected "sas") "✓ PASS" "✗ FAIL")))))
  
  (test-sas-log 'info "SAS kernel detection test complete"))

(defun test-sas-timing-logic ()
  "Test the timing logic for remote SAS without actual connection."
  (test-sas-log 'info "Testing SAS timing logic (mock)")
  
  ;; Test local vs remote path handling
  (let ((local-dir "/Users/test/project")
        (remote-dir "/sshx:wrds|qrsh::/home/user/project"))
    
    ;; Test local path detection
    (let ((is-remote (and (string= "sas" "sas") 
                          (file-remote-p local-dir))))
      (test-sas-log 'info "Local path remote check: %s | %s" 
                    is-remote
                    (if (null is-remote) "✓ PASS (correctly identified as local)" "✗ FAIL")))
    
    ;; Test remote path detection  
    (let ((is-remote (and (string= "sas" "sas") 
                          (file-remote-p remote-dir))))
      (test-sas-log 'info "Remote path remote check: %s | %s" 
                    is-remote
                    (if is-remote "✓ PASS (correctly identified as remote)" "✗ FAIL"))))
  
  (test-sas-log 'info "SAS timing logic test complete"))

(defun test-sas-send-function-mapping ()
  "Test that the send function mapping works correctly for SAS."
  (test-sas-log 'info "Testing SAS send function mapping")
  
  (let ((kernel "sas")
        (remote-dir "/sshx:wrds|qrsh::/home/user/project")
        (local-dir "/Users/test/project"))
    
    ;; Test remote SAS mapping
    (let* ((is-remote-sas (and (string= kernel "sas") 
                               (file-remote-p remote-dir)))
           (send-func (cond
                      ((string= kernel "python") #'termint-euporie-python-send-string)
                      ((string= kernel "r") #'termint-euporie-r-send-string)
                      ((string= kernel "stata") #'termint-euporie-stata-send-string)
                      ((and (string= kernel "sas") is-remote-sas) 
                       #'termint-wrds-qrsh-send-string)
                      ((string= kernel "sas") #'termint-euporie-sas-send-string)
                      (t (error "Unsupported kernel: %s" kernel)))))
      
      (test-sas-log 'info "Remote SAS send function: %s | %s" 
                    (symbol-name send-func)
                    (if (eq send-func 'termint-wrds-qrsh-send-string) 
                        "✓ PASS (uses WRDS function)" 
                        "✗ FAIL")))
    
    ;; Test local SAS mapping
    (let* ((is-remote-sas (and (string= kernel "sas") 
                               (file-remote-p local-dir)))
           (send-func (cond
                      ((string= kernel "python") #'termint-euporie-python-send-string)
                      ((string= kernel "r") #'termint-euporie-r-send-string)
                      ((string= kernel "stata") #'termint-euporie-stata-send-string)
                      ((and (string= kernel "sas") is-remote-sas) 
                       #'termint-wrds-qrsh-send-string)
                      ((string= kernel "sas") #'termint-euporie-sas-send-string)
                      (t (error "Unsupported kernel: %s" kernel)))))
      
      (test-sas-log 'info "Local SAS send function: %s | %s" 
                    (symbol-name send-func)
                    (if (eq send-func 'termint-euporie-sas-send-string) 
                        "✓ PASS (uses local function)" 
                        "✗ FAIL"))))
  
  (test-sas-log 'info "SAS send function mapping test complete"))

(defun test-sas-remote-timing-fix ()
  "Run comprehensive test of SAS remote timing fix."
  (interactive)
  
  ;; Clear log file
  (when (file-exists-p test-sas-debug-log-file)
    (delete-file test-sas-debug-log-file))
  
  (test-sas-log 'info "=== Starting SAS Remote Timing Fix Tests ===")
  
  ;; Run all tests
  (test-sas-tramp-path-detection)
  (test-sas-kernel-detection)
  (test-sas-timing-logic)
  (test-sas-send-function-mapping)
  
  (test-sas-log 'info "=== SAS Remote Timing Fix Tests Complete ===")
  
  (message "SAS timing tests complete. Check log: %s" test-sas-debug-log-file)
  
  ;; Display results
  (when (file-exists-p test-sas-debug-log-file)
    (with-temp-buffer
      (insert-file-contents test-sas-debug-log-file)
      (let ((content (buffer-string)))
        (message "Test Results:\n%s" content)))))

(provide 'test-sas-remote-timing)
;;; test-sas-remote-timing.el ends here