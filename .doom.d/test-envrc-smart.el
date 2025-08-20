;;; test-envrc-smart.el --- Test smart envrc functionality

(defun test-envrc-status-parsing ()
  "Test the envrc status parsing with different scenarios."
  (interactive)
  
  ;; Test function requires the envrc functions to be defined
  (unless (fboundp 'envrc--check-direnv-status)
    (error "envrc--check-direnv-status function not defined. Load config.el first."))
  
  (let ((test-dir "/tmp/envrc-test-smart"))
    ;; Create test directory with .envrc
    (unless (file-directory-p test-dir)
      (make-directory test-dir t))
    
    (let ((envrc-file (expand-file-name ".envrc" test-dir)))
      (with-temp-file envrc-file
        (insert "export TEST_VAR=hello\n"))
      
      ;; Test status detection
      (message "Testing envrc smart status detection...")
      
      ;; Test 1: Should detect blocked/unallowed .envrc
      (let ((status (envrc--check-direnv-status test-dir)))
        (message "Status for new .envrc: %s" status)
        (unless (memq status '(blocked denied none))
          (message "WARNING: Expected blocked/denied/none, got %s" status)))
      
      ;; Cleanup
      (delete-directory test-dir t)
      (message "✓ envrc smart status test completed"))))

(defun test-envrc-configuration ()
  "Test that the envrc configuration is properly set up."
  (interactive)
  (message "Testing envrc configuration...")
  
  ;; Check if the custom variable exists
  (if (boundp 'envrc-smart-allow-checking)
      (message "✓ envrc-smart-allow-checking variable: %s" envrc-smart-allow-checking)
    (message "✗ envrc-smart-allow-checking not defined"))
  
  ;; Check if the custom functions exist
  (if (fboundp 'envrc--check-direnv-status)
      (message "✓ envrc--check-direnv-status function defined")
    (message "✗ envrc--check-direnv-status not defined"))
  
  (if (fboundp 'envrc--smart-export)
      (message "✓ envrc--smart-export function defined")
    (message "✗ envrc--smart-export not defined"))
  
  ;; Check if advice is installed
  (if (advice-member-p 'envrc--smart-export 'envrc--export)
      (message "✓ envrc--smart-export advice installed")
    (message "✗ envrc--smart-export advice not installed"))
  
  (message "envrc configuration test completed"))

(provide 'test-envrc-smart)
;;; test-envrc-smart.el ends here