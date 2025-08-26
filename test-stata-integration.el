;; Test script for Stata jupyter-termint integration
;; This script tests the complete C-RET functionality for Stata

(defvar stata-test-log-file (expand-file-name "stata-integration-test.log" "~/"))

(defun stata-test-log (level format-string &rest args)
  "Log test results with timestamp."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) stata-test-log-file))))

(defun test-stata-integration ()
  "Complete test of Stata jupyter-termint integration."
  (interactive)
  
  ;; Clear previous log
  (when (file-exists-p stata-test-log-file)
    (delete-file stata-test-log-file))
  
  (stata-test-log 'info "=== STATA JUPYTER-TERMINT INTEGRATION TEST START ===")
  
  ;; Test 1: Function availability
  (stata-test-log 'info "TEST 1: Function availability")
  (let ((functions '(jupyter-stata jupyter-termint-setup termint-jupyter-stata-start smart-org-src-send)))
    (dolist (func functions)
      (if (fboundp func)
          (stata-test-log 'info "  ✓ %s: available" func)
        (stata-test-log 'error "  ✗ %s: missing" func))))
  
  ;; Test 2: Create a test org file with Stata code
  (stata-test-log 'info "TEST 2: Creating test org file")
  (let ((test-file "/tmp/stata-test.org"))
    (with-temp-file test-file
      (insert "#+title: Stata Integration Test\n\n")
      (insert "* Test Stata Code Block\n\n")
      (insert "#+begin_src stata\n")
      (insert "display \"Hello from Stata!\"\n")
      (insert "#+end_src\n"))
    
    (stata-test-log 'info "  ✓ Test file created: %s" test-file)
    
    ;; Test 3: Open file and test C-RET
    (find-file test-file)
    (goto-char (point-min))
    (search-forward "display")
    
    ;; Test org-src mode entry
    (org-edit-src-code)
    (stata-test-log 'info "TEST 3: Entered org-src mode")
    (stata-test-log 'info "  Buffer name: %s" (buffer-name))
    (stata-test-log 'info "  Major mode: %s" major-mode)
    (stata-test-log 'info "  org-src--lang: %s" (bound-and-true-p org-src--lang))
    
    ;; Test kernel detection
    (when (fboundp 'jupyter-termint-detect-kernel)
      (let ((detected-kernel (jupyter-termint-detect-kernel)))
        (stata-test-log 'info "  Detected kernel: %s" detected-kernel)))
    
    ;; Test C-RET functionality - simulate the process
    (stata-test-log 'info "TEST 4: Simulating C-RET functionality")
    (condition-case err
        (progn
          ;; Test the smart-org-src-send function directly
          (when (fboundp 'smart-org-src-send)
            (stata-test-log 'info "  Calling smart-org-src-send...")
            (smart-org-src-send)
            (stata-test-log 'info "  ✓ smart-org-src-send completed without errors"))
          
          ;; Check if jupyter-stata buffer was created
          (sleep-for 3) ; Give time for buffer creation
          (if (get-buffer "*jupyter-stata*")
              (stata-test-log 'info "  ✓ *jupyter-stata* buffer created successfully")
            (stata-test-log 'warn "  ⚠ *jupyter-stata* buffer not found yet"))
          
          ;; Test direct jupyter-stata function call
          (stata-test-log 'info "TEST 5: Direct jupyter-stata function test")
          (when (fboundp 'jupyter-stata)
            (jupyter-stata)
            (sleep-for 5) ; Give more time for console startup
            
            (if (get-buffer "*jupyter-stata*")
                (progn
                  (stata-test-log 'info "  ✓ jupyter-stata function created buffer successfully")
                  (with-current-buffer "*jupyter-stata*"
                    (stata-test-log 'info "  Buffer mode: %s" major-mode)
                    (stata-test-log 'info "  Buffer has process: %s" 
                                   (if (get-buffer-process (current-buffer)) "yes" "no"))
                    (stata-test-log 'info "  Buffer size: %d characters" (buffer-size))))
              (stata-test-log 'error "  ✗ jupyter-stata function did not create buffer"))))
      (error 
       (stata-test-log 'error "  ✗ Error during C-RET test: %s" err)))
    
    ;; Test 6: Window layout
    (stata-test-log 'info "TEST 6: Window layout verification")
    (let ((windows (mapcar (lambda (w) 
                            (buffer-name (window-buffer w)))
                          (window-list))))
      (stata-test-log 'info "  Active windows: %s" windows))
    
    ;; Cleanup
    (when (get-buffer "*jupyter-stata*")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer "*jupyter-stata*"))
      (stata-test-log 'info "  Cleaned up *jupyter-stata* buffer"))
    
    (when (get-buffer (file-name-nondirectory test-file))
      (kill-buffer (file-name-nondirectory test-file)))
    
    (stata-test-log 'info "=== TEST COMPLETED ===")
    (stata-test-log 'info "Check log file: %s" stata-test-log-file)
    
    ;; Show results
    (message "Stata integration test completed. Check %s for results." stata-test-log-file)
    (find-file stata-test-log-file)))

;; Run the test
(test-stata-integration)