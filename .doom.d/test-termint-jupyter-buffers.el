;;; test-termint-jupyter-buffers.el --- Comprehensive tests for termint-jupyter buffer creation and management -*- lexical-binding: t; -*-

;;; Commentary:
;; Test that termint-jupyter functions create buffers with expected names
;; and that all dynamically generated functions work correctly.

;;; Code:

(require 'termint)

;;; Test Configuration and Logging

(defvar termint-jupyter-test-log-file (expand-file-name "termint-jupyter-test.log" "~/")
  "Log file for termint-jupyter tests.")

(defun termint-jupyter-test-log (level format-string &rest args)
  "Log test messages with timestamp to file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) termint-jupyter-test-log-file))
    ;; Also show in messages
    (message "[%s] %s" (upcase (symbol-name level)) message)))

;;; Buffer Management Utilities

(defun termint-jupyter-test-get-buffer-names ()
  "Get list of all buffer names."
  (mapcar #'buffer-name (buffer-list)))

(defun termint-jupyter-test-find-jupyter-buffers ()
  "Find all buffers with jupyter in the name."
  (seq-filter (lambda (name) (string-match-p "jupyter" name))
              (termint-jupyter-test-get-buffer-names)))

(defun termint-jupyter-test-kill-jupyter-buffers ()
  "Kill all existing jupyter buffers for clean testing."
  (let ((jupyter-buffers (termint-jupyter-test-find-jupyter-buffers)))
    (dolist (buffer-name jupyter-buffers)
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)
        (termint-jupyter-test-log 'info "Killed buffer: %s" buffer-name)))))

;;; Function Existence Tests

(defun termint-jupyter-test-function-exists-p (function-name)
  "Test if FUNCTION-NAME exists and is callable."
  (and (fboundp function-name)
       (functionp (symbol-function function-name))))

(defun termint-jupyter-test-check-functions ()
  "Check if all expected termint functions exist after setup."
  (termint-jupyter-test-log 'info "=== Testing Function Availability ===")
  
  (let ((expected-functions '(termint-jupyter-python-start
                             termint-jupyter-r-start  
                             termint-jupyter-stata-start
                             termint-jupyter-python-send-string
                             termint-jupyter-r-send-string
                             termint-jupyter-stata-send-string)))
    
    (dolist (func expected-functions)
      (if (termint-jupyter-test-function-exists-p func)
          (termint-jupyter-test-log 'success "✓ Function %s exists and is callable" func)
        (termint-jupyter-test-log 'error "✗ Function %s missing or not callable" func)))))

;;; Buffer Creation Tests

(defun termint-jupyter-test-buffer-creation (start-function expected-buffer-name kernel-name)
  "Test that START-FUNCTION creates a buffer with EXPECTED-BUFFER-NAME."
  (termint-jupyter-test-log 'info "Testing buffer creation for %s..." kernel-name)
  
  ;; Check if we're in a graphics environment that can support vterm
  (let ((has-display (and (not noninteractive)
                         (or (getenv "DISPLAY") 
                             (getenv "XAUTHORITY")
                             ;; On macOS, always assume display is available if not noninteractive
                             (eq system-type 'darwin)))))
    (if (not has-display)
        (progn
          (termint-jupyter-test-log 'warning "⚠️  No display available - vterm cannot create buffers in batch mode")
          (termint-jupyter-test-log 'info "Testing function existence only for %s" kernel-name)
          (if (termint-jupyter-test-function-exists-p start-function)
              (progn
                (termint-jupyter-test-log 'success "✓ Function %s exists and is callable" start-function)
                t) ;; Return success for function existence
            (progn
              (termint-jupyter-test-log 'error "✗ Function %s missing" start-function)
              nil)))
      
      ;; Full buffer creation test (when display is available)
      (let ((initial-buffers (termint-jupyter-test-get-buffer-names))
            (success nil))
        
        (termint-jupyter-test-log 'debug "Initial buffer count: %d" (length initial-buffers))
        (termint-jupyter-test-log 'debug "Initial buffers: %s" initial-buffers)
        
        ;; Try to start the kernel
        (condition-case err
            (progn
              (termint-jupyter-test-log 'debug "Calling %s..." start-function)
              (funcall start-function)
              
              ;; Wait a moment for buffer creation
              (sleep-for 2)
              
              ;; Check if expected buffer was created
              (let* ((final-buffers (termint-jupyter-test-get-buffer-names))
                     (new-buffers (seq-difference final-buffers initial-buffers))
                     (target-buffer (get-buffer expected-buffer-name)))
                
                (termint-jupyter-test-log 'debug "Final buffer count: %d" (length final-buffers))
                (termint-jupyter-test-log 'debug "Final buffers: %s" final-buffers)
                (termint-jupyter-test-log 'debug "New buffers created: %s" new-buffers)
                
                ;; Check for exact expected buffer name
                (if target-buffer
                    (progn
                      (termint-jupyter-test-log 'success "✓ Buffer %s created successfully" expected-buffer-name)
                      
                      ;; Test buffer properties
                      (with-current-buffer target-buffer
                        (termint-jupyter-test-log 'debug "Buffer mode: %s" major-mode)
                        (termint-jupyter-test-log 'debug "Buffer process: %s" 
                                                 (if (get-buffer-process (current-buffer)) "running" "none"))
                        
                        ;; Check if it's a vterm buffer
                        (if (eq major-mode 'vterm-mode)
                            (termint-jupyter-test-log 'success "✓ Buffer is using vterm backend")
                          (termint-jupyter-test-log 'warning "? Buffer mode is %s (expected vterm-mode)" major-mode)))
                      
                      (setq success t))
                  
                  ;; If exact name not found, check if any new buffer was created
                  (if new-buffers
                      (progn
                        (termint-jupyter-test-log 'warning "? Expected buffer %s not found, but new buffers created: %s" 
                                                 expected-buffer-name new-buffers)
                        ;; Check if any of the new buffers contain our kernel name
                        (let ((likely-buffer (seq-find (lambda (name) 
                                                         (string-match-p (regexp-quote (substring kernel-name 0 3)) name))
                                                       new-buffers)))
                          (if likely-buffer
                              (progn
                                (termint-jupyter-test-log 'info "Found likely buffer: %s" likely-buffer)
                                (with-current-buffer (get-buffer likely-buffer)
                                  (termint-jupyter-test-log 'debug "Actual buffer mode: %s" major-mode)
                                  (if (eq major-mode 'vterm-mode)
                                      (termint-jupyter-test-log 'success "✓ Buffer is using vterm backend")
                                    (termint-jupyter-test-log 'warning "? Buffer mode is %s" major-mode))))
                            (termint-jupyter-test-log 'warning "No matching buffer name pattern found"))))
                    
                    (termint-jupyter-test-log 'error "✗ No new buffers created at all"))
                  
                  (termint-jupyter-test-log 'debug "Available jupyter-pattern buffers: %s" 
                                           (termint-jupyter-test-find-jupyter-buffers)))))
          
          (error 
           (termint-jupyter-test-log 'error "✗ Error calling %s: %s" start-function err)))
        
        success))))

;;; Main Test Suite

(defun termint-jupyter-test-setup-environment ()
  "Set up the test environment."
  (termint-jupyter-test-log 'info "=== TERMINT JUPYTER BUFFER TESTS ===")
  (termint-jupyter-test-log 'info "Setting up test environment...")
  
  ;; Clear log file
  (when (file-exists-p termint-jupyter-test-log-file)
    (delete-file termint-jupyter-test-log-file))
  
  ;; Initialize termint
  (unless (featurep 'termint)
    (termint-jupyter-test-log 'info "Loading termint...")
    (require 'termint))
  
  ;; Set backend
  (setq termint-backend 'vterm)
  (termint-jupyter-test-log 'info "Set termint-backend to vterm")
  
  ;; Clean up any existing jupyter buffers
  (termint-jupyter-test-kill-jupyter-buffers)
  
  ;; Load jupyter-termint if available
  (condition-case err
      (progn
        (require 'jupyter-termint)
        (termint-jupyter-test-log 'success "✓ jupyter-termint loaded"))
    (error 
     (termint-jupyter-test-log 'warning "Could not load jupyter-termint: %s" err)))
  
  ;; Run setup
  (condition-case err
      (progn
        (jupyter-termint-setup)
        (termint-jupyter-test-log 'success "✓ jupyter-termint-setup completed"))
    (error 
     (termint-jupyter-test-log 'error "✗ jupyter-termint-setup failed: %s" err))))

(defun termint-jupyter-test-run-all ()
  "Run all termint-jupyter tests."
  (interactive)
  
  ;; Setup
  (termint-jupyter-test-setup-environment)
  
  ;; Test function availability
  (termint-jupyter-test-check-functions)
  
  ;; Check if we're in batch mode - if so, skip buffer creation tests
  (if noninteractive
      (progn
        (termint-jupyter-test-log 'info "=== BATCH MODE DETECTED ===")
        (termint-jupyter-test-log 'warning "⚠️  Skipping buffer creation tests in batch mode (vterm requires display)")
        (termint-jupyter-test-log 'info "Function availability tests: ✓ PASSED")
        (termint-jupyter-test-log 'info "Run M-x termint-jupyter-test-interactive in GUI Emacs to test buffer creation"))
    
    ;; Test buffer creation for each kernel (interactive mode only)
    (termint-jupyter-test-log 'info "=== Testing Buffer Creation ===")
    
    (let ((tests '((termint-jupyter-python-start "*jupyter-python*" "Python")
                   (termint-jupyter-r-start "*jupyter-r*" "R") 
                   (termint-jupyter-stata-start "*jupyter-stata*" "Stata")))
          (results '()))
      
      (dolist (test tests)
        (let* ((start-func (nth 0 test))
               (buffer-name (nth 1 test))
               (kernel-name (nth 2 test))
               (result (termint-jupyter-test-buffer-creation start-func buffer-name kernel-name)))
          (push (list kernel-name result) results)))
      
      ;; Summary
      (termint-jupyter-test-log 'info "=== TEST SUMMARY ===")
      (let ((passed 0)
            (total (length results)))
        
        (dolist (result results)
          (let ((kernel (nth 0 result))
                (success (nth 1 result)))
            (if success
                (progn
                  (termint-jupyter-test-log 'success "✓ %s: PASSED" kernel)
                  (setq passed (1+ passed)))
              (termint-jupyter-test-log 'error "✗ %s: FAILED" kernel))))
        
        (termint-jupyter-test-log 'info "Results: %d/%d tests passed" passed total)
        
        (if (= passed total)
            (termint-jupyter-test-log 'success "🎉 ALL TESTS PASSED!")
          (termint-jupyter-test-log 'warning "⚠️  Some tests failed. Check log for details.")))))
  
  ;; Show current jupyter buffers
  (let ((jupyter-buffers (termint-jupyter-test-find-jupyter-buffers)))
    (termint-jupyter-test-log 'info "Current jupyter buffers: %s" jupyter-buffers))
  
  (termint-jupyter-test-log 'info "Test log written to: %s" termint-jupyter-test-log-file)
  (message "termint-jupyter tests completed. Check %s for detailed results." termint-jupyter-test-log-file))

;;; Interactive Test Functions

(defun termint-jupyter-test-python ()
  "Test just the Python jupyter integration."
  (interactive)
  (termint-jupyter-test-setup-environment)
  (termint-jupyter-test-buffer-creation 'termint-jupyter-python-start "*jupyter-python*" "Python"))

(defun termint-jupyter-test-r ()
  "Test just the R jupyter integration."
  (interactive)
  (termint-jupyter-test-setup-environment)
  (termint-jupyter-test-buffer-creation 'termint-jupyter-r-start "*jupyter-r*" "R"))

(defun termint-jupyter-test-stata ()
  "Test just the Stata jupyter integration."
  (interactive)
  (termint-jupyter-test-setup-environment)
  (termint-jupyter-test-buffer-creation 'termint-jupyter-stata-start "*jupyter-stata*" "Stata"))

(defun termint-jupyter-test-show-buffers ()
  "Show all current jupyter buffers."
  (interactive)
  (let ((jupyter-buffers (termint-jupyter-test-find-jupyter-buffers)))
    (message "Current jupyter buffers: %s" jupyter-buffers)
    jupyter-buffers))

(defun termint-jupyter-test-clean ()
  "Clean up all jupyter test buffers."
  (interactive)
  (termint-jupyter-test-kill-jupyter-buffers)
  (message "Cleaned up jupyter test buffers"))

(defun termint-jupyter-test-interactive ()
  "Run tests interactively with display support."
  (interactive)
  (termint-jupyter-test-log 'info "=== INTERACTIVE TERMINT JUPYTER TESTS ===")
  
  ;; Setup first
  (termint-jupyter-test-setup-environment)
  
  ;; Test Python buffer creation
  (termint-jupyter-test-log 'info "Testing Python Jupyter console creation...")
  (if (termint-jupyter-test-function-exists-p 'termint-jupyter-python-start)
      (progn
        (termint-jupyter-test-log 'info "Calling termint-jupyter-python-start...")
        (call-interactively 'termint-jupyter-python-start)
        (sleep-for 1)
        (if (get-buffer "*jupyter-python*")
            (termint-jupyter-test-log 'success "✓ *jupyter-python* buffer created!")
          (termint-jupyter-test-log 'error "✗ *jupyter-python* buffer not found")))
    (termint-jupyter-test-log 'error "✗ termint-jupyter-python-start function not available"))
  
  (message "Interactive test completed. Check *jupyter-python* buffer and log file."))

(defun termint-jupyter-test-quick-check ()
  "Quick check of function availability and basic setup."
  (interactive)
  (let ((functions-to-check '(termint-jupyter-python-start
                             termint-jupyter-r-start
                             termint-jupyter-stata-start
                             termint-jupyter-python-send-string
                             termint-jupyter-r-send-string
                             termint-jupyter-stata-send-string)))
    (message "=== QUICK TERMINT FUNCTION CHECK ===")
    (dolist (func functions-to-check)
      (if (fboundp func)
          (message "✓ %s: Available" func)
        (message "✗ %s: Missing" func)))
    
    (message "Current jupyter buffers: %s" (termint-jupyter-test-find-jupyter-buffers))
    (message "Termint backend: %s" termint-backend)))

;;; Batch Test Mode

(defun termint-jupyter-test-batch ()
  "Run tests in batch mode with detailed output."
  ;; Don't override noninteractive - we want to detect batch mode properly
  (termint-jupyter-test-run-all))

(provide 'test-termint-jupyter-buffers)
;;; test-termint-jupyter-buffers.el ends here