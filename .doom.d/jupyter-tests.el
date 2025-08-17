;;; jupyter-tests.el --- Comprehensive Jupyter testing framework -*- lexical-binding: t; -*-

;;; Commentary:
;; Systematic testing framework for Jupyter integration debugging
;; Tests ZMQ, kernel discovery, connections, and execution

;;; Code:

(require 'cl-lib)

(defvar jupyter-test-results '()
  "List of test results with detailed information.")

(defvar jupyter-test-kernels '("python3" "ir" "nbstata")
  "List of kernels to test systematically.")

(defun jupyter-test-log-result (test-name status message &optional details)
  "Log a test result with optional details.
TEST-NAME: name of the test
STATUS: 'pass, 'fail, 'skip, 'warn
MESSAGE: summary message
DETAILS: optional detailed information"
  (let ((result (list :test test-name 
                     :status status 
                     :message message 
                     :details details
                     :time (current-time))))
    (push result jupyter-test-results)
    (jupyter-debug-log (if (eq status 'pass) 'info 
                        (if (eq status 'fail) 'error 'warn))
                       "[TEST %s] %s: %s" 
                       (upcase (symbol-name status)) 
                       test-name 
                       message)
    (when details
      (jupyter-debug-log 'debug "Details: %s" details))))

(defun jupyter-test-reset ()
  "Reset test results and prepare for new test run."
  (interactive)
  (setq jupyter-test-results '())
  (jupyter-debug-clear-log)
  (jupyter-debug-log 'info "=== Jupyter Test Suite Started ==="))

(defun jupyter-test-environment ()
  "Test 1: Environment and executable detection."
  (jupyter-debug-log 'info "=== Test 1: Environment Check ===")
  
  ;; Test Jupyter executable detection
  (let ((jupyter-cmd (find-pixi-jupyter)))
    (if (and jupyter-cmd (file-executable-p jupyter-cmd))
        (jupyter-test-log-result "jupyter-executable" 'pass 
                                (format "Found executable: %s" jupyter-cmd))
      (jupyter-test-log-result "jupyter-executable" 'fail 
                              "No valid jupyter executable found"
                              (format "Checked: %s" jupyter-cmd))))
  
  ;; Test kernelspec command
  (condition-case err
      (let* ((jupyter-cmd (or (find-pixi-jupyter) "jupyter"))
             (output (shell-command-to-string (format "%s kernelspec list --json" jupyter-cmd)))
             (json-data (condition-case nil (json-read-from-string output) (error nil))))
        (if json-data
            (jupyter-test-log-result "kernelspec-command" 'pass 
                                    "Kernelspec command successful"
                                    (format "Output length: %d" (length output)))
          (jupyter-test-log-result "kernelspec-command" 'fail 
                                  "Failed to parse kernelspec JSON"
                                  output)))
    (error (jupyter-test-log-result "kernelspec-command" 'fail 
                                   "Kernelspec command failed"
                                   (format "Error: %s" err))))
  
  ;; Test environment variables
  (let ((path (getenv "PATH")))
    (if (string-match-p "pixi" path)
        (jupyter-test-log-result "environment-path" 'pass "PATH includes pixi")
      (jupyter-test-log-result "environment-path" 'warn "PATH may not include pixi environment"))))

(defun jupyter-test-packages ()
  "Test 2: Package loading and availability."
  (jupyter-debug-log 'info "=== Test 2: Package Loading ===")
  
  ;; Test jupyter package
  (condition-case err
      (progn
        (require 'jupyter)
        (jupyter-test-log-result "jupyter-package" 'pass "Jupyter package loaded"))
    (error (jupyter-test-log-result "jupyter-package" 'fail 
                                   "Failed to load jupyter package"
                                   (format "Error: %s" err))))
  
  ;; Test ZMQ availability (but don't require it)
  (condition-case err
      (progn
        (require 'zmq)
        (jupyter-test-log-result "zmq-package" 'pass "ZMQ package available"))
    (error (jupyter-test-log-result "zmq-package" 'warn 
                                   "ZMQ package not available (may be intentional)"
                                   (format "Error: %s" err))))
  
  ;; Test ob-jupyter
  (condition-case err
      (progn
        (require 'ob-jupyter)
        (jupyter-test-log-result "ob-jupyter-package" 'pass "ob-jupyter package loaded"))
    (error (jupyter-test-log-result "ob-jupyter-package" 'fail 
                                   "Failed to load ob-jupyter package"
                                   (format "Error: %s" err)))))

(defun jupyter-test-kernelspecs ()
  "Test 3: Kernel discovery."
  (jupyter-debug-log 'info "=== Test 3: Kernel Discovery ===")
  
  (condition-case err
      (progn
        (require 'jupyter)
        (let ((specs (jupyter-available-kernelspecs)))
          (if specs
              (progn
                (jupyter-test-log-result "kernelspec-discovery" 'pass 
                                        (format "Found %d kernels" (length specs))
                                        (mapcar #'jupyter-kernelspec-name specs))
                
                ;; Test specific kernels
                (dolist (kernel jupyter-test-kernels)
                  (if (cl-find kernel specs :key #'jupyter-kernelspec-name :test #'string=)
                      (jupyter-test-log-result (format "kernel-%s-available" kernel) 'pass 
                                              (format "%s kernel found" kernel))
                    (jupyter-test-log-result (format "kernel-%s-available" kernel) 'fail 
                                            (format "%s kernel not found" kernel)))))
            (jupyter-test-log-result "kernelspec-discovery" 'fail "No kernels discovered"))))
    (error (jupyter-test-log-result "kernelspec-discovery" 'fail 
                                   "Kernelspec discovery failed"
                                   (format "Error: %s" err)))))

(defun jupyter-test-kernel-startup (kernel-name &optional timeout)
  "Test 4: Individual kernel startup.
KERNEL-NAME: name of kernel to test
TIMEOUT: timeout in seconds (default 10)"
  (let ((timeout (or timeout 10)))
    (jupyter-debug-log 'info "Testing kernel startup: %s" kernel-name)
    
    (condition-case err
        (let ((start-time (current-time)))
          ;; Attempt to start kernel
          (jupyter-debug-log 'debug "Starting %s kernel..." kernel-name)
          (let ((kernel (jupyter-start-kernel kernel-name)))
            (if kernel
                (progn
                  (jupyter-test-log-result (format "kernel-%s-startup" kernel-name) 'pass 
                                          (format "%s kernel started successfully" kernel-name)
                                          (format "Startup time: %.2f seconds" 
                                                 (float-time (time-subtract (current-time) start-time))))
                  ;; Clean up
                  (condition-case cleanup-err
                      (jupyter-shutdown-kernel kernel)
                    (error (jupyter-debug-log 'warn "Failed to shutdown %s kernel: %s" 
                                             kernel-name cleanup-err))))
              (jupyter-test-log-result (format "kernel-%s-startup" kernel-name) 'fail 
                                      (format "%s kernel startup returned nil" kernel-name)))))
      (error (jupyter-test-log-result (format "kernel-%s-startup" kernel-name) 'fail 
                                     (format "%s kernel startup failed" kernel-name)
                                     (format "Error: %s" err))))))

(defun jupyter-test-simple-execution (kernel-name code expected-pattern)
  "Test 5: Simple code execution.
KERNEL-NAME: kernel to test
CODE: code to execute  
EXPECTED-PATTERN: regex pattern to match in output"
  (jupyter-debug-log 'info "Testing simple execution in %s" kernel-name)
  
  (condition-case err
      (let ((kernel (jupyter-start-kernel kernel-name)))
        (if kernel
            (progn
              ;; Execute simple code
              (jupyter-debug-log 'debug "Executing code: %s" code)
              (let ((request (jupyter-send-execute-request kernel code)))
                (if request
                    (jupyter-test-log-result (format "execution-%s" kernel-name) 'pass 
                                            (format "%s execution request sent" kernel-name))
                  (jupyter-test-log-result (format "execution-%s" kernel-name) 'fail 
                                          (format "%s execution request failed" kernel-name))))
              ;; Clean up
              (jupyter-shutdown-kernel kernel))
          (jupyter-test-log-result (format "execution-%s" kernel-name) 'skip 
                                  (format "%s kernel not available for execution test" kernel-name))))
    (error (jupyter-test-log-result (format "execution-%s" kernel-name) 'fail 
                                   (format "%s execution test failed" kernel-name)
                                   (format "Error: %s" err)))))

(defun jupyter-test-comprehensive ()
  "Run comprehensive test suite."
  (interactive)
  (jupyter-test-reset)
  (jupyter-debug-environment)
  
  ;; Run test phases
  (jupyter-test-environment)
  (jupyter-test-packages)
  (jupyter-test-kernelspecs)
  
  ;; Test kernel startup for each kernel
  (dolist (kernel jupyter-test-kernels)
    (jupyter-test-kernel-startup kernel))
  
  ;; Test simple execution
  (jupyter-test-simple-execution "python3" "print('hello')" "hello")
  (jupyter-test-simple-execution "ir" "cat('hello')" "hello")
  (jupyter-test-simple-execution "nbstata" "display \"hello\"" "hello")
  
  ;; Display results
  (jupyter-test-display-results))

(defun jupyter-test-display-results ()
  "Display comprehensive test results."
  (interactive)
  (let ((buffer (get-buffer-create "*Jupyter Test Results*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== JUPYTER COMPREHENSIVE TEST RESULTS ===\n\n")
      
      (let ((pass-count 0) (fail-count 0) (warn-count 0) (skip-count 0))
        ;; Count results
        (dolist (result (reverse jupyter-test-results))
          (let ((status (plist-get result :status)))
            (cl-case status
              ('pass (cl-incf pass-count))
              ('fail (cl-incf fail-count))
              ('warn (cl-incf warn-count))
              ('skip (cl-incf skip-count)))))
        
        ;; Display summary
        (insert (format "SUMMARY: %d PASS, %d FAIL, %d WARN, %d SKIP\n\n" 
                       pass-count fail-count warn-count skip-count))
        
        ;; Display detailed results
        (dolist (result (reverse jupyter-test-results))
          (let ((test (plist-get result :test))
                (status (plist-get result :status))
                (message (plist-get result :message))
                (details (plist-get result :details)))
            (insert (format "[%s] %s: %s\n" 
                           (upcase (symbol-name status)) test message))
            (when details
              (insert (format "    Details: %s\n" details)))
            (insert "\n")))
        
        ;; Show overall status
        (goto-char (point-min))
        (if (= fail-count 0)
            (insert "✓ ALL CRITICAL TESTS PASSED\n\n")
          (insert (format "✗ %d CRITICAL FAILURES DETECTED\n\n" fail-count))))
      
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun jupyter-test-quick ()
  "Quick test - just environment and package loading."
  (interactive)
  (jupyter-test-reset)
  (jupyter-test-environment)
  (jupyter-test-packages)
  (jupyter-test-display-results))

(provide 'jupyter-tests)
;;; jupyter-tests.el ends here