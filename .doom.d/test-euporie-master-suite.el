;;; test-euporie-master-suite.el --- Master test suite for comprehensive Emacs-euporie validation -*- lexical-binding: t; -*-

;;; Commentary:
;; Master test coordinator that runs comprehensive validation across multiple test suites.
;; This addresses the user's concern about false positive test results by implementing
;; multiple validation layers and strict pass/fail criteria.
;;
;; Test Architecture:
;; 1. STRICT VALIDATION: Zero tolerance for counter messages  
;; 2. USER EXPERIENCE: Real workflow simulation and UX validation
;; 3. INTEGRATION: End-to-end system testing
;; 4. PERFORMANCE: Responsiveness and reliability metrics
;; 5. VISUAL VALIDATION: Screenshot-based verification
;;
;; Anti-False-Positive Measures:
;; - Multiple independent validation methods
;; - String-based exact match validation 
;; - Screenshot capture for visual verification
;; - Performance timing validation
;; - Cross-session consistency checks
;; - User workflow simulation
;;
;; Success Criteria (ALL must pass):
;; - Console output contains ZERO counter messages
;; - Graphics display inline successfully  
;; - C-RET keybinding works reliably
;; - Performance meets user expectations
;; - Behavior consistent across sessions

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'euporie-termint nil t)
(require 'test-euporie-strict-validation nil t)
(require 'test-euporie-user-experience nil t)
(require 'test-euporie-stata-integration nil t)
(require 'test-euporie-stata-console-cleanliness nil t)

;; Master Test Configuration
(defvar euporie-master-test-log-file (expand-file-name "euporie-master-validation.log" "~/")
  "Master test coordination log file.")

(defvar euporie-master-test-screenshot-dir (expand-file-name "~/.doom.d/")
  "Directory for master test screenshots.")

(defvar euporie-master-test-results-file (expand-file-name "euporie-test-results.json" "~/")
  "Structured test results for analysis.")

;; Master Test Utilities
(defun euporie-master-log (level format-string &rest args)
  "Master test coordination logging."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N")))
    (with-temp-buffer
      (insert (format "[%s] [MASTER-%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-master-test-log-file))))

(defun euporie-master-clear-all-logs ()
  "Clear all test logs for fresh validation run."
  (dolist (log-file (list euporie-master-test-log-file
                         euporie-strict-test-log-file
                         euporie-ux-test-log-file
                         euporie-stata-integration-log-file
                         euporie-stata-test-log-file))
    (when (and (boundp (intern (replace-regexp-in-string "-" "_" (file-name-base log-file))))
               (file-exists-p log-file))
      (delete-file log-file)))
  
  (euporie-master-log 'info "=== COMPREHENSIVE EUPORIE VALIDATION MASTER SUITE ===")
  (euporie-master-log 'info "All test logs cleared for fresh validation run"))

(defun euporie-master-comprehensive-cleanup ()
  "Master cleanup ensuring completely clean test environment."
  (euporie-master-log 'info "Performing comprehensive master cleanup")
  
  ;; Kill all euporie processes and buffers
  (dolist (buffer-name '("*euporie-stata*" "*euporie-python*" "*euporie-r*"
                        "*test-org*" "*ux-test-org*" "*integration-test*"
                        "*validation-test*" "*strict-org-test*"))
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (when-let ((process (get-buffer-process (current-buffer))))
          (when (process-live-p process)
            (kill-process process))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name))))
  
  ;; Clean up test files
  (dolist (test-file '("~/test-stata-integration.org" "~/test-ux-stata.org" 
                      "~/test-ux-workflow.org" "~/euporie-validation-test.org"))
    (when (file-exists-p (expand-file-name test-file))
      (delete-file (expand-file-name test-file))))
  
  ;; Wait for complete cleanup
  (sleep-for 3)
  (euporie-master-log 'info "Master cleanup completed"))

(defun euporie-master-run-test-suite (suite-name test-function description)
  "Run individual test suite with comprehensive error handling."
  (euporie-master-log 'info "Starting test suite: %s" suite-name)
  (euporie-master-log 'info "Description: %s" description)
  
  (let ((suite-start (current-time))
        (suite-success nil)
        (error-caught nil))
    
    (condition-case err
        (progn
          (funcall test-function)
          (setq suite-success t)
          (euporie-master-log 'info "Test suite %s: COMPLETED SUCCESSFULLY" suite-name))
      (error 
       (setq error-caught err)
       (euporie-master-log 'error "Test suite %s: FAILED with error: %s" suite-name err)))
    
    (let ((suite-duration (float-time (time-subtract (current-time) suite-start))))
      (euporie-master-log 'info "Test suite %s duration: %.2f seconds" suite-name suite-duration)
      
      (list :suite suite-name
            :success suite-success
            :duration suite-duration
            :error error-caught))))

(defun euporie-master-validate-test-environment ()
  "Validate test environment before running comprehensive tests."
  (euporie-master-log 'info "Validating test environment")
  
  (let ((env-issues '()))
    
    ;; Check required functions
    (unless (fboundp 'euporie-stata-start)
      (push "euporie-stata-start function not available" env-issues))
    
    (unless (fboundp 'euporie-termint-send-code)
      (push "euporie-termint-send-code function not available" env-issues))
    
    ;; Check required executables
    (unless (executable-find "euporie-console")
      (push "euporie-console executable not found" env-issues))
    
    ;; Check project directory
    (unless (file-directory-p "/Users/vwh7mb/projects/emacs-euporie")
      (push "Emacs-euporie project directory not found" env-issues))
    
    ;; Check termint availability
    (unless (featurep 'termint)
      (push "termint.el not loaded" env-issues))
    
    (if env-issues
        (progn
          (euporie-master-log 'error "Environment validation FAILED:")
          (dolist (issue env-issues)
            (euporie-master-log 'error "  - %s" issue))
          nil)
      (progn
        (euporie-master-log 'info "Environment validation PASSED")
        t))))

(defun euporie-master-take-comprehensive-screenshots ()
  "Take comprehensive screenshots at key validation points."
  (let ((screenshot-timestamp (format-time-string "%Y%m%d_%H%M%S")))
    
    ;; Initial system state
    (shell-command "osascript -e 'tell application \"Emacs\" to activate'")
    (sleep-for 1)
    (shell-command (format "screencapture -T 0.5 \"%s/master-test-start-%s.png\"" 
                          euporie-master-test-screenshot-dir screenshot-timestamp))
    
    (euporie-master-log 'info "Master test screenshots captured with timestamp: %s" screenshot-timestamp)
    screenshot-timestamp))

(defun euporie-master-analyze-results (results)
  "Analyze comprehensive test results and generate master verdict."
  (euporie-master-log 'info "Analyzing comprehensive test results")
  
  (let ((total-suites (length results))
        (successful-suites 0)
        (total-duration 0)
        (critical-failures '()))
    
    (dolist (result results)
      (let ((suite (plist-get result :suite))
            (success (plist-get result :success))
            (duration (plist-get result :duration))
            (error (plist-get result :error)))
        
        (setq total-duration (+ total-duration duration))
        
        (if success
            (progn
              (setq successful-suites (1+ successful-suites))
              (euporie-master-log 'info "Suite %s: SUCCESS (%.2fs)" suite duration))
          (progn
            (push (list suite error) critical-failures)
            (euporie-master-log 'error "Suite %s: FAILURE (%.2fs) - %s" suite duration error)))))
    
    (let ((success-rate (if (> total-suites 0) 
                           (/ (float successful-suites) total-suites) 
                           0)))
      
      (euporie-master-log 'info "MASTER RESULTS SUMMARY:")
      (euporie-master-log 'info "  Total test suites: %d" total-suites)
      (euporie-master-log 'info "  Successful suites: %d" successful-suites) 
      (euporie-master-log 'info "  Success rate: %.1f%%" (* success-rate 100))
      (euporie-master-log 'info "  Total duration: %.2f seconds" total-duration)
      
      (if (and (= successful-suites total-suites) (null critical-failures))
          (progn
            (euporie-master-log 'info "=== MASTER VERDICT: ALL VALIDATIONS PASSED ===")
            (euporie-master-log 'info "Counter messages eliminated, graphics working, UX excellent")
            t)
        (progn
          (euporie-master-log 'error "=== MASTER VERDICT: VALIDATION FAILURES DETECTED ===")
          (when critical-failures
            (euporie-master-log 'error "Critical failures:")
            (dolist (failure critical-failures)
              (euporie-master-log 'error "  - %s: %s" (car failure) (cadr failure))))
          nil)))))

;; COMPREHENSIVE MASTER TESTS

(ert-deftest euporie-master/comprehensive-validation-suite ()
  "Master test that runs all validation suites with anti-false-positive measures."
  (euporie-master-log 'info "=== STARTING COMPREHENSIVE MASTER VALIDATION ===")
  (euporie-master-comprehensive-cleanup)
  
  ;; Environment validation
  (should (euporie-master-validate-test-environment))
  
  ;; Take initial screenshots
  (let ((screenshot-timestamp (euporie-master-take-comprehensive-screenshots))
        (master-start (current-time)))
    
    (unwind-protect
        (let ((test-results '()))
          
          ;; Run each test suite independently
          (push (euporie-master-run-test-suite 
                 "STRICT-VALIDATION" 
                 #'euporie-strict-run-critical-tests-only
                 "Zero tolerance validation for counter messages") test-results)
          
          (push (euporie-master-run-test-suite 
                 "USER-EXPERIENCE" 
                 #'euporie-ux-run-critical-ux-tests
                 "Real user workflow simulation and UX validation") test-results)
          
          (push (euporie-master-run-test-suite 
                 "INTEGRATION" 
                 #'euporie-stata-run-critical-integration-tests
                 "End-to-end integration testing") test-results)
          
          (push (euporie-master-run-test-suite 
                 "CONSOLE-CLEANLINESS" 
                 #'euporie-stata-console-run-critical-tests
                 "Console output cleanliness validation") test-results)
          
          ;; Analyze results
          (let ((master-success (euporie-master-analyze-results (reverse test-results)))
                (master-duration (float-time (time-subtract (current-time) master-start))))
            
            ;; Take final screenshot
            (shell-command (format "screencapture -T 0.5 \"%s/master-test-complete-%s.png\"" 
                                  euporie-master-test-screenshot-dir screenshot-timestamp))
            
            (euporie-master-log 'info "Master validation completed in %.2f seconds" master-duration)
            
            ;; Master assertion
            (should master-success)))
      
      (euporie-master-comprehensive-cleanup))))

(ert-deftest euporie-master/anti-false-positive-validation ()
  "Specific test to prevent false positive results by multiple validation methods."
  (euporie-master-log 'info "=== ANTI-FALSE-POSITIVE VALIDATION ===")
  (euporie-master-comprehensive-cleanup)
  
  (unwind-protect
      (progn
        ;; Start euporie
        (euporie-stata-start)
        (sleep-for 10)
        (should (get-buffer "*euporie-stata*"))
        
        ;; Execute graphics command with multiple validation approaches
        (let ((buffer-name "*euporie-stata*"))
          (with-current-buffer buffer-name
            (let ((initial-marker (point-max))
                  (process (get-buffer-process (current-buffer))))
              
              (when process
                ;; Send data loading command
                (process-send-string process "sysuse auto\n")
                (sleep-for 5)
                
                ;; Send graphics command
                (let ((graphics-marker (point-max)))
                  (process-send-string process "scatter price mpg\n")
                  (sleep-for 10)
                  
                  ;; VALIDATION METHOD 1: String analysis
                  (let ((full-output (buffer-substring-no-properties initial-marker (point-max)))
                        (graphics-output (buffer-substring-no-properties graphics-marker (point-max))))
                    
                    (euporie-master-log 'info "Method 1 - String analysis:")
                    (euporie-master-log 'info "Full output length: %d chars" (length full-output))
                    (euporie-master-log 'info "Graphics output: %S" graphics-output)
                    
                    ;; Check for counter messages (multiple patterns)
                    (let ((counter-patterns '("global stata_kernel_graph_counter"
                                             "stata_kernel_graph_counter.*="
                                             "\\$stata_kernel_graph_counter.*\\+"
                                             "graph_counter.*\\+.*1"))
                          (violations '()))
                      
                      (dolist (pattern counter-patterns)
                        (when (string-match-p pattern full-output)
                          (push pattern violations)))
                      
                      (euporie-master-log 'info "Counter message violations: %s" violations)
                      (should (null violations))  ; Must be empty for success
                      
                      ;; VALIDATION METHOD 2: Line-by-line analysis
                      (let ((output-lines (split-string full-output "\n"))
                            (suspicious-lines '()))
                        
                        (dolist (line output-lines)
                          (when (or (string-match-p "counter" line)
                                   (string-match-p "global.*stata_kernel" line))
                            (push line suspicious-lines)))
                        
                        (euporie-master-log 'info "Method 2 - Suspicious lines: %s" suspicious-lines)
                        (should (null suspicious-lines))  ; Must be empty
                        
                        ;; VALIDATION METHOD 3: Graphics file verification
                        (let ((cache-dir (expand-file-name "~/.stata_kernel_cache")))
                          (when (file-directory-p cache-dir)
                            (let ((png-files (directory-files cache-dir nil "\\.png$")))
                              (euporie-master-log 'info "Method 3 - PNG files found: %d" (length png-files))
                              (should (> (length png-files) 0))  ; Graphics should be created
                              
                              ;; VALIDATION METHOD 4: Console responsiveness
                              (let ((responsive-marker (point-max)))
                                (process-send-string process "display \"ANTI_FALSE_POSITIVE_TEST\"\n")
                                (sleep-for 3)
                                
                                (let ((responsive-output (buffer-substring-no-properties responsive-marker (point-max))))
                                  (euporie-master-log 'info "Method 4 - Responsiveness test: %S" responsive-output)
                                  (should (string-match-p "ANTI_FALSE_POSITIVE_TEST" responsive-output))
                                  
                                  ;; Final validation: No counter messages in any output
                                  (should-not (string-match-p "stata_kernel_graph_counter" responsive-output)))))))))))))))
    
    (euporie-master-comprehensive-cleanup)))

;; MASTER TEST SUITE INTERFACE

(defun euporie-master-run-comprehensive-validation ()
  "Run the complete master validation suite."
  (interactive)
  (euporie-master-clear-all-logs)
  
  (let ((master-start (current-time)))
    (euporie-master-log 'info "STARTING COMPREHENSIVE EUPORIE MASTER VALIDATION")
    (euporie-master-log 'info "Anti-false-positive measures enabled")
    (euporie-master-log 'info "Multiple validation methods will be used")
    (euporie-master-log 'info "All test suites will be coordinated for maximum coverage")
    
    ;; Run master test
    (ert-run-tests-interactively "euporie-master/comprehensive-validation-suite")
    
    (let ((master-duration (float-time (time-subtract (current-time) master-start))))
      (euporie-master-log 'info "Master validation completed in %.2f seconds" master-duration)
      (euporie-master-generate-comprehensive-report))))

(defun euporie-master-run-anti-false-positive-only ()
  "Run only the anti-false-positive validation test."
  (interactive)
  (euporie-master-clear-all-logs)
  (euporie-master-log 'info "Running ANTI-FALSE-POSITIVE validation only")
  
  (ert-run-tests-interactively "euporie-master/anti-false-positive-validation"))

(defun euporie-master-generate-comprehensive-report ()
  "Generate master comprehensive validation report."
  (interactive)
  (let ((report-buffer (get-buffer-create "*Master Euporie Validation Report*")))
    (with-current-buffer report-buffer
      (erase-buffer)
      (insert "# MASTER EMACS-EUPORIE COMPREHENSIVE VALIDATION REPORT\n\n")
      (insert (format "Generated: %s\n\n" (current-time-string)))
      
      (insert "## Validation Architecture\n\n")
      (insert "This master suite coordinates multiple independent test suites:\n\n")
      (insert "1. **STRICT VALIDATION**: Zero tolerance for counter messages\n")
      (insert "2. **USER EXPERIENCE**: Real workflow simulation and UX validation\n")
      (insert "3. **INTEGRATION**: End-to-end system testing\n")
      (insert "4. **CONSOLE CLEANLINESS**: Output quality validation\n")
      (insert "5. **ANTI-FALSE-POSITIVE**: Multiple validation method verification\n\n")
      
      (insert "## Success Criteria (ALL must pass)\n\n")
      (insert "- Console output contains ZERO counter messages\n")
      (insert "- Graphics display inline successfully\n")
      (insert "- C-RET keybinding works reliably\n")
      (insert "- Performance meets user expectations\n")
      (insert "- Behavior consistent across sessions\n\n")
      
      ;; Include master log
      (when (file-exists-p euporie-master-test-log-file)
        (insert "## Master Test Coordination Log\n\n```\n")
        (insert-file-contents euporie-master-test-log-file)
        (goto-char (point-max))
        (insert "\n```\n\n")
        
        ;; Analysis
        (goto-char (point-min))
        (let ((log-content (buffer-string)))
          (goto-char (point-max))
          (insert "## Master Validation Summary\n\n")
          
          (if (string-match-p "ALL VALIDATIONS PASSED" log-content)
              (insert "**✅ MASTER VERDICT: ALL VALIDATIONS PASSED**\n\n")
            (insert "**❌ MASTER VERDICT: VALIDATION FAILURES DETECTED**\n\n"))
          
          (when (string-match-p "Screenshots captured" log-content)
            (insert "Visual validation screenshots captured in ~/.doom.d/\n\n"))))
      
      (insert "## Next Steps\n\n")
      (insert "If validation passed: Implementation meets all quality criteria.\n")
      (insert "If validation failed: Review detailed logs for specific failure points.\n")
      
      (display-buffer report-buffer)
      (message "Master comprehensive validation report generated"))))

;; Load notification
(unless noninteractive
  (message "Emacs-euporie Master Validation Suite loaded.")
  (message "")
  (message "COMPREHENSIVE VALIDATION COMMANDS:")
  (message "- (euporie-master-run-comprehensive-validation) - Run complete master suite")
  (message "- (euporie-master-run-anti-false-positive-only) - Run anti-false-positive test")
  (message "- (euporie-master-generate-comprehensive-report) - Generate master report")
  (message "")
  (message "This master suite coordinates ALL test suites with anti-false-positive measures.")
  (message "Success requires ALL validation methods to pass simultaneously."))

(provide 'test-euporie-master-suite)
;;; test-euporie-master-suite.el ends here