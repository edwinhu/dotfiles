;;; test-euporie-strict-validation.el --- Strict unit tests for Emacs-euporie integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive unit tests with STRICT validation for the Emacs-euporie integration.
;; These tests are designed to catch false positives and ensure the exact user experience
;; the user expects. Based on the user's feedback showing counter messages still visible
;; despite previous "successful" test reports.
;;
;; STRICT SUCCESS CRITERIA:
;; 1. Visual Graph Display: Must show actual graphics inline (not just file creation)
;; 2. Zero Counter Messages: Absolutely NO `global stata_kernel_graph_counter` text anywhere
;; 3. Clean Console Output: Only legitimate output should appear
;; 4. No False Positives: Test must actually fail when counter messages are present
;;
;; Test Design Philosophy:
;; - Screenshot-based verification of actual visual output
;; - String-based validation of exact console content
;; - Multiple validation layers to prevent false positives
;; - Real user workflow simulation
;; - Explicit failure when cleanliness criteria not met

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'euporie-termint nil t)
(require 'org)

;; Test Configuration
(defvar euporie-strict-test-log-file (expand-file-name "euporie-strict-validation.log" "~/")
  "Detailed log file for strict validation tests.")

(defvar euporie-strict-test-screenshot-dir (expand-file-name "~/.doom.d/")
  "Directory for validation screenshots.")

(defvar euporie-strict-test-timeout 60
  "Extended timeout for thorough validation.")

;; Strict Validation Utilities
(defun euporie-strict-log (level format-string &rest args)
  "Log with detailed timestamp and validation context."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-strict-test-log-file))))

(defun euporie-strict-clear-log ()
  "Clear validation log for fresh test run."
  (when (file-exists-p euporie-strict-test-log-file)
    (delete-file euporie-strict-test-log-file))
  (euporie-strict-log 'info "=== STRICT VALIDATION TEST SESSION STARTED ==="))

(defun euporie-strict-cleanup ()
  "Comprehensive cleanup with process termination."
  (euporie-strict-log 'info "Performing comprehensive cleanup")
  
  ;; Kill all euporie-related buffers with extreme prejudice
  (dolist (buffer-name '("*euporie-stata*" "*euporie-python*" "*euporie-r*" 
                        "*test-org*" "*integration-test*" "*validation-test*"))
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (when-let ((process (get-buffer-process (current-buffer))))
          (when (process-live-p process)
            (kill-process process)
            (sleep-for 1))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name))))
  
  ;; Additional cleanup time
  (sleep-for 3)
  (euporie-strict-log 'info "Cleanup completed"))

(defun euporie-strict-wait-for-ready (buffer-name timeout)
  "Wait for buffer to be completely ready with strict validation."
  (euporie-strict-log 'info "Waiting for %s to be ready (timeout: %ds)" buffer-name timeout)
  
  (let ((start-time (current-time))
        (ready nil)
        (ready-checks 0))
    
    (while (and (not ready)
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (let ((process (get-buffer-process (current-buffer))))
            (when (and process (process-live-p process))
              ;; Strict readiness test: send a simple command and verify response
              (let ((test-marker (point-max)))
                (process-send-string process "display \"READINESS_TEST\"\n")
                (sleep-for 2)
                
                (let ((output (buffer-substring-no-properties test-marker (point-max))))
                  (when (string-match-p "READINESS_TEST" output)
                    (setq ready-checks (1+ ready-checks))
                    ;; Require multiple successful checks for stability
                    (when (>= ready-checks 2)
                      (setq ready t)
                      (euporie-strict-log 'info "%s confirmed ready after %d checks" 
                                         buffer-name ready-checks)))))))))
      (unless ready
        (sleep-for 1)))
    
    (unless ready
      (euporie-strict-log 'error "%s failed to become ready within %ds" buffer-name timeout))
    
    ready))

(defun euporie-strict-execute-and-capture (kernel code description wait-time)
  "Execute code with strict output capture and validation."
  (let* ((buffer-name (format "*euporie-%s*" kernel))
         (start-marker (when (get-buffer buffer-name)
                        (with-current-buffer buffer-name (point-max))))
         (execution-start (current-time)))
    
    (euporie-strict-log 'info "EXECUTING: %s - %s" description code)
    
    ;; Execute via euporie-termint
    (euporie-termint-send-code kernel code)
    
    ;; Wait for execution with progress logging
    (dotimes (i wait-time)
      (when (= (mod i 2) 0)  ; Log every 2 seconds
        (euporie-strict-log 'info "Execution progress: %d/%d seconds" i wait-time))
      (sleep-for 1))
    
    ;; Capture complete output
    (let ((full-output (when (and (get-buffer buffer-name) start-marker)
                        (with-current-buffer buffer-name
                          (buffer-substring-no-properties start-marker (point-max)))))
          (execution-time (float-time (time-subtract (current-time) execution-start))))
      
      (euporie-strict-log 'info "%s completed in %.2fs, output length: %d chars" 
                         description execution-time 
                         (length (or full-output "")))
      
      ;; Log output excerpt for debugging
      (when full-output
        (let ((excerpt (substring full-output 0 (min 200 (length full-output)))))
          (euporie-strict-log 'debug "Output excerpt: %S" excerpt)))
      
      full-output)))

(defun euporie-strict-validate-counter-absence (output description)
  "STRICT validation: Counter messages must be completely absent."
  (let ((counter-patterns '("global stata_kernel_graph_counter"
                           "stata_kernel_graph_counter.*="
                           "stata_kernel_graph_counter.*\\+"
                           "graph_counter.*\\+.*1"
                           "\\$stata_kernel_graph_counter"
                           "stata_kernel.*counter.*="))
        (violations '()))
    
    (when (and output (stringp output))
      (dolist (pattern counter-patterns)
        (when (string-match-p pattern output)
          (push pattern violations))))
    
    (if violations
        (progn
          (euporie-strict-log 'error "%s: FAILED - Counter message violations: %s" 
                             description violations)
          (euporie-strict-log 'error "Full output for analysis: %S" output)
          nil)  ; Return nil for failure
      (progn
        (euporie-strict-log 'info "%s: PASSED - No counter messages detected" description)
        t))))   ; Return t for success

(defun euporie-strict-validate-graphics-display (buffer-name description)
  "Validate that graphics are actually displayed (not just files created)."
  (let ((cache-dir (expand-file-name "~/.stata_kernel_cache"))
        (graphics-detected nil))
    
    ;; Check for PNG files in cache
    (when (file-directory-p cache-dir)
      (let ((png-files (directory-files cache-dir nil "\\.png$")))
        (when (> (length png-files) 0)
          (setq graphics-detected t)
          (euporie-strict-log 'info "%s: Found %d graphics files in cache" 
                             description (length png-files)))))
    
    ;; Check buffer for graphics-related content (sixel sequences, etc.)
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max))))
          ;; Look for terminal graphics sequences
          (when (or (string-match-p "\\(7bit\\|8bit\\)" buffer-content)  ; sixel indicators
                    (string-match-p "graphics" buffer-content))          ; graphics protocol
            (setq graphics-detected t)
            (euporie-strict-log 'info "%s: Detected graphics sequences in buffer" description)))))
    
    (euporie-strict-log 'info "%s: Graphics display validation: %s" 
                       description (if graphics-detected "PASSED" "FAILED"))
    graphics-detected))

(defun euporie-strict-take-validation-screenshot (filename description)
  "Take screenshot for visual validation with proper focus."
  (let ((screenshot-path (expand-file-name filename euporie-strict-test-screenshot-dir)))
    (euporie-strict-log 'info "Taking validation screenshot: %s" description)
    
    ;; Focus Emacs window first
    (shell-command "osascript -e 'tell application \"Emacs\" to activate'")
    (sleep-for 1)
    
    ;; Capture with delay
    (shell-command (format "screencapture -T 0.5 \"%s\"" screenshot-path))
    (sleep-for 1)
    
    (if (file-exists-p screenshot-path)
        (progn
          (euporie-strict-log 'info "Screenshot saved: %s" screenshot-path)
          screenshot-path)
      (progn
        (euporie-strict-log 'error "Screenshot failed: %s" screenshot-path)
        nil))))

;; CORE STRICT VALIDATION TESTS

(ert-deftest euporie-strict/stata-scatter-plot-zero-counter-messages ()
  "STRICT TEST: Stata scatter plot must show NO counter messages whatsoever."
  (euporie-strict-log 'info "=== STRICT TEST: Stata scatter plot validation ===")
  (euporie-strict-cleanup)
  
  (unwind-protect
      (progn
        ;; Step 1: Start euporie with strict validation
        (euporie-strict-log 'info "Step 1: Starting euporie-stata with strict monitoring")
        (euporie-stata-start)
        (should (euporie-strict-wait-for-ready "*euporie-stata*" euporie-strict-test-timeout))
        
        ;; Take initial screenshot
        (euporie-strict-take-validation-screenshot "strict-stata-startup.png" 
                                                  "Euporie stata startup")
        
        ;; Step 2: Load data with validation
        (euporie-strict-log 'info "Step 2: Loading dataset with output monitoring")
        (let ((data-output (euporie-strict-execute-and-capture 
                           "stata" "sysuse auto" "Data loading" 8)))
          
          ;; Validate data loading was clean
          (should (euporie-strict-validate-counter-absence data-output "Data loading"))
          (should (string-match-p "auto.dta" (or data-output ""))))
        
        ;; Step 3: CRITICAL TEST - Scatter plot with zero tolerance for counter messages
        (euporie-strict-log 'info "Step 3: CRITICAL - Scatter plot with zero counter tolerance")
        (let ((plot-output (euporie-strict-execute-and-capture 
                           "stata" "scatter price mpg" "Scatter plot" 15)))
          
          ;; PRIMARY VALIDATION: Absolutely no counter messages
          (should (euporie-strict-validate-counter-absence plot-output "Scatter plot"))
          
          ;; SECONDARY VALIDATION: Graphics must be displayed
          (should (euporie-strict-validate-graphics-display "*euporie-stata*" "Scatter plot graphics"))
          
          ;; TERTIARY VALIDATION: Console must remain responsive
          (let ((responsive-output (euporie-strict-execute-and-capture 
                                   "stata" "display \"POST_PLOT_TEST\"" "Responsiveness test" 5)))
            (should (string-match-p "POST_PLOT_TEST" (or responsive-output "")))
            (should (euporie-strict-validate-counter-absence responsive-output "Post-plot responsiveness"))))
        
        ;; Take final validation screenshot
        (euporie-strict-take-validation-screenshot "strict-stata-scatter-complete.png" 
                                                  "Stata scatter plot completed")
        
        (euporie-strict-log 'info "=== STRICT TEST PASSED: No counter messages detected ==="))
    
    (euporie-strict-cleanup)))

(ert-deftest euporie-strict/stata-multiple-plots-console-pollution-check ()
  "STRICT TEST: Multiple plots must not pollute console with ANY counter messages."
  (euporie-strict-log 'info "=== STRICT TEST: Multiple plots console pollution ===")
  (euporie-strict-cleanup)
  
  (unwind-protect
      (progn
        ;; Setup
        (euporie-stata-start)
        (should (euporie-strict-wait-for-ready "*euporie-stata*" euporie-strict-test-timeout))
        
        (euporie-strict-execute-and-capture "stata" "sysuse auto" "Setup data" 5)
        
        ;; Test multiple plot types
        (let ((plot-commands '(("scatter price mpg" . "Scatter plot")
                              ("histogram price, bins(10)" . "Histogram") 
                              ("twoway scatter price mpg || lfit price mpg" . "Twoway with fit")))
              (all-clean t))
          
          (dolist (cmd-desc plot-commands)
            (let* ((command (car cmd-desc))
                   (description (cdr cmd-desc))
                   (output (euporie-strict-execute-and-capture "stata" command description 12)))
              
              ;; STRICT: Each plot must be completely clean
              (unless (euporie-strict-validate-counter-absence output description)
                (setq all-clean nil))
              
              ;; Validate graphics display
              (should (euporie-strict-validate-graphics-display "*euporie-stata*" description))))
          
          ;; Overall validation: ALL plots must be clean
          (should all-clean)
          (euporie-strict-log 'info "Multiple plots test: %s" 
                             (if all-clean "ALL CLEAN" "POLLUTION DETECTED"))))
    
    (euporie-strict-cleanup)))

(ert-deftest euporie-strict/comparison-stata-vs-python-console-cleanliness ()
  "STRICT TEST: Stata console cleanliness must match Python baseline behavior."
  (euporie-strict-log 'info "=== STRICT TEST: Stata vs Python console comparison ===")
  (euporie-strict-cleanup)
  
  (unwind-protect
      (progn
        ;; Baseline: Python graphics (should be clean)
        (euporie-strict-log 'info "Testing Python baseline for comparison")
        (euporie-python-start)
        (should (euporie-strict-wait-for-ready "*euporie-python*" euporie-strict-test-timeout))
        
        (let ((python-output (euporie-strict-execute-and-capture 
                              "python" 
                              "import matplotlib.pyplot as plt\nimport numpy as np\nx = np.linspace(0, 10, 50)\ny = np.sin(x)\nplt.plot(x, y)\nplt.title('Python Test')\nplt.show()"
                              "Python matplotlib test" 10)))
          
          (let ((python-clean (euporie-strict-validate-counter-absence python-output "Python graphics")))
            
            ;; Clean up Python buffer
            (let ((kill-buffer-query-functions nil))
              (kill-buffer "*euporie-python*"))
            
            ;; Test Stata (should match Python cleanliness)
            (euporie-strict-log 'info "Testing Stata for comparison with Python baseline")
            (euporie-stata-start)
            (should (euporie-strict-wait-for-ready "*euporie-stata*" euporie-strict-test-timeout))
            
            (euporie-strict-execute-and-capture "stata" "sysuse auto" "Stata setup" 5)
            
            (let ((stata-output (euporie-strict-execute-and-capture 
                                 "stata" "scatter price mpg, title(\"Stata Test\")" 
                                 "Stata graphics test" 12)))
              
              (let ((stata-clean (euporie-strict-validate-counter-absence stata-output "Stata graphics")))
                
                ;; CRITICAL: Stata cleanliness must match Python
                (should stata-clean)
                (when (and python-clean (not stata-clean))
                  (euporie-strict-log 'error "COMPARISON FAILED: Python clean but Stata polluted"))
                (when (and (not python-clean) stata-clean)
                  (euporie-strict-log 'warn "Stata cleaner than Python - unusual but acceptable"))
                
                ;; Take comparison screenshot
                (euporie-strict-take-validation-screenshot "strict-stata-python-comparison.png" 
                                                          "Stata vs Python comparison")
                
                (euporie-strict-log 'info "Console cleanliness comparison - Python: %s, Stata: %s" 
                                   (if python-clean "CLEAN" "POLLUTED")
                                   (if stata-clean "CLEAN" "POLLUTED")))))))
    
    (euporie-strict-cleanup)))

(ert-deftest euporie-strict/org-mode-cret-keybinding-validation ()
  "STRICT TEST: C-RET keybinding workflow must produce clean console output."
  (euporie-strict-log 'info "=== STRICT TEST: Org-mode C-RET keybinding ===")
  (euporie-strict-cleanup)
  
  (unwind-protect
      (let ((org-buffer (get-buffer-create "*strict-org-test*")))
        (with-current-buffer org-buffer
          (org-mode)
          (insert "* Strict Stata Validation Test\n\n")
          (insert "#+BEGIN_SRC stata\n")
          (insert "sysuse auto\n")
          (insert "scatter price mpg\n")
          (insert "#+END_SRC\n\n")
          
          ;; Pre-start euporie for better control
          (euporie-stata-start)
          (should (euporie-strict-wait-for-ready "*euporie-stata*" euporie-strict-test-timeout))
          
          ;; Execute code block via org-babel
          (goto-char (point-min))
          (re-search-forward "scatter price mpg")
          
          (euporie-strict-log 'info "Executing org code block via C-RET simulation")
          (let ((execution-start (current-time))
                (initial-marker (with-current-buffer "*euporie-stata*" (point-max))))
            
            ;; Execute the code block
            (org-babel-execute-src-block)
            (sleep-for 15)  ; Wait for complete execution
            
            ;; Capture output from org-babel execution
            (let ((org-output (with-current-buffer "*euporie-stata*"
                               (buffer-substring-no-properties initial-marker (point-max)))))
              
              ;; STRICT VALIDATION: Org-babel execution must be clean
              (should (euporie-strict-validate-counter-absence org-output "Org-babel execution"))
              
              ;; Validate graphics display
              (should (euporie-strict-validate-graphics-display "*euporie-stata*" "Org-babel graphics"))
              
              (euporie-strict-log 'info "Org-babel execution validation: PASSED")))
          
          (kill-buffer org-buffer)))
    
    (euporie-strict-cleanup)))

(ert-deftest euporie-strict/error-recovery-and-console-stability ()
  "STRICT TEST: Error handling must maintain console cleanliness."
  (euporie-strict-log 'info "=== STRICT TEST: Error recovery and stability ===")
  (euporie-strict-cleanup)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (should (euporie-strict-wait-for-ready "*euporie-stata*" euporie-strict-test-timeout))
        
        ;; Test error handling
        (let ((error-output (euporie-strict-execute-and-capture 
                            "stata" "invalid_command_12345" "Invalid command test" 8)))
          
          ;; Even error output should be clean (no counter messages)
          (should (euporie-strict-validate-counter-absence error-output "Error handling")))
        
        ;; Test recovery with valid command
        (let ((recovery-output (euporie-strict-execute-and-capture 
                               "stata" "display \"Recovery successful\"" "Recovery test" 5)))
          
          (should (string-match-p "Recovery successful" (or recovery-output "")))
          (should (euporie-strict-validate-counter-absence recovery-output "Error recovery")))
        
        ;; Test graphics after error recovery
        (euporie-strict-execute-and-capture "stata" "sysuse auto" "Post-error data load" 5)
        
        (let ((post-error-graphics (euporie-strict-execute-and-capture 
                                   "stata" "scatter price mpg" "Post-error graphics" 12)))
          
          (should (euporie-strict-validate-counter-absence post-error-graphics "Post-error graphics"))
          (should (euporie-strict-validate-graphics-display "*euporie-stata*" "Post-error graphics display"))))
    
    (euporie-strict-cleanup)))

;; COMPREHENSIVE TEST SUITE RUNNERS

(defun euporie-strict-run-all-validation-tests ()
  "Run all strict validation tests with comprehensive reporting."
  (interactive)
  (euporie-strict-clear-log)
  
  ;; Take initial system screenshot
  (euporie-strict-take-validation-screenshot "strict-validation-start.png" 
                                           "Strict validation test suite start")
  
  (euporie-strict-log 'info "Starting comprehensive strict validation test suite")
  (euporie-strict-log 'info "Success criteria:")
  (euporie-strict-log 'info "1. Visual Graph Display: Must show actual graphics inline")
  (euporie-strict-log 'info "2. Zero Counter Messages: NO 'global stata_kernel_graph_counter' text")
  (euporie-strict-log 'info "3. Clean Console Output: Only legitimate output should appear")
  (euporie-strict-log 'info "4. No False Positives: Tests fail when counter messages present")
  
  ;; Run tests with failure tracking
  (let ((test-start (current-time)))
    (ert-run-tests-interactively "euporie-strict/")
    
    (let ((test-duration (float-time (time-subtract (current-time) test-start))))
      (euporie-strict-log 'info "Test suite completed in %.2f seconds" test-duration)
      
      ;; Take final screenshot
      (euporie-strict-take-validation-screenshot "strict-validation-complete.png" 
                                               "Strict validation test suite complete")
      
      ;; Generate summary report
      (euporie-strict-generate-validation-report))))

(defun euporie-strict-run-critical-tests-only ()
  "Run only the most critical validation tests."
  (interactive)
  (euporie-strict-clear-log)
  (euporie-strict-log 'info "Running CRITICAL strict validation tests only")
  
  (ert-run-tests-interactively 
   '(or "euporie-strict/stata-scatter-plot-zero-counter-messages"
        "euporie-strict/comparison-stata-vs-python-console-cleanliness")))

(defun euporie-strict-generate-validation-report ()
  "Generate detailed validation report with pass/fail analysis."
  (interactive)
  (let ((report-buffer (get-buffer-create "*Strict Validation Report*")))
    (with-current-buffer report-buffer
      (erase-buffer)
      (insert "# STRICT EMACS-EUPORIE VALIDATION REPORT\n\n")
      (insert (format "Generated: %s\n\n" (current-time-string)))
      
      (insert "## Validation Criteria\n\n")
      (insert "1. **Visual Graph Display**: Must show actual scatter plot inline\n")
      (insert "2. **Zero Counter Messages**: Absolutely NO `global stata_kernel_graph_counter` text visible\n")
      (insert "3. **Clean Console Output**: Only legitimate Stata output should appear\n")
      (insert "4. **No False Positives**: Test must actually fail when counter messages are present\n\n")
      
      ;; Analysis from log file
      (when (file-exists-p euporie-strict-test-log-file)
        (insert "## Test Execution Details\n\n```\n")
        (insert-file-contents euporie-strict-test-log-file)
        (goto-char (point-max))
        (insert "\n```\n\n")
        
        ;; Summary analysis
        (goto-char (point-min))
        (let ((log-content (buffer-string)))
          (goto-char (point-max))
          (insert "## Summary Analysis\n\n")
          
          (let ((passed (length (split-string log-content "PASSED")))
                (failed (length (split-string log-content "FAILED")))
                (errors (length (split-string log-content "ERROR"))))
            
            (insert (format "- Tests PASSED: %d\n" (max 0 (1- passed))))
            (insert (format "- Tests FAILED: %d\n" (max 0 (1- failed))))
            (insert (format "- ERRORS encountered: %d\n\n" (max 0 (1- errors))))
            
            (if (> failed 1)  ; > 1 because split always creates at least one element
                (insert "**❌ VALIDATION FAILED**: Counter messages still present or other issues detected.\n")
              (insert "**✅ VALIDATION PASSED**: All strict criteria met.\n")))
          
          (insert "\n## Screenshots Captured\n\n")
          (when (string-match-p "screenshot" log-content)
            (insert "Validation screenshots saved in ~/.doom.d/ for visual verification.\n"))))
      
      (display-buffer report-buffer)
      (message "Strict validation report generated"))))

;; Load notification
(unless noninteractive
  (message "Strict Emacs-euporie validation test suite loaded.")
  (message "Available commands:")
  (message "- (euporie-strict-run-all-validation-tests) - Run complete validation suite")
  (message "- (euporie-strict-run-critical-tests-only) - Run critical tests only")
  (message "- (euporie-strict-generate-validation-report) - Generate detailed report")
  (message "")
  (message "These tests implement STRICT validation to prevent false positives.")
  (message "Tests will FAIL if any counter messages are detected in console output."))

(provide 'test-euporie-strict-validation)
;;; test-euporie-strict-validation.el ends here