;;; test-euporie-stata-integration.el --- Integration tests for complete Stata euporie workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; End-to-end integration tests that validate the complete user experience
;; for Stata kernel graphics in euporie console. These tests simulate actual
;; user workflows and validate that the fixes achieve professional-grade
;; console behavior matching Python/R kernels.
;;
;; Key Integration Scenarios:
;; 1. Fresh euporie startup -> data loading -> graphics creation
;; 2. Interactive C-RET workflows from org-mode code blocks  
;; 3. Multiple graphics sessions with clean console output
;; 4. Error handling and recovery scenarios
;; 5. Performance under realistic usage patterns
;;
;; These tests validate the complete stack:
;; - euporie-termint.el integration
;; - stata_kernel fixes (no counter messages)
;; - Terminal graphics protocols (sixel/kitty)
;; - Buffer management and process handling

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'euporie-termint nil t)
(require 'org)

;; Integration test configuration
(defvar euporie-stata-integration-log-file (expand-file-name "euporie-stata-integration.log" "~/")
  "Log file for integration tests.")

(defvar euporie-stata-integration-timeout 45
  "Timeout for integration tests.")

(defvar euporie-stata-integration-screenshot-dir (expand-file-name "~/.doom.d/")
  "Directory for test screenshots.")

;; Integration test utilities
(defun euporie-stata-integration-log (level format-string &rest args)
  "Log integration test message with LEVEL, FORMAT-STRING and ARGS."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-stata-integration-log-file))))

(defun euporie-stata-integration-clear-log ()
  "Clear integration test log."
  (when (file-exists-p euporie-stata-integration-log-file)
    (delete-file euporie-stata-integration-log-file)))

(defun euporie-stata-integration-wait-for-buffer-ready (buffer-name timeout)
  "Wait for buffer to be ready for interaction."
  (let ((start-time (current-time))
        (ready nil))
    (while (and (not ready)
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (let ((process (get-buffer-process (current-buffer))))
            (when (and process (process-live-p process))
              ;; Test with a simple command
              (let ((test-marker (point-max)))
                (process-send-string process "display \"test\"\n")
                (sleep-for 3)
                (let ((output (buffer-substring-no-properties test-marker (point-max))))
                  (when (string-match-p "test" output)
                    (setq ready t))))))))
      (unless ready
        (sleep-for 1)))
    ready))

(defun euporie-stata-integration-capture-full-session (buffer-name)
  "Capture the complete session output from buffer."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun euporie-stata-integration-send-code-block (kernel code description)
  "Send code block to kernel and return session output."
  (let* ((buffer-name (format "*euporie-%s*" kernel))
         (start-marker (when (get-buffer buffer-name)
                        (with-current-buffer buffer-name (point-max)))))
    
    (euporie-stata-integration-log 'info "Sending %s: %s" description code)
    
    ;; Use the euporie-termint integration
    (euporie-termint-send-code kernel code)
    
    ;; Wait for execution
    (sleep-for 8)
    
    ;; Capture output since start
    (when (and (get-buffer buffer-name) start-marker)
      (with-current-buffer buffer-name
        (let ((output (buffer-substring-no-properties start-marker (point-max))))
          (euporie-stata-integration-log 'info "%s output: %S" description output)
          output)))))

(defun euporie-stata-integration-take-screenshot (filename description)
  "Take screenshot for integration test documentation."
  (let ((screenshot-path (expand-file-name filename euporie-stata-integration-screenshot-dir)))
    (euporie-stata-integration-log 'info "Taking screenshot: %s - %s" filename description)
    (shell-command (format "screencapture -x %s" screenshot-path))
    (sleep-for 1)
    screenshot-path))

(defun euporie-stata-integration-cleanup-all ()
  "Complete cleanup for integration tests."
  (dolist (buffer-name '("*euporie-stata*" "*euporie-python*" "*euporie-r*" 
                        "*test-org-stata*" "*integration-test*"))
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name))))
  (sleep-for 2))

(defun euporie-stata-integration-check-console-cleanliness (output description)
  "Check output for console cleanliness and log results."
  (let ((has-counter-messages (and output
                                  (stringp output)
                                  (or (string-match-p "global stata_kernel_graph_counter" output)
                                      (string-match-p "stata_kernel_graph_counter.*=" output)
                                      (string-match-p "graph_counter.*\\+.*1" output)))))
    
    (euporie-stata-integration-log 'info "%s console cleanliness: %s" 
                                  description 
                                  (if has-counter-messages "FAILED" "PASSED"))
    
    (not has-counter-messages)))

;;; Core Integration Tests

(ert-deftest euporie-stata-integration/complete-user-workflow ()
  "Test complete user workflow from fresh start to graphics display."
  (euporie-stata-integration-log 'info "Testing complete user workflow")
  (euporie-stata-integration-cleanup-all)
  
  (unwind-protect
      (let ((workflow-start (current-time)))
        
        ;; Step 1: Start fresh euporie console
        (euporie-stata-integration-log 'info "Step 1: Starting euporie console")
        (euporie-stata-start)
        (should (euporie-stata-integration-wait-for-buffer-ready "*euporie-stata*" 
                                                                euporie-stata-integration-timeout))
        
        ;; Take initial screenshot
        (euporie-stata-integration-take-screenshot "stata-integration-step1-startup.png" 
                                                  "Fresh euporie startup")
        
        ;; Step 2: Load data
        (euporie-stata-integration-log 'info "Step 2: Loading dataset")
        (let ((data-output (euporie-stata-integration-send-code-block 
                           "stata" "sysuse auto" "Data loading")))
          
          (should (euporie-stata-integration-check-console-cleanliness 
                   data-output "Data loading"))
          (should (string-match-p "auto.dta" (or data-output ""))))
        
        ;; Step 3: Create scatter plot
        (euporie-stata-integration-log 'info "Step 3: Creating scatter plot")
        (let ((plot-output (euporie-stata-integration-send-code-block 
                           "stata" "scatter price mpg" "Scatter plot creation")))
          
          ;; Primary validation: Console should be clean
          (should (euporie-stata-integration-check-console-cleanliness 
                   plot-output "Scatter plot"))
          
          ;; Should not contain counter messages
          (should-not (string-match-p "global stata_kernel_graph_counter" (or plot-output "")))
          (should-not (string-match-p "stata_kernel_graph_counter.*\\+" (or plot-output ""))))
        
        ;; Take screenshot after plot
        (euporie-stata-integration-take-screenshot "stata-integration-step3-plot.png" 
                                                  "After scatter plot creation")
        
        ;; Step 4: Verify console responsiveness
        (euporie-stata-integration-log 'info "Step 4: Testing console responsiveness")
        (let ((responsive-output (euporie-stata-integration-send-code-block 
                                 "stata" "display \"Workflow complete\"" "Responsiveness test")))
          
          (should (string-match-p "Workflow complete" (or responsive-output "")))
          (should (euporie-stata-integration-check-console-cleanliness 
                   responsive-output "Responsiveness test")))
        
        ;; Step 5: Multiple plots test
        (euporie-stata-integration-log 'info "Step 5: Testing multiple plots")
        (let ((hist-output (euporie-stata-integration-send-code-block 
                           "stata" "histogram price, bins(10)" "Histogram creation")))
          
          (should (euporie-stata-integration-check-console-cleanliness 
                   hist-output "Histogram")))
        
        ;; Final screenshot
        (euporie-stata-integration-take-screenshot "stata-integration-complete.png" 
                                                  "Complete workflow finished")
        
        ;; Record total workflow time
        (let ((total-time (float-time (time-subtract (current-time) workflow-start))))
          (euporie-stata-integration-log 'info "Complete workflow took %.2f seconds" total-time)
          
          ;; Should complete in reasonable time
          (should (< total-time 120))) ; 2 minutes max for complete workflow
        
        ;; Step 6: Verify graphics files created
        (let* ((cache-dir (expand-file-name "~/.stata_kernel_cache"))
               (png-files (when (file-directory-p cache-dir)
                           (directory-files cache-dir nil "\\.png$"))))
          (should (> (length png-files) 0))
          (euporie-stata-integration-log 'info "Created %d graphics files" (length png-files))))
    
    (euporie-stata-integration-cleanup-all)))

(ert-deftest euporie-stata-integration/c-ret-keybinding-workflow ()
  "Test C-RET keybinding workflow from org-mode code blocks."
  (euporie-stata-integration-log 'info "Testing C-RET keybinding workflow")
  (euporie-stata-integration-cleanup-all)
  
  (unwind-protect
      (progn
        ;; Create a test org buffer with Stata code blocks
        (let ((org-buffer (get-buffer-create "*integration-org-test*")))
          (with-current-buffer org-buffer
            (org-mode)
            (insert "* Test Stata Integration\n\n")
            (insert "#+BEGIN_SRC stata\n")
            (insert "sysuse auto\n")
            (insert "#+END_SRC\n\n")
            (insert "#+BEGIN_SRC stata\n")
            (insert "scatter price mpg\n")
            (insert "#+END_SRC\n\n")
            (insert "#+BEGIN_SRC stata\n")
            (insert "histogram price\n")
            (insert "#+END_SRC\n")
            
            ;; Save buffer for testing
            (write-file (expand-file-name "test-stata-integration.org" "~/")))
          
          ;; Test executing code blocks
          (with-current-buffer org-buffer
            ;; Go to first code block
            (goto-char (point-min))
            (re-search-forward "sysuse auto")
            
            ;; Start euporie session first
            (euporie-stata-start)
            (should (euporie-stata-integration-wait-for-buffer-ready "*euporie-stata*" 
                                                                    euporie-stata-integration-timeout))
            
            ;; Execute via org-babel (simulating C-RET)
            (euporie-stata-integration-log 'info "Executing first code block via org-babel")
            (let ((first-result (org-babel-execute-src-block)))
              (euporie-stata-integration-log 'info "First code block result: %S" first-result))
            
            ;; Wait and check console output
            (sleep-for 5)
            (let ((console-content (euporie-stata-integration-capture-full-session "*euporie-stata*")))
              (should (euporie-stata-integration-check-console-cleanliness 
                       console-content "First code block execution")))
            
            ;; Execute second code block (graphics)
            (re-search-forward "scatter price mpg")
            (euporie-stata-integration-log 'info "Executing scatter plot via org-babel")
            (let ((second-result (org-babel-execute-src-block)))
              (euporie-stata-integration-log 'info "Second code block result: %S" second-result))
            
            ;; Wait and validate
            (sleep-for 8)
            (let ((console-content (euporie-stata-integration-capture-full-session "*euporie-stata*")))
              ;; Should be clean with no counter messages
              (should (euporie-stata-integration-check-console-cleanliness 
                       console-content "Graphics code block execution"))
              (should-not (string-match-p "stata_kernel_graph_counter" (or console-content ""))))
            
            ;; Take screenshot of org-mode integration
            (euporie-stata-integration-take-screenshot "stata-org-integration.png" 
                                                      "Org-mode integration result")
            
            ;; Clean up test file
            (delete-file (expand-file-name "test-stata-integration.org" "~/"))
            (kill-buffer org-buffer))))
    
    (euporie-stata-integration-cleanup-all)))

(ert-deftest euporie-stata-integration/comparison-with-python-r ()
  "Integration test comparing Stata behavior with Python and R kernels."
  (euporie-stata-integration-log 'info "Comparing Stata with Python/R kernel behavior")
  (euporie-stata-integration-cleanup-all)
  
  (unwind-protect
      (progn
        ;; Test Python graphics (baseline)
        (euporie-stata-integration-log 'info "Testing Python graphics baseline")
        (euporie-python-start)
        (should (euporie-stata-integration-wait-for-buffer-ready "*euporie-python*" 
                                                                euporie-stata-integration-timeout))
        
        (let ((python-output (euporie-stata-integration-send-code-block 
                              "python" 
                              "import matplotlib.pyplot as plt\nimport numpy as np\nx = np.linspace(0, 10, 100)\ny = np.sin(x)\nplt.plot(x, y)\nplt.title('Python Test Plot')\nplt.show()"
                              "Python graphics test")))
          
          (euporie-stata-integration-log 'info "Python console cleanliness: %s" 
                                        (if (euporie-stata-integration-check-console-cleanliness 
                                             python-output "Python graphics")
                                            "PASSED" "FAILED")))
        
        ;; Test R graphics (baseline)
        (euporie-stata-integration-log 'info "Testing R graphics baseline")
        (euporie-r-start)
        (should (euporie-stata-integration-wait-for-buffer-ready "*euporie-r*" 
                                                               euporie-stata-integration-timeout))
        
        (let ((r-output (euporie-stata-integration-send-code-block 
                         "r"
                         "library(ggplot2)\ndata(mtcars)\nggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() + ggtitle('R Test Plot')"
                         "R graphics test")))
          
          (euporie-stata-integration-log 'info "R console cleanliness: %s" 
                                        (if (euporie-stata-integration-check-console-cleanliness 
                                             r-output "R graphics")
                                            "PASSED" "FAILED")))
        
        ;; Test Stata graphics (should match baseline behavior)
        (euporie-stata-integration-log 'info "Testing Stata graphics (comparison)")
        (euporie-stata-start)
        (should (euporie-stata-integration-wait-for-buffer-ready "*euporie-stata*" 
                                                                euporie-stata-integration-timeout))
        
        (euporie-stata-integration-send-code-block "stata" "sysuse auto" "Stata data loading")
        
        (let ((stata-output (euporie-stata-integration-send-code-block 
                            "stata"
                            "scatter price mpg, title(\"Stata Test Plot\")"
                            "Stata graphics test")))
          
          ;; Stata should be as clean as Python/R
          (should (euporie-stata-integration-check-console-cleanliness 
                   stata-output "Stata graphics"))
          
          ;; Specific checks for Stata cleanliness
          (should-not (string-match-p "global.*stata_kernel_graph_counter" (or stata-output "")))
          (should-not (string-match-p "graph_counter.*\\+" (or stata-output ""))))
        
        ;; Take comparison screenshot
        (euporie-stata-integration-take-screenshot "stata-python-r-comparison.png" 
                                                  "Three kernel comparison"))
    
    (euporie-stata-integration-cleanup-all)))

(ert-deftest euporie-stata-integration/error-handling-and-recovery ()
  "Test error handling and recovery scenarios."
  (euporie-stata-integration-log 'info "Testing error handling and recovery")
  (euporie-stata-integration-cleanup-all)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (should (euporie-stata-integration-wait-for-buffer-ready "*euporie-stata*" 
                                                                euporie-stata-integration-timeout))
        
        ;; Test invalid command handling
        (euporie-stata-integration-log 'info "Testing invalid command handling")
        (let ((error-output (euporie-stata-integration-send-code-block 
                             "stata" "invalid_command_xyz" "Invalid command test")))
          
          ;; Should handle errors gracefully
          (should error-output) ; Should get some output
          (should (euporie-stata-integration-check-console-cleanliness 
                   error-output "Error handling")))
        
        ;; Test recovery with valid command
        (euporie-stata-integration-log 'info "Testing recovery after error")
        (let ((recovery-output (euporie-stata-integration-send-code-block 
                               "stata" "display \"Recovery test\"" "Recovery test")))
          
          (should (string-match-p "Recovery test" (or recovery-output "")))
          (should (euporie-stata-integration-check-console-cleanliness 
                   recovery-output "Recovery after error")))
        
        ;; Test graphics after error recovery
        (euporie-stata-integration-send-code-block "stata" "sysuse auto" "Data after recovery")
        (let ((graphics-after-error (euporie-stata-integration-send-code-block 
                                     "stata" "scatter price mpg" "Graphics after error")))
          
          (should (euporie-stata-integration-check-console-cleanliness 
                   graphics-after-error "Graphics after error recovery"))))
    
    (euporie-stata-integration-cleanup-all)))

(ert-deftest euporie-stata-integration/performance-under-load ()
  "Test performance under realistic usage patterns."
  (euporie-stata-integration-log 'info "Testing performance under load")
  (euporie-stata-integration-cleanup-all)
  
  (unwind-protect
      (let ((load-start (current-time)))
        (euporie-stata-start)
        (should (euporie-stata-integration-wait-for-buffer-ready "*euporie-stata*" 
                                                                euporie-stata-integration-timeout))
        
        (euporie-stata-integration-send-code-block "stata" "sysuse auto" "Data for load test")
        
        ;; Create multiple plots in sequence (realistic usage)
        (let ((plot-commands '("scatter price mpg"
                              "histogram price, bins(15)" 
                              "twoway scatter price mpg || lfit price mpg"
                              "scatter price weight"
                              "histogram mpg, bins(10)"
                              "twoway scatter price mpg, by(foreign)"))
              (all-clean t))
          
          (dolist (command plot-commands)
            (let ((output (euporie-stata-integration-send-code-block 
                          "stata" command (format "Load test: %s" command))))
              
              (unless (euporie-stata-integration-check-console-cleanliness 
                       output (format "Load test: %s" command))
                (setq all-clean nil))))
          
          ;; All plots should maintain clean console
          (should all-clean)
          
          ;; Total time should be reasonable
          (let ((total-time (float-time (time-subtract (current-time) load-start))))
            (euporie-stata-integration-log 'info "Load test completed in %.2f seconds" total-time)
            (should (< total-time 180))) ; 3 minutes max for 6 plots
          
          ;; Console should still be responsive
          (let ((final-test (euporie-stata-integration-send-code-block 
                            "stata" "summarize price mpg" "Final responsiveness")))
            (should (string-match-p "Variable.*Mean" (or final-test ""))))))
    
    (euporie-stata-integration-cleanup-all)))

;;; Integration Test Suite Runners

(defun euporie-stata-run-integration-tests ()
  "Run all Stata euporie integration tests."
  (interactive)
  (euporie-stata-integration-clear-log)
  (euporie-stata-integration-log 'info "Starting complete Stata euporie integration test suite")
  
  ;; Take initial system screenshot
  (euporie-stata-integration-take-screenshot "stata-integration-start.png" 
                                            "Integration test suite start")
  
  (ert-run-tests-interactively "euporie-stata-integration/"))

(defun euporie-stata-run-critical-integration-tests ()
  "Run critical integration tests only."
  (interactive)
  (euporie-stata-integration-clear-log)
  (euporie-stata-integration-log 'info "Starting critical Stata euporie integration tests")
  
  (ert-run-tests-interactively 
   '(or "euporie-stata-integration/complete-user-workflow"
        "euporie-stata-integration/comparison-with-python-r")))

(defun euporie-stata-integration-generate-report ()
  "Generate comprehensive integration test report."
  (interactive)
  (when (file-exists-p euporie-stata-integration-log-file)
    (let ((report-buffer (get-buffer-create "*Stata Integration Report*")))
      (with-current-buffer report-buffer
        (erase-buffer)
        (insert "# Stata Euporie Integration Test Report\n\n")
        (insert (format "Generated: %s\n\n" (current-time-string)))
        
        ;; Insert log contents
        (insert "## Test Execution Log\n\n```\n")
        (insert-file-contents euporie-stata-integration-log-file)
        (goto-char (point-max))
        (insert "\n```\n\n")
        
        ;; Summary section
        (goto-char (point-min))
        (let ((log-content (buffer-string)))
          (goto-char (point-max))
          (insert "## Summary\n\n")
          
          (let ((passed-count (length (split-string log-content "console cleanliness: PASSED")))
                (failed-count (length (split-string log-content "console cleanliness: FAILED"))))
            (insert (format "- Console cleanliness tests passed: %d\n" (1- passed-count)))
            (insert (format "- Console cleanliness tests failed: %d\n" (1- failed-count))))
          
          (when (string-match-p "Complete workflow took" log-content)
            (insert "- Complete workflow test: EXECUTED\n"))
          
          (when (string-match-p "screenshot" log-content)
            (insert "- Screenshots captured for documentation\n")))
        
        (display-buffer report-buffer)))))

;; Provide helpful information when loaded
(unless noninteractive
  (message "Stata euporie integration test suite loaded.")
  (message "Available commands:")
  (message "- (euporie-stata-run-integration-tests) - Run all integration tests")
  (message "- (euporie-stata-run-critical-integration-tests) - Run critical tests")
  (message "- (euporie-stata-integration-generate-report) - Generate test report"))

(provide 'test-euporie-stata-integration)
;;; test-euporie-stata-integration.el ends here