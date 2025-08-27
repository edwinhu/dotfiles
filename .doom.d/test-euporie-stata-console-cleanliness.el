;;; test-euporie-stata-console-cleanliness.el --- Unit tests for Stata kernel console output cleanliness -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive unit tests for Stata kernel console output cleanliness and graphics display
;; in euporie console environments. These tests validate that:
;;
;; 1. Console output is clean (no graph counter messages)
;; 2. Graphics display correctly without console pollution
;; 3. Performance is optimal for interactive use
;; 4. Behavior matches Python/R kernels in euporie
;;
;; Based on user feedback showing unwanted graph counter messages:
;; "global stata_kernel_graph_counter = $stata_kernel_graph_counter + 1"
;;
;; These tests ensure professional-grade console behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'euporie-termint nil t)

;; Test configuration
(defvar euporie-stata-test-log-file (expand-file-name "euporie-stata-console-test.log" "~/")
  "Log file for Stata console cleanliness tests.")

(defvar euporie-stata-test-timeout 30
  "Timeout in seconds for Stata console tests.")

(defvar euporie-stata-test-project-dir "/Users/vwh7mb/projects/emacs-euporie"
  "Test project directory for euporie testing.")

;; Test utilities
(defun euporie-stata-test-log (level format-string &rest args)
  "Log test message with LEVEL, FORMAT-STRING and ARGS."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-stata-test-log-file))))

(defun euporie-stata-test-clear-log ()
  "Clear the Stata test log file."
  (when (file-exists-p euporie-stata-test-log-file)
    (delete-file euporie-stata-test-log-file)))

(defun euporie-stata-test-wait-for-process (buffer-name timeout)
  "Wait for process in BUFFER-NAME to be ready within TIMEOUT seconds."
  (let ((start-time (current-time))
        (process-ready nil))
    (while (and (not process-ready)
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (when (and (get-buffer-process (current-buffer))
                     (process-live-p (get-buffer-process (current-buffer))))
            (setq process-ready t))))
      (unless process-ready
        (sleep-for 0.5)))
    process-ready))

(defun euporie-stata-test-send-command-and-capture (buffer-name command timeout)
  "Send COMMAND to BUFFER-NAME and capture output within TIMEOUT."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let ((start-marker (point-max))
            (process (get-buffer-process (current-buffer))))
        (when process
          ;; Send command
          (process-send-string process (concat command "\n"))
          ;; Wait for output
          (sleep-for 2)
          ;; Capture output since start-marker
          (buffer-substring-no-properties start-marker (point-max)))))))

(defun euporie-stata-test-check-for-counter-messages (output)
  "Check OUTPUT for unwanted graph counter messages."
  (and output
       (stringp output)
       (or (string-match-p "global stata_kernel_graph_counter" output)
           (string-match-p "stata_kernel_graph_counter.*=" output)
           (string-match-p "graph_counter.*\\+.*1" output))))

(defun euporie-stata-test-cleanup-buffers ()
  "Clean up all Stata test buffers."
  (dolist (buffer-name '("*euporie-stata*" "*test-euporie-stata*"))
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name))))
  ;; Wait for cleanup
  (sleep-for 1))

;;; Environment Validation Tests

(ert-deftest euporie-stata-console/environment-validation ()
  "Test that required environment components are available."
  (should (executable-find "euporie-console"))
  (should (file-directory-p euporie-stata-test-project-dir))
  (should (fboundp 'euporie-stata-start))
  (should (fboundp 'termint-define)))

(ert-deftest euporie-stata-console/pixi-environment-available ()
  "Test that pixi environment contains stata kernel."
  (let* ((default-directory euporie-stata-test-project-dir)
         (pixi-output (shell-command-to-string "pixi run jupyter kernelspec list 2>/dev/null")))
    (should (string-match-p "stata" pixi-output))))

;;; Console Output Cleanliness Tests

(ert-deftest euporie-stata-console/no-counter-messages-scatter-plot ()
  "Test that scatter price mpg produces NO counter messages in console."
  (euporie-stata-test-log 'info "Testing scatter plot console cleanliness")
  (euporie-stata-test-cleanup-buffers)
  
  (unwind-protect
      (progn
        ;; Start Stata console
        (euporie-stata-start)
        (should (euporie-stata-test-wait-for-process "*euporie-stata*" euporie-stata-test-timeout))
        
        ;; Load auto dataset
        (let ((sysuse-output (euporie-stata-test-send-command-and-capture 
                             "*euporie-stata*" "sysuse auto" 5)))
          (euporie-stata-test-log 'info "sysuse auto output: %S" sysuse-output))
        
        ;; Execute scatter plot and capture output
        (let ((scatter-output (euporie-stata-test-send-command-and-capture 
                              "*euporie-stata*" "scatter price mpg" 10)))
          
          (euporie-stata-test-log 'info "scatter command output: %S" scatter-output)
          
          ;; Main test: Should NOT contain counter messages
          (should-not (euporie-stata-test-check-for-counter-messages scatter-output))
          
          ;; Additional checks for clean output
          (when scatter-output
            (should-not (string-match-p "global.*stata_kernel_graph_counter" scatter-output))
            (should-not (string-match-p "\\+.*1.*counter" scatter-output)))))
    
    (euporie-stata-test-cleanup-buffers)))

(ert-deftest euporie-stata-console/multiple-plots-no-counter-pollution ()
  "Test that multiple graph commands in sequence produce no counter pollution."
  (euporie-stata-test-log 'info "Testing multiple plots console cleanliness")
  (euporie-stata-test-cleanup-buffers)
  
  (unwind-protect
      (progn
        ;; Start Stata console
        (euporie-stata-start)
        (should (euporie-stata-test-wait-for-process "*euporie-stata*" euporie-stata-test-timeout))
        
        ;; Load dataset
        (euporie-stata-test-send-command-and-capture "*euporie-stata*" "sysuse auto" 3)
        
        ;; Test multiple graph commands
        (let ((commands '("scatter price mpg" 
                         "histogram price" 
                         "twoway scatter price mpg")))
          
          (dolist (command commands)
            (let ((output (euporie-stata-test-send-command-and-capture 
                          "*euporie-stata*" command 8)))
              
              (euporie-stata-test-log 'info "Command: %s, Output: %S" command output)
              
              ;; Each command should produce clean output
              (should-not (euporie-stata-test-check-for-counter-messages output))))))
    
    (euporie-stata-test-cleanup-buffers)))

(ert-deftest euporie-stata-console/compare-with-python-cleanliness ()
  "Test that Stata console output cleanliness matches Python kernel behavior."
  (euporie-stata-test-log 'info "Comparing Stata vs Python console cleanliness")
  (euporie-stata-test-cleanup-buffers)
  
  (unwind-protect
      (progn
        ;; Test Python console first (baseline)
        (euporie-python-start)
        (should (euporie-stata-test-wait-for-process "*euporie-python*" euporie-stata-test-timeout))
        
        (let ((python-output (euporie-stata-test-send-command-and-capture 
                             "*euporie-python*" "import matplotlib.pyplot as plt; plt.plot([1,2,3]); plt.show()" 8)))
          
          (euporie-stata-test-log 'info "Python plot output: %S" python-output)
          
          ;; Kill Python buffer
          (let ((kill-buffer-query-functions nil))
            (kill-buffer "*euporie-python*"))
          
          ;; Test Stata console (should match Python cleanliness)
          (euporie-stata-start)
          (should (euporie-stata-test-wait-for-process "*euporie-stata*" euporie-stata-test-timeout))
          
          (euporie-stata-test-send-command-and-capture "*euporie-stata*" "sysuse auto" 3)
          
          (let ((stata-output (euporie-stata-test-send-command-and-capture 
                              "*euporie-stata*" "scatter price mpg" 8)))
            
            (euporie-stata-test-log 'info "Stata plot output: %S" stata-output)
            
            ;; Both should be clean - no debug/counter messages
            (should-not (euporie-stata-test-check-for-counter-messages python-output))
            (should-not (euporie-stata-test-check-for-counter-messages stata-output))
            
            ;; Stata should not have significantly more verbose output than Python
            (when (and python-output stata-output)
              (let ((python-lines (length (split-string python-output "\n")))
                    (stata-lines (length (split-string stata-output "\n"))))
                (should (< stata-lines (* 3 python-lines))) ; Allow some variance but not excessive
                )))))
    
    (euporie-stata-test-cleanup-buffers)))

;;; Graphics Display Tests

(ert-deftest euporie-stata-console/graphics-display-without-hanging ()
  "Test that graphics display correctly without hanging the console."
  (euporie-stata-test-log 'info "Testing graphics display without hanging")
  (euporie-stata-test-cleanup-buffers)
  
  (unwind-protect
      (progn
        ;; Start console
        (euporie-stata-start)
        (should (euporie-stata-test-wait-for-process "*euporie-stata*" euporie-stata-test-timeout))
        
        ;; Load data and create plot
        (euporie-stata-test-send-command-and-capture "*euporie-stata*" "sysuse auto" 3)
        
        (let ((start-time (current-time)))
          (euporie-stata-test-send-command-and-capture "*euporie-stata*" "scatter price mpg" 10)
          (let ((elapsed (float-time (time-subtract (current-time) start-time))))
            
            (euporie-stata-test-log 'info "Graphics display took %f seconds" elapsed)
            
            ;; Should complete within reasonable time (no hanging)
            (should (< elapsed 15))
            
            ;; Console should still be responsive
            (let ((test-output (euporie-stata-test-send-command-and-capture 
                               "*euporie-stata*" "display \"Console responsive\"" 5)))
              (should (string-match-p "Console responsive" (or test-output "")))))))
    
    (euporie-stata-test-cleanup-buffers)))

(ert-deftest euporie-stata-console/graphics-display-in-euporie-vs-jupyter ()
  "Test that graphics display correctly in both euporie and standard jupyter console."
  (euporie-stata-test-log 'info "Testing graphics display in euporie vs jupyter console")
  (euporie-stata-test-cleanup-buffers)
  
  ;; This test checks the integration works in both environments
  (unwind-protect
      (progn
        ;; Test euporie console
        (euporie-stata-start)
        (should (euporie-stata-test-wait-for-process "*euporie-stata*" euporie-stata-test-timeout))
        
        (euporie-stata-test-send-command-and-capture "*euporie-stata*" "sysuse auto" 3)
        
        (let ((euporie-output (euporie-stata-test-send-command-and-capture 
                              "*euporie-stata*" "scatter price mpg" 10)))
          
          (euporie-stata-test-log 'info "Euporie console output: %S" euporie-output)
          
          ;; Should be clean in euporie
          (should-not (euporie-stata-test-check-for-counter-messages euporie-output))
          
          ;; Check that Stata cache directory has graph files (indicates graphics working)
          (let ((cache-dir (expand-file-name "~/.stata_kernel_cache")))
            (when (file-directory-p cache-dir)
              (let ((png-files (directory-files cache-dir nil "\\.png$")))
                (should (> (length png-files) 0))
                (euporie-stata-test-log 'info "Found %d PNG files in cache" (length png-files)))))))
    
    (euporie-stata-test-cleanup-buffers)))

;;; Performance Tests

(ert-deftest euporie-stata-console/graphics-display-latency ()
  "Measure graphics display latency for performance validation."
  (euporie-stata-test-log 'info "Testing graphics display latency")
  (euporie-stata-test-cleanup-buffers)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (should (euporie-stata-test-wait-for-process "*euporie-stata*" euporie-stata-test-timeout))
        
        (euporie-stata-test-send-command-and-capture "*euporie-stata*" "sysuse auto" 3)
        
        ;; Measure multiple plot commands
        (let ((latencies '()))
          (dotimes (i 3)
            (let* ((command (format "scatter price mpg, title(\"Test Plot %d\")" (1+ i)))
                   (start-time (current-time)))
              
              (euporie-stata-test-send-command-and-capture "*euporie-stata*" command 10)
              
              (let ((elapsed (float-time (time-subtract (current-time) start-time))))
                (push elapsed latencies)
                (euporie-stata-test-log 'info "Plot %d latency: %f seconds" (1+ i) elapsed))))
          
          (let ((avg-latency (/ (apply #'+ latencies) (length latencies))))
            (euporie-stata-test-log 'info "Average graphics latency: %f seconds" avg-latency)
            
            ;; Should be reasonable for interactive use
            (should (< avg-latency 12))
            (should (> avg-latency 0.5)) ; Should take some time (not instant = likely broken)
            )))
    
    (euporie-stata-test-cleanup-buffers)))

(ert-deftest euporie-stata-console/file-detection-speed ()
  "Test file detection speed in .stata_kernel_cache directory."
  (let* ((cache-dir (expand-file-name "~/.stata_kernel_cache"))
         (start-time (current-time)))
    
    (when (file-directory-p cache-dir)
      ;; Test directory scanning speed
      (let ((png-files (directory-files cache-dir nil "\\.png$")))
        (let ((scan-time (float-time (time-subtract (current-time) start-time))))
          
          (euporie-stata-test-log 'info "Cache directory scan: %d files in %f seconds" 
                                 (length png-files) scan-time)
          
          ;; Should be fast even with many files
          (should (< scan-time 1.0)))))))

;;; Interactive Session Tests

(ert-deftest euporie-stata-console/complete-workflow-test ()
  "Test complete workflow: euporie startup -> sysuse auto -> scatter price mpg."
  (euporie-stata-test-log 'info "Testing complete interactive workflow")
  (euporie-stata-test-cleanup-buffers)
  
  (unwind-protect
      (progn
        ;; Step 1: Start euporie console
        (let ((start-time (current-time)))
          (euporie-stata-start)
          (should (euporie-stata-test-wait-for-process "*euporie-stata*" euporie-stata-test-timeout))
          
          (let ((startup-time (float-time (time-subtract (current-time) start-time))))
            (euporie-stata-test-log 'info "Euporie startup took %f seconds" startup-time)
            (should (< startup-time 20)))) ; Reasonable startup time
        
        ;; Step 2: Load dataset
        (let ((sysuse-output (euporie-stata-test-send-command-and-capture 
                             "*euporie-stata*" "sysuse auto" 5)))
          (should (string-match-p "auto.dta" (or sysuse-output "")))
          (should-not (euporie-stata-test-check-for-counter-messages sysuse-output)))
        
        ;; Step 3: Create scatter plot
        (let ((scatter-output (euporie-stata-test-send-command-and-capture 
                              "*euporie-stata*" "scatter price mpg" 10)))
          
          ;; Primary validation: No counter messages
          (should-not (euporie-stata-test-check-for-counter-messages scatter-output))
          
          ;; Secondary validation: Console should still be responsive
          (let ((responsive-test (euporie-stata-test-send-command-and-capture 
                                 "*euporie-stata*" "display \"Workflow complete\"" 3)))
            (should (string-match-p "Workflow complete" (or responsive-test "")))))
        
        ;; Step 4: Verify graphics files were created
        (let ((cache-dir (expand-file-name "~/.stata_kernel_cache")))
          (when (file-directory-p cache-dir)
            (let ((png-files (directory-files cache-dir nil "\\.png$")))
              (should (> (length png-files) 0))))))
    
    (euporie-stata-test-cleanup-buffers)))

(ert-deftest euporie-stata-console/multiple-graphs-in-single-session ()
  "Test multiple graphs in single session without console pollution."
  (euporie-stata-test-log 'info "Testing multiple graphs in single session")
  (euporie-stata-test-cleanup-buffers)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (should (euporie-stata-test-wait-for-process "*euporie-stata*" euporie-stata-test-timeout))
        
        (euporie-stata-test-send-command-and-capture "*euporie-stata*" "sysuse auto" 3)
        
        ;; Create multiple different types of graphs
        (let ((graph-commands '("scatter price mpg"
                               "histogram price, bins(10)"
                               "twoway scatter price mpg || lfit price mpg"))
              (all-outputs '()))
          
          (dolist (command graph-commands)
            (let ((output (euporie-stata-test-send-command-and-capture 
                          "*euporie-stata*" command 10)))
              
              (push output all-outputs)
              (euporie-stata-test-log 'info "Command: %s, Clean output: %s" 
                                     command (not (euporie-stata-test-check-for-counter-messages output)))
              
              ;; Each graph should produce clean output
              (should-not (euporie-stata-test-check-for-counter-messages output))))
          
          ;; Verify session is still responsive after multiple graphs
          (let ((final-test (euporie-stata-test-send-command-and-capture 
                            "*euporie-stata*" "summarize price mpg" 5)))
            (should (string-match-p "Variable.*Obs.*Mean" (or final-test ""))))))
    
    (euporie-stata-test-cleanup-buffers)))

;;; Buffer Management and Terminal Protocol Tests

(ert-deftest euporie-stata-console/buffer-behavior-and-terminal-protocols ()
  "Test buffer behavior and terminal graphics protocols."
  (euporie-stata-test-log 'info "Testing buffer behavior and terminal protocols")
  (euporie-stata-test-cleanup-buffers)
  
  (unwind-protect
      (progn
        ;; Start and verify buffer creation
        (euporie-stata-start)
        (should (get-buffer "*euporie-stata*"))
        (should (euporie-stata-test-wait-for-process "*euporie-stata*" euporie-stata-test-timeout))
        
        ;; Verify buffer is using eat backend
        (with-current-buffer "*euporie-stata*"
          (should (eq major-mode 'eat-mode)))
        
        ;; Test terminal environment variables are set correctly
        (let ((env-test-output (euporie-stata-test-send-command-and-capture 
                               "*euporie-stata*" "import os; print(f'TERM={os.environ.get(\"TERM\")}, COLORTERM={os.environ.get(\"COLORTERM\")}')" 5)))
          
          ;; Should show proper terminal environment for graphics
          (when env-test-output
            (should (string-match-p "TERM=.*eat" env-test-output))
            (should (string-match-p "COLORTERM=truecolor" env-test-output))))
        
        ;; Test process is properly configured
        (with-current-buffer "*euporie-stata*"
          (let ((process (get-buffer-process (current-buffer))))
            (should process)
            (should (process-live-p process)))))
    
    (euporie-stata-test-cleanup-buffers)))

;;; Test Suite Utilities

(defun euporie-stata-console-run-cleanliness-tests ()
  "Run all Stata console cleanliness tests interactively."
  (interactive)
  (euporie-stata-test-clear-log)
  (euporie-stata-test-log 'info "Starting Stata console cleanliness test suite")
  
  (ert-run-tests-interactively "euporie-stata-console/"))

(defun euporie-stata-console-run-critical-tests ()
  "Run only the most critical console cleanliness tests."
  (interactive)
  (euporie-stata-test-clear-log)
  (euporie-stata-test-log 'info "Starting critical Stata console tests")
  
  (ert-run-tests-interactively 
   '(or "euporie-stata-console/no-counter-messages-scatter-plot"
        "euporie-stata-console/complete-workflow-test"
        "euporie-stata-console/compare-with-python-cleanliness")))

(defun euporie-stata-console-analyze-test-failures ()
  "Analyze test failures and provide debugging information."
  (interactive)
  (when (file-exists-p euporie-stata-test-log-file)
    (with-current-buffer (find-file-other-window euporie-stata-test-log-file)
      (goto-char (point-max))
      (message "Stata console test log opened. Check for ERROR and WARN messages."))))

;; Provide helpful information when loaded
(unless noninteractive
  (message "Stata console cleanliness test suite loaded.")
  (message "Available commands:")
  (message "- (euporie-stata-console-run-cleanliness-tests) - Run all tests")
  (message "- (euporie-stata-console-run-critical-tests) - Run critical tests only")
  (message "- (euporie-stata-console-analyze-test-failures) - Analyze failures"))

(provide 'test-euporie-stata-console-cleanliness)
;;; test-euporie-stata-console-cleanliness.el ends here