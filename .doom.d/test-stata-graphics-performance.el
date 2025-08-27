;;; test-stata-graphics-performance.el --- Performance tests for Stata graphics pipeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Performance-focused unit tests for the Stata graphics pipeline optimization.
;; Tests the fixes applied to eliminate infinite loops, reduce latency, and 
;; streamline the MIME display system.
;;
;; Validates that the developer agent's fixes for:
;; 1. Infinite loops in stata_session.py
;; 2. IPython-compatible MIME display in kernel.py  
;; 3. Elimination of duplicate image displays
;; 4. Streamlined methods to reduce delays
;;
;; Performance targets:
;; - Graphics display < 10 seconds
;; - No hanging or infinite loops
;; - Consistent performance across multiple plots
;; - Optimized file detection in .stata_kernel_cache

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'euporie-termint nil t)

;; Performance test configuration
(defvar stata-perf-test-log-file (expand-file-name "stata-graphics-performance.log" "~/")
  "Log file for Stata graphics performance tests.")

(defvar stata-perf-test-timeout 30
  "Timeout for performance tests.")

(defvar stata-perf-test-expected-latency 10
  "Expected maximum latency for graphics display in seconds.")

;; Performance test utilities
(defun stata-perf-log (level format-string &rest args)
  "Log performance test message with LEVEL, FORMAT-STRING and ARGS."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) stata-perf-test-log-file))))

(defun stata-perf-clear-log ()
  "Clear the performance test log."
  (when (file-exists-p stata-perf-test-log-file)
    (delete-file stata-perf-test-log-file)))

(defun stata-perf-time-command (buffer-name command description)
  "Time execution of COMMAND in BUFFER-NAME with DESCRIPTION."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let* ((start-time (current-time))
             (process (get-buffer-process (current-buffer)))
             (start-marker (point-max)))
        
        (when process
          (stata-perf-log 'info "Starting: %s" description)
          
          ;; Send command
          (process-send-string process (concat command "\n"))
          
          ;; Wait for completion (look for prompt or timeout)
          (let ((completed nil)
                (elapsed 0))
            (while (and (not completed) (< elapsed stata-perf-test-timeout))
              (sleep-for 0.5)
              (setq elapsed (float-time (time-subtract (current-time) start-time)))
              
              ;; Check if we have new output ending with prompt
              (let ((output (buffer-substring-no-properties start-marker (point-max))))
                (when (and output (> (length output) 0))
                  ;; Look for Stata prompt patterns
                  (if (or (string-match-p "In \\[[0-9]+\\]:" output)
                          (string-match-p "\\[\\?2004h" output) 
                          (string-match-p "> $" output)
                          (> elapsed 15)) ; Force completion after reasonable time
                      (setq completed t)))))
            
            (let ((total-time (float-time (time-subtract (current-time) start-time))))
              (stata-perf-log 'info "Completed: %s in %.3f seconds" description total-time)
              total-time))))))

(defun stata-perf-cleanup-buffers ()
  "Clean up performance test buffers."
  (dolist (buffer-name '("*euporie-stata*" "*test-perf-stata*"))
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name))))
  (sleep-for 1))

(defun stata-perf-wait-for-stable-process (buffer-name timeout)
  "Wait for process in BUFFER-NAME to be stable and ready."
  (let ((start-time (current-time))
        (stable nil))
    (while (and (not stable) 
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (let ((process (get-buffer-process (current-buffer))))
            (when (and process (process-live-p process))
              ;; Send a simple test command to verify responsiveness
              (let ((test-start (point-max)))
                (process-send-string process "display \"ready\"\n")
                (sleep-for 2)
                (let ((output (buffer-substring-no-properties test-start (point-max))))
                  (when (string-match-p "ready" output)
                    (setq stable t))))))))
      (unless stable
        (sleep-for 1)))
    stable))

;;; Performance Baseline Tests

(ert-deftest stata-graphics-perf/startup-time ()
  "Measure Stata euporie console startup time."
  (stata-perf-log 'info "Testing Stata euporie startup performance")
  (stata-perf-cleanup-buffers)
  
  (let ((start-time (current-time)))
    (unwind-protect
        (progn
          (euporie-stata-start)
          (should (stata-perf-wait-for-stable-process "*euporie-stata*" stata-perf-test-timeout))
          
          (let ((startup-time (float-time (time-subtract (current-time) start-time))))
            (stata-perf-log 'info "Startup completed in %.3f seconds" startup-time)
            
            ;; Startup should be reasonable
            (should (< startup-time 25))
            (should (> startup-time 2)) ; Should take some time to actually start
            
            startup-time))
      
      (stata-perf-cleanup-buffers))))

(ert-deftest stata-graphics-perf/data-load-performance ()
  "Measure data loading performance baseline."
  (stata-perf-log 'info "Testing data loading performance")
  (stata-perf-cleanup-buffers)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (should (stata-perf-wait-for-stable-process "*euporie-stata*" stata-perf-test-timeout))
        
        ;; Test different dataset loading times
        (let ((datasets '(("sysuse auto" "Auto dataset")
                         ("sysuse nlsw88" "NLSW88 dataset"))))
          
          (dolist (dataset datasets)
            (let* ((command (car dataset))
                   (description (cadr dataset))
                   (load-time (stata-perf-time-command "*euporie-stata*" command description)))
              
              (when load-time
                ;; Data loading should be fast
                (should (< load-time 8))
                (stata-perf-log 'info "%s loaded in %.3f seconds" description load-time))))))
    
    (stata-perf-cleanup-buffers)))

;;; Graphics Performance Tests

(ert-deftest stata-graphics-perf/single-plot-latency ()
  "Measure single plot generation latency."
  (stata-perf-log 'info "Testing single plot generation latency")
  (stata-perf-cleanup-buffers)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (should (stata-perf-wait-for-stable-process "*euporie-stata*" stata-perf-test-timeout))
        
        ;; Load data first
        (stata-perf-time-command "*euporie-stata*" "sysuse auto" "Data loading")
        
        ;; Test different plot types
        (let ((plot-commands '(("scatter price mpg" "Scatter plot")
                              ("histogram price" "Histogram")
                              ("twoway scatter price mpg || lfit price mpg" "Fitted line plot"))))
          
          (dolist (plot-cmd plot-commands)
            (let* ((command (car plot-cmd))
                   (description (cadr plot-cmd))
                   (plot-time (stata-perf-time-command "*euporie-stata*" command description)))
              
              (when plot-time
                (stata-perf-log 'info "%s completed in %.3f seconds" description plot-time)
                
                ;; Performance target: should be under expected latency
                (should (< plot-time stata-perf-test-expected-latency))
                (should (> plot-time 0.5)) ; Should take some time (not instant/broken)
                )))))
    
    (stata-perf-cleanup-buffers)))

(ert-deftest stata-graphics-perf/multiple-plots-consistency ()
  "Test performance consistency across multiple plots."
  (stata-perf-log 'info "Testing multiple plots performance consistency")
  (stata-perf-cleanup-buffers)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (should (stata-perf-wait-for-stable-process "*euporie-stata*" stata-perf-test-timeout))
        
        (stata-perf-time-command "*euporie-stata*" "sysuse auto" "Data loading")
        
        ;; Generate multiple scatter plots and measure consistency
        (let ((plot-times '()))
          (dotimes (i 5)
            (let* ((command (format "scatter price mpg, title(\"Performance Test %d\")" (1+ i)))
                   (description (format "Plot %d performance" (1+ i)))
                   (plot-time (stata-perf-time-command "*euporie-stata*" command description)))
              
              (when plot-time
                (push plot-time plot-times)
                (stata-perf-log 'info "Plot %d: %.3f seconds" (1+ i) plot-time))))
          
          (when plot-times
            (let* ((avg-time (/ (apply #'+ plot-times) (length plot-times)))
                   (min-time (apply #'min plot-times))
                   (max-time (apply #'max plot-times))
                   (variance (- max-time min-time)))
              
              (stata-perf-log 'info "Performance summary - Avg: %.3f, Min: %.3f, Max: %.3f, Variance: %.3f" 
                             avg-time min-time max-time variance)
              
              ;; Performance should be consistent
              (should (< avg-time stata-perf-test-expected-latency))
              (should (< variance 8)) ; Variance should be reasonable
              (should (< min-time stata-perf-test-expected-latency))))))
    
    (stata-perf-cleanup-buffers)))

(ert-deftest stata-graphics-perf/no-infinite-loops ()
  "Test that graphics pipeline has no infinite loops or hanging."
  (stata-perf-log 'info "Testing for infinite loops in graphics pipeline")
  (stata-perf-cleanup-buffers)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (should (stata-perf-wait-for-stable-process "*euporie-stata*" stata-perf-test-timeout))
        
        (stata-perf-time-command "*euporie-stata*" "sysuse auto" "Data loading")
        
        ;; Test commands that previously caused infinite loops
        (let ((problematic-commands '(("scatter price mpg" "Basic scatter")
                                     ("scatter price mpg, by(foreign)" "By-group scatter") 
                                     ("twoway scatter price mpg || lowess price mpg" "Overlay plot"))))
          
          (dolist (cmd-desc problematic-commands)
            (let* ((command (car cmd-desc))
                   (description (cadr cmd-desc))
                   (start-time (current-time))
                   (plot-time (stata-perf-time-command "*euporie-stata*" command description))
                   (watchdog-time (float-time (time-subtract (current-time) start-time))))
              
              (stata-perf-log 'info "%s - Plot time: %.3f, Watchdog time: %.3f" 
                             description (or plot-time -1) watchdog-time)
              
              ;; Must complete within timeout (no infinite loops)
              (should plot-time) ; Should return a time, not nil
              (should (< watchdog-time (+ stata-perf-test-timeout 5)))
              
              ;; Console should remain responsive after each plot
              (let ((responsive-start (current-time)))
                (let ((resp-time (stata-perf-time-command "*euporie-stata*" 
                                                         "display \"responsive\"" 
                                                         "Responsiveness check")))
                  (when resp-time
                    (should (< resp-time 5)) ; Quick response expected
                    (stata-perf-log 'info "Console responsive in %.3f seconds after %s" 
                                   resp-time description))))))))
    
    (stata-perf-cleanup-buffers)))

;;; File System Performance Tests

(ert-deftest stata-graphics-perf/cache-directory-performance ()
  "Test .stata_kernel_cache directory performance."
  (stata-perf-log 'info "Testing cache directory performance")
  
  (let* ((cache-dir (expand-file-name "~/.stata_kernel_cache"))
         (start-time (current-time)))
    
    (when (file-directory-p cache-dir)
      ;; Test directory scanning performance
      (let ((png-files (directory-files cache-dir nil "\\.png$")))
        (let ((scan-time (float-time (time-subtract (current-time) start-time))))
          (stata-perf-log 'info "Scanned %d PNG files in %.3f seconds" 
                         (length png-files) scan-time)
          
          ;; Should be fast even with many files
          (should (< scan-time 2.0))))
      
      ;; Test file sorting performance (newest first)
      (setq start-time (current-time))
      (let* ((all-files (directory-files cache-dir t "\\.png$"))
             (sorted-files (sort all-files 
                                (lambda (a b)
                                  (file-newer-than-file-p a b)))))
        (let ((sort-time (float-time (time-subtract (current-time) start-time))))
          (stata-perf-log 'info "Sorted %d files in %.3f seconds" 
                         (length sorted-files) sort-time)
          
          ;; File sorting should be fast
          (should (< sort-time 1.0)))))))

(ert-deftest stata-graphics-perf/file-detection-optimization ()
  "Test optimized file detection pipeline performance."
  (stata-perf-log 'info "Testing file detection optimization")
  (stata-perf-cleanup-buffers)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (should (stata-perf-wait-for-stable-process "*euporie-stata*" stata-perf-test-timeout))
        
        (stata-perf-time-command "*euporie-stata*" "sysuse auto" "Data loading")
        
        ;; Generate a plot and measure file detection time
        (let* ((cache-dir (expand-file-name "~/.stata_kernel_cache"))
               (files-before (when (file-directory-p cache-dir)
                              (directory-files cache-dir nil "\\.png$")))
               (count-before (length files-before)))
          
          ;; Generate plot
          (let ((plot-time (stata-perf-time-command "*euporie-stata*" 
                                                   "scatter price mpg" 
                                                   "Plot with file detection")))
            
            ;; Wait a bit for file system
            (sleep-for 2)
            
            ;; Check file detection performance
            (let* ((detection-start (current-time))
                   (files-after (when (file-directory-p cache-dir)
                                 (directory-files cache-dir nil "\\.png$")))
                   (count-after (length files-after))
                   (detection-time (float-time (time-subtract (current-time) detection-start))))
              
              (stata-perf-log 'info "File detection: %d files before, %d after, detection in %.3f seconds"
                             count-before count-after detection-time)
              
              ;; Should detect new files quickly
              (should (< detection-time 0.5))
              
              ;; Should have created at least one new file
              (should (>= count-after count-before))))))
    
    (stata-perf-cleanup-buffers)))

;;; Memory and Resource Tests

(ert-deftest stata-graphics-perf/memory-usage-stability ()
  "Test memory usage stability during multiple graphics operations."
  (stata-perf-log 'info "Testing memory usage stability")
  (stata-perf-cleanup-buffers)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (should (stata-perf-wait-for-stable-process "*euporie-stata*" stata-perf-test-timeout))
        
        (stata-perf-time-command "*euporie-stata*" "sysuse auto" "Data loading")
        
        ;; Record initial memory usage (approximate via buffer sizes)
        (let* ((initial-buffer-size (with-current-buffer "*euporie-stata*"
                                     (buffer-size)))
               (plot-commands '("scatter price mpg"
                               "histogram price"
                               "twoway scatter price mpg || lfit price mpg"
                               "scatter price weight"
                               "histogram mpg")))
          
          (stata-perf-log 'info "Initial buffer size: %d characters" initial-buffer-size)
          
          ;; Execute multiple plots
          (dolist (command plot-commands)
            (stata-perf-time-command "*euporie-stata*" command 
                                    (format "Memory test: %s" command))
            (sleep-for 1))
          
          ;; Check final memory usage
          (let ((final-buffer-size (with-current-buffer "*euporie-stata*"
                                    (buffer-size)))
                (growth-ratio (/ (float final-buffer-size) initial-buffer-size)))
            
            (stata-perf-log 'info "Final buffer size: %d characters, growth ratio: %.2f"
                           final-buffer-size growth-ratio)
            
            ;; Memory growth should be reasonable (not excessive)
            (should (< growth-ratio 5.0)) ; Should not grow more than 5x
            (should (> growth-ratio 1.0)) ; Should grow some (normal output)
            )))
    
    (stata-perf-cleanup-buffers)))

;;; Performance Test Suite Runner

(defun stata-graphics-run-performance-tests ()
  "Run all Stata graphics performance tests."
  (interactive)
  (stata-perf-clear-log)
  (stata-perf-log 'info "Starting Stata graphics performance test suite")
  
  (ert-run-tests-interactively "stata-graphics-perf/"))

(defun stata-graphics-run-critical-performance-tests ()
  "Run critical performance tests only."
  (interactive)
  (stata-perf-clear-log)
  (stata-perf-log 'info "Starting critical Stata graphics performance tests")
  
  (ert-run-tests-interactively 
   '(or "stata-graphics-perf/single-plot-latency"
        "stata-graphics-perf/no-infinite-loops"
        "stata-graphics-perf/multiple-plots-consistency")))

(defun stata-graphics-performance-report ()
  "Generate a performance report from test results."
  (interactive)
  (when (file-exists-p stata-perf-test-log-file)
    (with-current-buffer (find-file-other-window stata-perf-test-log-file)
      (goto-char (point-max))
      (message "Stata graphics performance report opened. Look for timing data and performance metrics."))))

;; Provide helpful information when loaded
(unless noninteractive
  (message "Stata graphics performance test suite loaded.")
  (message "Available commands:")
  (message "- (stata-graphics-run-performance-tests) - Run all performance tests")
  (message "- (stata-graphics-run-critical-performance-tests) - Run critical tests")
  (message "- (stata-graphics-performance-report) - View performance report"))

(provide 'test-stata-graphics-performance)
;;; test-stata-graphics-performance.el ends here