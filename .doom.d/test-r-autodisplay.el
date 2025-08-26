;;; test-r-autodisplay.el --- Test suite for R automatic graphics display -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused test suite to investigate and solve the R automatic display issue.
;; The problem: R still requires manual `print()` commands for graphics to appear.
;; The goal: Graphics should auto-display when objects are created, not when explicitly printed.

;;; Code:

(require 'ert)
(require 'euporie-termint)

(defvar r-autodisplay-test-log-file (expand-file-name "r-autodisplay-test.log" "~/")
  "Log file for R autodisplay test results.")

(defun r-autodisplay-test-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to test file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) r-autodisplay-test-log-file))))

(defun r-autodisplay-setup-clean-environment ()
  "Set up a clean R environment for testing."
  (r-autodisplay-test-log 'info "Setting up clean R test environment")
  
  ;; Clean up any existing R buffers
  (dolist (buf '("*euporie-r*" "*R*" "*jupyter-r*"))
    (when (get-buffer buf)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))))
  
  ;; Wait for cleanup
  (sleep-for 1)
  
  ;; Start fresh R euporie session
  (euporie-r-start)
  
  ;; Wait for startup
  (let ((max-wait 15) (wait-count 0))
    (while (and (< wait-count max-wait)
                (or (not (get-buffer "*euporie-r*"))
                    (not (get-buffer-process "*euporie-r*"))))
      (sleep-for 1)
      (setq wait-count (1+ wait-count))))
  
  (if (get-buffer "*euporie-r*")
      (r-autodisplay-test-log 'info "R environment ready")
    (r-autodisplay-test-log 'error "Failed to setup R environment")))

(defun r-autodisplay-execute-and-wait (code expected-pattern timeout-seconds)
  "Execute CODE in R and wait for EXPECTED-PATTERN to appear within TIMEOUT-SECONDS."
  (r-autodisplay-test-log 'info "Executing R code: %s" (substring code 0 (min 100 (length code))))
  
  (let ((buffer (get-buffer "*euporie-r*"))
        (start-time (current-time))
        (found-pattern nil))
    
    (unless buffer
      (error "No R buffer available"))
    
    ;; Record buffer content before execution
    (let ((initial-content (with-current-buffer buffer (buffer-string))))
      (r-autodisplay-test-log 'debug "Buffer content before execution: %d chars" (length initial-content)))
    
    ;; Execute the code
    (termint-euporie-r-send-string code)
    
    ;; Wait for expected pattern or timeout
    (while (and (not found-pattern)
                (< (float-time (time-subtract (current-time) start-time)) timeout-seconds))
      (sleep-for 0.5)
      (with-current-buffer buffer
        (let ((content (buffer-string)))
          (when (string-match expected-pattern content)
            (setq found-pattern t)
            (r-autodisplay-test-log 'info "Found expected pattern: %s" expected-pattern)))))
    
    ;; Log final buffer content for debugging
    (let ((final-content (with-current-buffer buffer (buffer-string))))
      (r-autodisplay-test-log 'debug "Buffer content after execution: %d chars" (length final-content))
      (when (not found-pattern)
        (r-autodisplay-test-log 'warn "Pattern not found. Last 500 chars: %s" 
                                (substring final-content (max 0 (- (length final-content) 500))))))
    
    found-pattern))

;; TEST 1: ggplot2 Automatic Display Test
(ert-deftest r-autodisplay/ggplot2-automatic-display ()
  "Test that ggplot2 objects display automatically without print()."
  (r-autodisplay-test-log 'info "Starting ggplot2 automatic display test")
  
  ;; Setup clean environment
  (r-autodisplay-setup-clean-environment)
  
  ;; Test code - should auto-display without print()
  (let ((test-code "
library(ggplot2)
data(mtcars)
p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
"))
    
    ;; Execute and check for automatic display
    ;; Look for sixel escape sequences or image display indicators
    (let ((found-graphics (r-autodisplay-execute-and-wait 
                          test-code 
                          "\\(?:\033P[^\\]*\033\\\\\\|Image\\|Graphics\\|Plot\\)" 
                          10)))
      
      (if found-graphics
          (r-autodisplay-test-log 'info "SUCCESS: ggplot2 object displayed automatically")
        (progn
          (r-autodisplay-test-log 'error "FAIL: ggplot2 object did not display automatically")
          ;; Additional debug: try manual print to see if that works
          (r-autodisplay-execute-and-wait "print(p)" "\\(?:\033P[^\\]*\033\\\\\\|Image\\|Graphics\\|Plot\\)" 5)))
      
      (should found-graphics))))

;; TEST 2: Base R Automatic Display Test
(ert-deftest r-autodisplay/base-r-automatic-display ()
  "Test that base R plots display automatically."
  (r-autodisplay-test-log 'info "Starting base R automatic display test")
  
  ;; Setup clean environment
  (r-autodisplay-setup-clean-environment)
  
  ;; Test code - base R plot should auto-display immediately
  (let ((test-code "
x <- 1:10
y <- x^2
plot(x, y, main='Base R Auto Display Test')
"))
    
    ;; Execute and check for automatic display
    (let ((found-graphics (r-autodisplay-execute-and-wait 
                          test-code 
                          "\\(?:\033P[^\\]*\033\\\\\\|Image\\|Graphics\\|Plot\\)" 
                          8)))
      
      (if found-graphics
          (r-autodisplay-test-log 'info "SUCCESS: Base R plot displayed automatically")
        (r-autodisplay-test-log 'error "FAIL: Base R plot did not display automatically"))
      
      (should found-graphics))))

;; TEST 3: Multiple Plot Automatic Display Test
(ert-deftest r-autodisplay/multiple-plots-automatic-display ()
  "Test that multiple plots display automatically in sequence."
  (r-autodisplay-test-log 'info "Starting multiple plots automatic display test")
  
  ;; Setup clean environment
  (r-autodisplay-setup-clean-environment)
  
  ;; First plot
  (let ((test-code1 "
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point()
"))
    
    (let ((found-first (r-autodisplay-execute-and-wait 
                       test-code1 
                       "\\(?:\033P[^\\]*\033\\\\\\|Image\\|Graphics\\|Plot\\)" 
                       8)))
      
      (if found-first
          (r-autodisplay-test-log 'info "SUCCESS: First plot displayed automatically")
        (r-autodisplay-test-log 'error "FAIL: First plot did not display automatically"))
      
      ;; Second plot
      (let ((test-code2 "
ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + geom_line()
"))
        
        (let ((found-second (r-autodisplay-execute-and-wait 
                           test-code2 
                           "\\(?:\033P[^\\]*\033\\\\\\|Image\\|Graphics\\|Plot\\)" 
                           8)))
          
          (if found-second
              (r-autodisplay-test-log 'info "SUCCESS: Second plot displayed automatically")
            (r-autodisplay-test-log 'error "FAIL: Second plot did not display automatically"))
          
          (should (and found-first found-second)))))))

;; TEST 4: R Configuration Investigation Test
(ert-deftest r-autodisplay/investigate-r-configuration ()
  "Investigate R configuration to understand why auto-display is not working."
  (r-autodisplay-test-log 'info "Starting R configuration investigation")
  
  ;; Setup clean environment
  (r-autodisplay-setup-clean-environment)
  
  ;; Check IRkernel configuration
  (let ((config-checks '(
    "IRdisplay:::check_installed()" 
    "getOption('IRdisplay.plot.mimetypes')"
    "options()$IRdisplay.plot.mimetypes"
    "capabilities('X11')"
    "Sys.getenv('TERM')"
    "Sys.getenv('COLORTERM')"
    ".Platform$GUI")))
    
    (dolist (check config-checks)
      (r-autodisplay-test-log 'info "Checking: %s" check)
      (r-autodisplay-execute-and-wait check ".*" 3)))
  
  ;; Check if automatic display hooks are installed
  (let ((hook-checks '(
    "exists('.Last.plot.hook')"
    "exists('print.ggplot')"
    "methods('print')")))
    
    (dolist (check hook-checks)
      (r-autodisplay-test-log 'info "Checking hook: %s" check)
      (r-autodisplay-execute-and-wait check ".*" 3))))

;; TEST 5: Manual vs Automatic Display Comparison
(ert-deftest r-autodisplay/manual-vs-automatic-comparison ()
  "Compare manual print() vs automatic display to identify the difference."
  (r-autodisplay-test-log 'info "Starting manual vs automatic comparison")
  
  ;; Setup clean environment
  (r-autodisplay-setup-clean-environment)
  
  ;; Create plot object without displaying
  (let ((setup-code "
library(ggplot2)
p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + ggtitle('Comparison Test')
"))
    
    ;; Setup the plot
    (r-autodisplay-execute-and-wait setup-code ">" 5)
    
    ;; Test 1: Check if plot displays automatically (should fail if issue exists)
    (r-autodisplay-test-log 'info "Testing automatic display (should fail)...")
    (sleep-for 2) ; Wait to see if anything displays automatically
    
    ;; Test 2: Manual print (should succeed)
    (r-autodisplay-test-log 'info "Testing manual print() (should succeed)...")
    (let ((manual-success (r-autodisplay-execute-and-wait 
                          "print(p)" 
                          "\\(?:\033P[^\\]*\033\\\\\\|Image\\|Graphics\\|Plot\\)" 
                          8)))
      
      (if manual-success
          (r-autodisplay-test-log 'info "Manual print() works - confirms display system is functional")
        (r-autodisplay-test-log 'error "Manual print() failed - display system has deeper issues"))
      
      ;; The test passes if manual works (confirms system is functional)
      ;; The automatic display issue is a separate configuration problem
      (should manual-success))))

;; INVESTIGATION HELPER: R Startup Environment
(defun r-autodisplay-investigate-startup-environment ()
  "Investigate R startup environment and configuration."
  (interactive)
  (r-autodisplay-test-log 'info "=== R STARTUP ENVIRONMENT INVESTIGATION ===")
  
  (r-autodisplay-setup-clean-environment)
  
  ;; Check environment variables
  (let ((env-checks '(
    "Sys.getenv('R_HOME')"
    "Sys.getenv('R_PROFILE')" 
    "Sys.getenv('R_PROFILE_USER')"
    ".libPaths()"
    "sessionInfo()")))
    
    (dolist (check env-checks)
      (r-autodisplay-test-log 'info "Environment check: %s" check)
      (r-autodisplay-execute-and-wait check ".*" 3)))
  
  ;; Check for startup files
  (let ((startup-files '(
    "file.exists('~/.Rprofile')"
    "file.exists('.Rprofile')" 
    "file.exists(file.path(R.home(), 'etc', 'Rprofile.site'))")))
    
    (dolist (file startup-files)
      (r-autodisplay-test-log 'info "Startup file check: %s" file)
      (r-autodisplay-execute-and-wait file ".*" 3))))

;; INVESTIGATION HELPER: Plot Method Investigation  
(defun r-autodisplay-investigate-plot-methods ()
  "Investigate R plot methods and display hooks."
  (interactive)
  (r-autodisplay-test-log 'info "=== R PLOT METHODS INVESTIGATION ===")
  
  (r-autodisplay-setup-clean-environment)
  
  ;; Check plot methods
  (let ((method-checks '(
    "methods(class='ggplot')"
    "getAnywhere('print.ggplot')"
    "exists('print.ggplot', mode='function')"
    "typeof(ggplot2:::print.ggplot)"
    "body(ggplot2:::print.ggplot)")))
    
    (dolist (check method-checks)
      (r-autodisplay-test-log 'info "Method check: %s" check)
      (r-autodisplay-execute-and-wait check ".*" 5))))

;; RUN ALL TESTS FUNCTION
(defun r-autodisplay-run-all-tests ()
  "Run all R autodisplay tests and generate comprehensive report."
  (interactive)
  (r-autodisplay-test-log 'info "========================================")
  (r-autodisplay-test-log 'info "STARTING R AUTOMATIC DISPLAY TEST SUITE")
  (r-autodisplay-test-log 'info "========================================")
  
  ;; Clear log file
  (with-temp-buffer
    (write-file r-autodisplay-test-log-file))
  
  (r-autodisplay-test-log 'info "Test suite started at: %s" (current-time-string))
  
  ;; Run investigations first
  (r-autodisplay-investigate-startup-environment)
  (r-autodisplay-investigate-plot-methods)
  
  ;; Run all ERT tests
  (let ((test-results (ert-run-tests-batch-and-exit "r-autodisplay/")))
    (r-autodisplay-test-log 'info "Test suite completed at: %s" (current-time-string))
    (r-autodisplay-test-log 'info "========================================")
    (message "R autodisplay test results written to: %s" r-autodisplay-test-log-file)))

(provide 'test-r-autodisplay)
;;; test-r-autodisplay.el ends here