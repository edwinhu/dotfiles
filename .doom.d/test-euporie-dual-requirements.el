;;; test-euporie-dual-requirements.el --- Strict unit tests for dual requirements -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that BOTH requirements are met simultaneously:
;; 1. Clean console output (no counter messages)
;; 2. Actual inline graphics display (not just PNG file creation)
;;
;; These tests are expected to FAIL with the current implementation
;; as the fixes broke graphics display functionality.

;;; Code:

(require 'ert)
(require 'euporie-termint nil t)

(defvar euporie-dual-test-log (expand-file-name "euporie-dual-requirements-test.log" "~/"))
(defvar euporie-dual-test-screenshot-counter 0)

;;; Test Infrastructure

(defun euporie-dual-log (level format-string &rest args)
  "Log test message with timestamp."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-dual-test-log))))

(defun euporie-dual-test-cleanup ()
  "Clean up all euporie test buffers."
  (euporie-dual-log 'info "Cleaning up test environment")
  (dolist (buf '("*euporie-stata*" "*euporie-python*" "*euporie-r*"))
    (when (get-buffer buf)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))))
  ;; Wait for cleanup
  (sleep-for 2))

(defun euporie-dual-test-screenshot (description)
  "Take screenshot for test verification."
  (setq euporie-dual-test-screenshot-counter (1+ euporie-dual-test-screenshot-counter))
  (let ((screenshot-file (format "dual-req-test-%03d-%s.png" 
                                euporie-dual-test-screenshot-counter
                                (replace-regexp-in-string "[^a-zA-Z0-9]" "-" description))))
    (shell-command (format "osascript -e 'tell application \"Emacs\" to activate' && sleep 0.5 && screencapture '%s'" 
                          screenshot-file))
    (euporie-dual-log 'info "Screenshot taken: %s - %s" screenshot-file description)
    screenshot-file))

;;; Console Output Analysis

(defun euporie-dual-console-has-counter-pollution (buffer-content)
  "Check if console output contains counter pollution."
  (when (stringp buffer-content)
    (or (string-match-p "stata_kernel_graph_counter" buffer-content)
        (string-match-p "global.*graph.*counter" buffer-content)
        (string-match-p "counter.*=" buffer-content))))

(defun euporie-dual-console-has-png-fallback-messages (buffer-content)
  "Check if console shows PNG fallback messages instead of inline graphics."
  (when (stringp buffer-content)
    (or (string-match-p "file.*written in PNG format" buffer-content)
        (string-match-p "\.png.*saved" buffer-content)
        (string-match-p "Graph export.*PNG" buffer-content))))

;;; Graphics Display Detection

(defun euporie-dual-buffer-has-inline-graphics (buffer-name)
  "Check if buffer actually displays graphics inline (not just text)."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        ;; Check for actual graphics display indicators
        ;; Sixel graphics would contain escape sequences
        (or (string-match-p "\e\\[\\?2026[hl]" content)  ; Sixel mode
            (string-match-p "\eP[0-9;]*q" content)       ; Sixel data start
            (string-match-p "\e\\\\\\|\\eP.*\e\\\\" content) ; Sixel sequences
            ;; Kitty graphics protocol
            (string-match-p "\e_G.*\e\\\\" content)
            ;; iTerm2 inline images
            (string-match-p "\e]1337;File=" content))))))

(defun euporie-dual-detect-graphics-protocol-used (buffer-name)
  "Detect which graphics protocol was actually used in the buffer."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (cond 
         ((string-match-p "\eP[0-9;]*q\\|\\eP.*\e\\\\" content) "sixel")
         ((string-match-p "\e_G.*\e\\\\" content) "kitty")
         ((string-match-p "\e]1337;File=" content) "iterm2")
         (t "none"))))))

;;; Core Test Helper Functions

(defun euporie-stata-send-and-wait (command wait-seconds expected-text)
  "Send COMMAND to Stata console and wait for EXPECTED-TEXT or timeout."
  (let ((buffer "*euporie-stata*")
        (start-time (current-time))
        (timeout wait-seconds)
        (found nil))
    
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (let ((proc (get-buffer-process (current-buffer)))
              (start-pos (point-max)))
          
          (when proc
            (euporie-dual-log 'info "Sending command: %s" command)
            (process-send-string proc (concat command "\n"))
            
            ;; Wait for expected text or timeout
            (while (and (< (time-to-seconds (time-subtract (current-time) start-time)) timeout)
                       (not found))
              (sleep-for 0.5)
              (let ((output (buffer-substring-no-properties start-pos (point-max))))
                (when (and expected-text (string-match-p expected-text output))
                  (setq found t))
                (when (not expected-text)  ; Just wait for time
                  (setq found t))))
            
            ;; Return output for analysis
            (buffer-substring-no-properties start-pos (point-max))))))))

;;; Main Test Suite

(ert-deftest euporie-dual/stata-console-and-graphics-integration ()
  "CRITICAL TEST: Verify BOTH clean console AND inline graphics display."
  :tags '(dual-requirements critical)
  
  ;; Clean setup
  (when (file-exists-p euporie-dual-test-log) (delete-file euporie-dual-test-log))
  (setq euporie-dual-test-screenshot-counter 0)
  (euporie-dual-log 'info "=== DUAL REQUIREMENTS TEST STARTED ===")
  (euporie-dual-test-cleanup)
  
  (unwind-protect
      (progn
        ;; Step 1: Start Stata console
        (euporie-dual-log 'info "Starting Stata euporie console...")
        (euporie-stata-start)
        (sleep-for 15)  ; Allow full startup
        
        (should (get-buffer "*euporie-stata*"))
        (euporie-dual-test-screenshot "stata-console-startup")
        
        ;; Step 2: Load test data (verify basic functionality)
        (let ((data-output (euporie-stata-send-and-wait "sysuse auto" 8 "1978 automobile data")))
          (should data-output)
          (euporie-dual-log 'info "Data loaded successfully")
          
          ;; REQUIREMENT 1: Verify clean output (no counter pollution)
          (should-not (euporie-dual-console-has-counter-pollution data-output))
          (euporie-dual-log 'info "✓ Data command has clean output"))
        
        ;; Step 3: Generate graphics command and test BOTH requirements
        (let ((plot-start-pos (with-current-buffer "*euporie-stata*" (point-max))))
          
          (euporie-dual-log 'info "Executing scatter plot command...")
          (let ((graphics-output (euporie-stata-send-and-wait "scatter price mpg" 15 nil)))
            
            ;; Take screenshot immediately after graphics command
            (euporie-dual-test-screenshot "after-scatter-command")
            
            ;; REQUIREMENT 1: Clean Console Output
            (let ((has-counter-pollution (euporie-dual-console-has-counter-pollution graphics-output))
                  (has-png-fallback (euporie-dual-console-has-png-fallback-messages graphics-output)))
              
              (euporie-dual-log 'info "Console cleanliness check:")
              (euporie-dual-log 'info "  - Counter pollution: %s" (if has-counter-pollution "FAIL" "PASS"))
              (euporie-dual-log 'info "  - PNG fallback messages: %s" (if has-png-fallback "DETECTED" "NONE"))
              
              (should-not has-counter-pollution))
            
            ;; REQUIREMENT 2: Actual Inline Graphics Display
            (let ((has-inline-graphics (euporie-dual-buffer-has-inline-graphics "*euporie-stata*"))
                  (graphics-protocol (euporie-dual-detect-graphics-protocol-used "*euporie-stata*")))
              
              (euporie-dual-log 'info "Graphics display check:")
              (euporie-dual-log 'info "  - Inline graphics detected: %s" (if has-inline-graphics "YES" "NO"))
              (euporie-dual-log 'info "  - Graphics protocol used: %s" graphics-protocol)
              
              ;; This should currently FAIL because graphics display is broken
              (should has-inline-graphics)
              (should-not (string= graphics-protocol "none"))
              
              ;; Take final verification screenshot
              (euporie-dual-test-screenshot "final-verification")))))
    
    ;; Cleanup
    (euporie-dual-test-cleanup))
  
  (euporie-dual-log 'info "=== DUAL REQUIREMENTS TEST COMPLETED ==="))

(ert-deftest euporie-dual/stata-multiple-plots-sustained-performance ()
  "Test that both requirements are maintained across multiple plot commands."
  :tags '(dual-requirements performance)
  
  (euporie-dual-log 'info "=== MULTIPLE PLOTS PERFORMANCE TEST ===")
  (euporie-dual-test-cleanup)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (sleep-for 15)
        
        ;; Load data
        (euporie-stata-send-and-wait "sysuse auto" 8 "1978 automobile data")
        
        ;; Test multiple plot types
        (dolist (plot-cmd '("scatter price mpg"
                           "hist price"
                           "graph bar (mean) price, over(foreign)"))
          
          (euporie-dual-log 'info "Testing plot command: %s" plot-cmd)
          (let ((output (euporie-stata-send-and-wait plot-cmd 15 nil)))
            
            ;; Both requirements must pass for each plot
            (should-not (euporie-dual-console-has-counter-pollution output))
            (should (euporie-dual-buffer-has-inline-graphics "*euporie-stata*"))
            
            (euporie-dual-test-screenshot (format "plot-%s" (replace-regexp-in-string " " "-" plot-cmd)))
            (sleep-for 3))))  ; Brief pause between plots
    
    (euporie-dual-test-cleanup))
  
  (euporie-dual-log 'info "=== MULTIPLE PLOTS PERFORMANCE TEST COMPLETED ==="))

(ert-deftest euporie-dual/stata-user-workflow-simulation ()
  "Simulate actual user workflow with mixed commands and graphics."
  :tags '(dual-requirements workflow)
  
  (euporie-dual-log 'info "=== USER WORKFLOW SIMULATION ===")
  (euporie-dual-test-cleanup)
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (sleep-for 15)
        
        ;; Simulate typical user session
        (let ((user-commands '(("sysuse auto" "1978 automobile data")
                              ("describe" nil)
                              ("summarize price mpg" nil) 
                              ("scatter price mpg" nil)    ; Graphics command
                              ("list in 1/5" nil)
                              ("hist price, bin(10)" nil)  ; Another graphics command
                              ("tab foreign" nil))))
          
          (dolist (cmd-pair user-commands)
            (let* ((cmd (car cmd-pair))
                   (expected (cadr cmd-pair))
                   (output (euporie-stata-send-and-wait cmd 10 expected))
                   (is-graphics-cmd (string-match-p "scatter\\|hist\\|graph\\|plot" cmd)))
              
              (euporie-dual-log 'info "User command: %s (graphics: %s)" cmd is-graphics-cmd)
              
              ;; All commands should have clean output
              (should-not (euporie-dual-console-has-counter-pollution output))
              
              ;; Graphics commands should display inline
              (when is-graphics-cmd
                (should (euporie-dual-buffer-has-inline-graphics "*euporie-stata*"))
                (euporie-dual-test-screenshot (format "workflow-%s" (replace-regexp-in-string "[^a-zA-Z0-9]" "-" cmd))))
              
              (sleep-for 2)))))
    
    (euporie-dual-test-cleanup))
  
  (euporie-dual-log 'info "=== USER WORKFLOW SIMULATION COMPLETED ==="))

;;; Test Runner Functions

(defun euporie-dual-run-critical-test ()
  "Run only the critical dual requirements test."
  (interactive)
  (message "Running critical dual requirements test...")
  (ert-run-tests-interactively "euporie-dual/stata-console-and-graphics-integration"))

(defun euporie-dual-run-all-tests ()
  "Run all dual requirements tests."
  (interactive)
  (message "Running all dual requirements tests...")
  (ert-run-tests-interactively "dual-requirements"))

(defun euporie-dual-view-test-log ()
  "View the test log file."
  (interactive)
  (if (file-exists-p euporie-dual-test-log)
      (find-file euporie-dual-test-log)
    (message "Test log file not found: %s" euporie-dual-test-log)))

;;; Expected Test Results with Current Implementation

;;; EXPECTED FAILURE POINTS:
;;;
;;; 1. euporie-dual-buffer-has-inline-graphics will return nil
;;;    because the current implementation broke graphics display
;;;
;;; 2. euporie-dual-detect-graphics-protocol-used will return "none"
;;;    because no graphics protocols are actually being used
;;;
;;; 3. Screenshots will show PNG fallback messages instead of actual graphics
;;;
;;; SUCCESS CRITERIA:
;;; - Tests pass only when user gets BOTH:
;;;   a) Clean console (no counter messages) 
;;;   b) Visible inline graphics (actual display, not just PNG files)

(provide 'test-euporie-dual-requirements)
;;; test-euporie-dual-requirements.el ends here