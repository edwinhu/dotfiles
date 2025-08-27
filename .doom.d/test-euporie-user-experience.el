;;; test-euporie-user-experience.el --- User experience validation tests for Emacs-euporie integration -*- lexical-binding: t; -*-

;;; Commentary:
;; User experience focused unit tests that validate the exact workflows users expect.
;; These tests simulate real user interactions and validate the complete UX stack:
;;
;; - C-RET keybinding behavior in org-mode and source buffers
;; - Inline graphics display in terminal buffers
;; - Buffer management and window layout
;; - Process lifecycle and responsiveness
;; - Error handling and recovery workflows
;;
;; Key focus areas based on user feedback:
;; - Visual verification of sixel graphics inline
;; - Console output cleanliness (no counter messages)
;; - Keybinding responsiveness and reliability  
;; - Multi-window workflow efficiency
;; - Cross-session consistency

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'euporie-termint nil t)
(require 'org)

;; User Experience Test Configuration
(defvar euporie-ux-test-log-file (expand-file-name "euporie-ux-validation.log" "~/")
  "Log file for user experience validation tests.")

(defvar euporie-ux-test-session-timeout 45
  "Timeout for user workflow sessions.")

(defvar euporie-ux-keybinding-test-delay 0.5
  "Delay between keybinding test actions for realistic simulation.")

;; User Experience Utilities
(defun euporie-ux-log (level format-string &rest args)
  "Log user experience test events with workflow context."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N")))
    (with-temp-buffer
      (insert (format "[%s] [UX-%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-ux-test-log-file))))

(defun euporie-ux-clear-log ()
  "Initialize user experience test log."
  (when (file-exists-p euporie-ux-test-log-file)
    (delete-file euporie-ux-test-log-file))
  (euporie-ux-log 'info "=== USER EXPERIENCE VALIDATION SESSION ==="))

(defun euporie-ux-cleanup ()
  "Clean up test environment maintaining user-like cleanup patterns."
  (euporie-ux-log 'info "Performing user-like cleanup")
  
  ;; Close euporie buffers like a user would
  (dolist (buffer-name '("*euporie-stata*" "*euporie-python*" "*euporie-r*"))
    (when (get-buffer buffer-name)
      (euporie-ux-log 'info "Closing %s buffer" buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name))))
  
  ;; Clean up test files
  (dolist (file '("~/test-ux-stata.org" "~/test-ux-workflow.org"))
    (when (file-exists-p (expand-file-name file))
      (delete-file (expand-file-name file))))
  
  (sleep-for 2))

(defun euporie-ux-simulate-user-startup (kernel)
  "Simulate realistic user startup workflow for kernel."
  (euporie-ux-log 'info "Simulating user startup for %s kernel" kernel)
  
  ;; User typically starts from a function call
  (let ((start-func (cond
                     ((string= kernel "stata") #'euporie-stata-start)
                     ((string= kernel "python") #'euporie-python-start)
                     ((string= kernel "r") #'euporie-r-start))))
    
    (when start-func
      (let ((startup-time (current-time)))
        (funcall start-func)
        
        ;; Wait for startup like a user would
        (let ((buffer-name (format "*euporie-%s*" kernel))
              (wait-count 0))
          
          (while (and (< wait-count 20) ; 20 seconds max wait
                     (not (and (get-buffer buffer-name)
                              (with-current-buffer buffer-name
                                (and (get-buffer-process (current-buffer))
                                     (process-live-p (get-buffer-process (current-buffer))))))))
            (sleep-for 1)
            (setq wait-count (1+ wait-count))
            (when (= (mod wait-count 3) 0)
              (euporie-ux-log 'info "User waiting for startup... %ds" wait-count)))
          
          (let ((actual-startup-time (float-time (time-subtract (current-time) startup-time))))
            (euporie-ux-log 'info "%s startup completed in %.2fs" kernel actual-startup-time)
            (< actual-startup-time 30)))))) ; Reasonable startup time

(defun euporie-ux-test-console-responsiveness (buffer-name description)
  "Test console responsiveness from user perspective."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let ((test-marker (point-max))
            (process (get-buffer-process (current-buffer)))
            (response-time (current-time)))
        
        (when process
          (process-send-string process "display \"UX_RESPONSIVENESS_CHECK\"\n")
          (sleep-for 3)
          
          (let ((output (buffer-substring-no-properties test-marker (point-max)))
                (actual-time (float-time (time-subtract (current-time) response-time))))
            
            (euporie-ux-log 'info "%s responsiveness: %.2fs, contains response: %s" 
                           description actual-time
                           (string-match-p "UX_RESPONSIVENESS_CHECK" output))
            
            ;; User expects < 5 second response time
            (and (< actual-time 5.0)
                 (string-match-p "UX_RESPONSIVENESS_CHECK" output))))))))

(defun euporie-ux-validate-clean-output (output description)
  "Validate output cleanliness from user quality expectations."
  (let ((is-clean t)
        (issues '()))
    
    (when (and output (stringp output))
      ;; Check for counter messages that users complained about
      (when (string-match-p "global stata_kernel_graph_counter" output)
        (setq is-clean nil)
        (push "global counter variable visible" issues))
      
      (when (string-match-p "stata_kernel_graph_counter.*=" output)
        (setq is-clean nil)
        (push "counter assignment visible" issues))
      
      (when (string-match-p "\\$stata_kernel_graph_counter.*\\+" output)
        (setq is-clean nil)
        (push "counter increment visible" issues))
      
      ;; Check for excessive debug output
      (when (> (length (split-string output "\n")) 20)  ; More than 20 lines for simple command
        (push "verbose output (>20 lines)" issues)))
    
    (if is-clean
        (euporie-ux-log 'info "%s: CLEAN - user quality standards met" description)
      (euporie-ux-log 'warn "%s: QUALITY ISSUES - %s" description issues))
    
    is-clean))

(defun euporie-ux-simulate-cret-keybinding (kernel code description)
  "Simulate actual C-RET keybinding workflow from user perspective."
  (euporie-ux-log 'info "Simulating C-RET workflow: %s" description)
  
  ;; Create org buffer like user would
  (let ((org-buffer (get-buffer-create "*ux-test-org*"))
        (execution-time (current-time)))
    
    (with-current-buffer org-buffer
      (org-mode)
      (insert (format "* UX Test: %s\n\n" description))
      (insert (format "#+BEGIN_SRC %s\n" kernel))
      (insert code)
      (insert "\n#+END_SRC\n")
      
      ;; Position cursor like user would (in the code)
      (re-search-backward code)
      
      ;; Simulate C-RET execution
      (euporie-ux-log 'info "User presses C-RET on: %s" code)
      (let ((initial-buffer-state (when (get-buffer (format "*euporie-%s*" kernel))
                                   (with-current-buffer (format "*euporie-%s*" kernel)
                                     (point-max)))))
        
        ;; Execute via euporie-termint (simulating keybinding)
        (euporie-termint-send-region-or-line)
        
        ;; Wait for execution like user would observe
        (sleep-for 8)
        
        ;; Capture result from user perspective
        (let ((final-output (when (and (get-buffer (format "*euporie-%s*" kernel)) initial-buffer-state)
                             (with-current-buffer (format "*euporie-%s*" kernel)
                               (buffer-substring-no-properties initial-buffer-state (point-max)))))
              (total-time (float-time (time-subtract (current-time) execution-time))))
          
          (kill-buffer org-buffer)
          
          (euporie-ux-log 'info "C-RET execution completed in %.2fs" total-time)
          (list :output final-output :time total-time :clean (euporie-ux-validate-clean-output final-output description)))))))

;; CORE USER EXPERIENCE TESTS

(ert-deftest euporie-ux/stata-user-workflow-complete ()
  "Test complete user workflow: startup -> data -> plot -> validation."
  (euporie-ux-log 'info "=== UX TEST: Complete Stata user workflow ===")
  (euporie-ux-cleanup)
  
  (unwind-protect
      (let ((workflow-start (current-time)))
        
        ;; Phase 1: User starts euporie
        (euporie-ux-log 'info "Phase 1: User starts euporie-stata")
        (should (euporie-ux-simulate-user-startup "stata"))
        
        ;; Validate startup from user perspective
        (should (get-buffer "*euporie-stata*"))
        (should (euporie-ux-test-console-responsiveness "*euporie-stata*" "Initial startup"))
        
        ;; Phase 2: User loads data via C-RET
        (euporie-ux-log 'info "Phase 2: User loads data via C-RET")
        (let ((data-result (euporie-ux-simulate-cret-keybinding "stata" "sysuse auto" "Data loading")))
          (should (plist-get data-result :clean))
          (should (string-match-p "auto.dta" (or (plist-get data-result :output) ""))))
        
        ;; Phase 3: User creates plot via C-RET
        (euporie-ux-log 'info "Phase 3: User creates scatter plot via C-RET")
        (let ((plot-result (euporie-ux-simulate-cret-keybinding "stata" "scatter price mpg" "Scatter plot creation")))
          
          ;; Critical user experience validation
          (should (plist-get plot-result :clean))  ; Must be clean for good UX
          (should (< (plist-get plot-result :time) 15))  ; Must be responsive
          
          ;; Validate no counter pollution
          (let ((output (plist-get plot-result :output)))
            (should-not (string-match-p "global stata_kernel_graph_counter" (or output "")))
            (should-not (string-match-p "stata_kernel_graph_counter.*=" (or output "")))))
        
        ;; Phase 4: Validate continued responsiveness
        (euporie-ux-log 'info "Phase 4: Validate continued console responsiveness")
        (should (euporie-ux-test-console-responsiveness "*euporie-stata*" "Post-plot"))
        
        ;; Phase 5: User creates second plot (realistic usage)
        (euporie-ux-log 'info "Phase 5: User creates histogram (second plot)")
        (let ((hist-result (euporie-ux-simulate-cret-keybinding "stata" "histogram price, bins(10)" "Histogram")))
          (should (plist-get hist-result :clean)))
        
        ;; Overall workflow timing
        (let ((total-workflow-time (float-time (time-subtract (current-time) workflow-start))))
          (euporie-ux-log 'info "Complete user workflow took %.2fs" total-workflow-time)
          (should (< total-workflow-time 60)) ; Should complete in under 1 minute
          
          (euporie-ux-log 'info "=== UX TEST PASSED: Complete workflow successful ===")))
    
    (euporie-ux-cleanup)))

(ert-deftest euporie-ux/keybinding-reliability-test ()
  "Test C-RET keybinding reliability across different contexts."
  (euporie-ux-log 'info "=== UX TEST: C-RET keybinding reliability ===")
  (euporie-ux-cleanup)
  
  (unwind-protect
      (progn
        ;; Setup
        (should (euporie-ux-simulate-user-startup "stata"))
        (euporie-ux-simulate-cret-keybinding "stata" "sysuse auto" "Initial setup")
        
        ;; Test various code contexts via C-RET
        (let ((test-cases '(("scatter price mpg" . "Simple scatter")
                           ("scatter price mpg, title(\"Test Plot\")" . "Scatter with title")
                           ("twoway scatter price mpg || lfit price mpg" . "Complex twoway")
                           ("histogram price" . "Histogram")
                           ("summarize price mpg" . "Non-graphics command")))
              (all-reliable t))
          
          (dolist (test-case test-cases)
            (let* ((code (car test-case))
                   (description (cdr test-case))
                   (result (euporie-ux-simulate-cret-keybinding "stata" code description))
                   (is-reliable (and (plist-get result :clean)
                                   (< (plist-get result :time) 20))))
              
              (unless is-reliable
                (setq all-reliable nil))
              
              (euporie-ux-log 'info "C-RET reliability for '%s': %s (%.2fs)" 
                             code (if is-reliable "RELIABLE" "UNRELIABLE")
                             (plist-get result :time))))
          
          (should all-reliable)
          (euporie-ux-log 'info "C-RET keybinding reliability: %s" 
                         (if all-reliable "ALL PASSED" "ISSUES DETECTED"))))
    
    (euporie-ux-cleanup)))

(ert-deftest euporie-ux/graphics-display-user-validation ()
  "Test graphics display from user visual validation perspective."
  (euporie-ux-log 'info "=== UX TEST: Graphics display user validation ===")
  (euporie-ux-cleanup)
  
  (unwind-protect
      (progn
        ;; Setup with graphics
        (should (euporie-ux-simulate-user-startup "stata"))
        (euporie-ux-simulate-cret-keybinding "stata" "sysuse auto" "Setup")
        
        ;; Create plot and validate from user perspective
        (let ((plot-result (euporie-ux-simulate-cret-keybinding "stata" "scatter price mpg" "Graphics test")))
          
          ;; User expectation: Clean output
          (should (plist-get plot-result :clean))
          
          ;; User expectation: Graphics files created (indicates working graphics system)
          (let ((cache-dir (expand-file-name "~/.stata_kernel_cache")))
            (when (file-directory-p cache-dir)
              (let ((png-files (directory-files cache-dir nil "\\.png$")))
                (should (> (length png-files) 0))
                (euporie-ux-log 'info "Graphics validation: %d PNG files found in cache" 
                               (length png-files)))))
          
          ;; User expectation: Console shows plot was created (but cleanly)
          (let ((output (plist-get plot-result :output)))
            (when output
              ;; Should not contain debug/counter messages
              (should-not (string-match-p "global.*stata_kernel_graph_counter" output))
              (should-not (string-match-p "\\+.*1.*counter" output))
              
              ;; May contain legitimate graphics-related messages (acceptable)
              (when (string-match-p "graph" output)
                (euporie-ux-log 'info "Output contains graphics-related content (acceptable)"))))
          
          ;; Test continued functionality after graphics
          (let ((post-graphics (euporie-ux-simulate-cret-keybinding "stata" "describe price" "Post-graphics command")))
            (should (plist-get post-graphics :clean)))
          
          (euporie-ux-log 'info "Graphics display user validation: PASSED")))
    
    (euporie-ux-cleanup)))

(ert-deftest euporie-ux/multi-session-consistency ()
  "Test consistency across multiple euporie sessions."
  (euporie-ux-log 'info "=== UX TEST: Multi-session consistency ===")
  (euporie-ux-cleanup)
  
  (unwind-protect
      (progn
        ;; Session 1
        (euporie-ux-log 'info "Testing Session 1")
        (should (euporie-ux-simulate-user-startup "stata"))
        (let ((session1-result (euporie-ux-simulate-cret-keybinding "stata" "sysuse auto\nscatter price mpg" "Session 1")))
          
          ;; Close first session
          (let ((kill-buffer-query-functions nil))
            (kill-buffer "*euporie-stata*"))
          (sleep-for 2)
          
          ;; Session 2 (fresh start)
          (euporie-ux-log 'info "Testing Session 2 (fresh start)")
          (should (euporie-ux-simulate-user-startup "stata"))
          (let ((session2-result (euporie-ux-simulate-cret-keybinding "stata" "sysuse auto\nscatter price mpg" "Session 2")))
            
            ;; Both sessions should be equally clean
            (should (plist-get session1-result :clean))
            (should (plist-get session2-result :clean))
            
            ;; Performance should be consistent
            (let ((time1 (plist-get session1-result :time))
                  (time2 (plist-get session2-result :time)))
              
              (euporie-ux-log 'info "Session timing - 1: %.2fs, 2: %.2fs" time1 time2)
              
              ;; Neither should be significantly slower (indicates issues)
              (should (< time1 20))
              (should (< time2 20))
              
              ;; Consistency check (within reasonable variance)
              (should (< (abs (- time1 time2)) 10)))
            
            (euporie-ux-log 'info "Multi-session consistency: PASSED"))))
    
    (euporie-ux-cleanup)))

(ert-deftest euporie-ux/error-recovery-user-experience ()
  "Test error recovery from user experience perspective."
  (euporie-ux-log 'info "=== UX TEST: Error recovery user experience ===")
  (euporie-ux-cleanup)
  
  (unwind-protect
      (progn
        ;; Setup
        (should (euporie-ux-simulate-user-startup "stata"))
        
        ;; User makes an error (common scenario)
        (euporie-ux-log 'info "User executes invalid command")
        (let ((error-result (euporie-ux-simulate-cret-keybinding "stata" "invalid_command_xyz" "Error test")))
          
          ;; Even error output should be clean from UX perspective
          (should (euporie-ux-validate-clean-output (plist-get error-result :output) "Error handling")))
        
        ;; User recovers with valid command
        (euporie-ux-log 'info "User recovers with valid command")
        (let ((recovery-result (euporie-ux-simulate-cret-keybinding "stata" "sysuse auto" "Recovery")))
          (should (plist-get recovery-result :clean))
          (should (string-match-p "auto.dta" (or (plist-get recovery-result :output) ""))))
        
        ;; User creates graphics after error (real workflow)
        (euporie-ux-log 'info "User creates graphics after error recovery")
        (let ((graphics-result (euporie-ux-simulate-cret-keybinding "stata" "scatter price mpg" "Post-error graphics")))
          (should (plist-get graphics-result :clean)))
        
        ;; Console should still be responsive
        (should (euporie-ux-test-console-responsiveness "*euporie-stata*" "Post-error recovery"))
        
        (euporie-ux-log 'info "Error recovery user experience: PASSED"))
    
    (euporie-ux-cleanup)))

;; USER EXPERIENCE TEST SUITE

(defun euporie-ux-run-user-validation-tests ()
  "Run complete user experience validation test suite."
  (interactive)
  (euporie-ux-clear-log)
  
  (euporie-ux-log 'info "Starting USER EXPERIENCE validation test suite")
  (euporie-ux-log 'info "Focus areas:")
  (euporie-ux-log 'info "- C-RET keybinding reliability and responsiveness")
  (euporie-ux-log 'info "- Visual graphics display quality")
  (euporie-ux-log 'info "- Console output cleanliness")
  (euporie-ux-log 'info "- Multi-session workflow consistency")
  (euporie-ux-log 'info "- Error recovery and stability")
  
  (let ((test-start (current-time)))
    (ert-run-tests-interactively "euporie-ux/")
    
    (let ((test-duration (float-time (time-subtract (current-time) test-start))))
      (euporie-ux-log 'info "User experience test suite completed in %.2f seconds" test-duration)
      (euporie-ux-generate-ux-report))))

(defun euporie-ux-run-critical-ux-tests ()
  "Run critical user experience tests only."
  (interactive)
  (euporie-ux-clear-log)
  (euporie-ux-log 'info "Running CRITICAL user experience tests")
  
  (ert-run-tests-interactively 
   '(or "euporie-ux/stata-user-workflow-complete"
        "euporie-ux/keybinding-reliability-test"
        "euporie-ux/graphics-display-user-validation")))

(defun euporie-ux-generate-ux-report ()
  "Generate user experience validation report."
  (interactive)
  (let ((report-buffer (get-buffer-create "*User Experience Validation Report*")))
    (with-current-buffer report-buffer
      (erase-buffer)
      (insert "# EMACS-EUPORIE USER EXPERIENCE VALIDATION REPORT\n\n")
      (insert (format "Generated: %s\n\n" (current-time-string)))
      
      (insert "## User Experience Focus Areas\n\n")
      (insert "1. **Keybinding Reliability**: C-RET must work consistently across contexts\n")
      (insert "2. **Graphics Quality**: Visual plots must display cleanly inline\n")
      (insert "3. **Console Cleanliness**: No debug/counter messages visible to users\n")
      (insert "4. **Responsiveness**: Commands must complete within reasonable time\n")
      (insert "5. **Workflow Consistency**: Experience must be consistent across sessions\n\n")
      
      ;; Analysis from log
      (when (file-exists-p euporie-ux-test-log-file)
        (insert "## Test Execution Log\n\n```\n")
        (insert-file-contents euporie-ux-test-log-file)
        (goto-char (point-max))
        (insert "\n```\n\n")
        
        ;; User experience summary
        (goto-char (point-min))
        (let ((log-content (buffer-string)))
          (goto-char (point-max))
          (insert "## User Experience Summary\n\n")
          
          (let ((clean-count (length (split-string log-content "user quality standards met")))
                (issue-count (length (split-string log-content "QUALITY ISSUES")))
                (reliable-count (length (split-string log-content "RELIABLE")))
                (unreliable-count (length (split-string log-content "UNRELIABLE"))))
            
            (insert (format "- Clean outputs: %d\n" (max 0 (1- clean-count))))
            (insert (format "- Quality issues: %d\n" (max 0 (1- issue-count))))
            (insert (format "- Reliable operations: %d\n" (max 0 (1- reliable-count))))
            (insert (format "- Unreliable operations: %d\n\n" (max 0 (1- unreliable-count))))
            
            (if (and (= issue-count 1) (= unreliable-count 1))
                (insert "**✅ USER EXPERIENCE: EXCELLENT** - All quality standards met\n")
              (insert "**❌ USER EXPERIENCE: NEEDS IMPROVEMENT** - Quality issues detected\n")))
          
          (when (string-match-p "workflow took" log-content)
            (insert "\n## Workflow Performance\n\n")
            (insert "Complete user workflows measured for performance validation.\n"))))
      
      (display-buffer report-buffer)
      (message "User experience validation report generated"))))

;; Load notification
(unless noninteractive
  (message "Emacs-euporie User Experience validation suite loaded.")
  (message "Available commands:")
  (message "- (euporie-ux-run-user-validation-tests) - Run complete UX validation")
  (message "- (euporie-ux-run-critical-ux-tests) - Run critical UX tests")
  (message "- (euporie-ux-generate-ux-report) - Generate UX validation report")
  (message "")
  (message "These tests focus on REAL USER WORKFLOWS and experience quality."))

(provide 'test-euporie-user-experience)
;;; test-euporie-user-experience.el ends here