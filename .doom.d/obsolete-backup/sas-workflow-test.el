;;; sas-workflow-test.el --- SAS workflow comprehensive testing and debugging -*- lexical-binding: t; -*-

;;; Commentary:
;; This provides comprehensive testing and debugging tools for the SAS workflow
;; integration including C-RET → SAS execution pipeline testing.

;;; Code:

(require 'bindings)
(require 'termint-org-src)
(require 'euporie-termint)

;;; Test Configuration
(defvar sas-workflow-test-remote-dir "/sshx:wrds|qrsh::/home/nyu/eddyhu/projects/wander2"
  "Test remote directory for SAS workflow testing.")

(defvar sas-workflow-test-local-dir "/Users/vwh7mb/projects/wander2"
  "Test local directory for SAS workflow testing.")

;;; Logging Management
(defun sas-workflow-clear-all-logs ()
  "Clear all SAS workflow debug logs for fresh testing."
  (interactive)
  (let ((logs '("~/sas-workflow-debug.log" 
                "~/termint-org-src-debug.log" 
                "~/euporie-debug.log")))
    (dolist (log logs)
      (when (file-exists-p (expand-file-name log))
        (delete-file (expand-file-name log))))
    (sas-workflow-debug-log 'info "=== SAS WORKFLOW TEST SESSION STARTED ===")
    (termint-org-src-debug-log 'info "=== TERMINT ORG-SRC TEST SESSION STARTED ===")
    (euporie-termint-debug-log 'info "=== EUPORIE TERMINT TEST SESSION STARTED ===")
    (message "All SAS workflow logs cleared and test session started")))

(defun sas-workflow-show-logs ()
  "Display all SAS workflow debug logs in separate buffers."
  (interactive)
  (let ((logs '(("*SAS Workflow Log*" . "~/sas-workflow-debug.log")
                ("*Termint Org-Src Log*" . "~/termint-org-src-debug.log") 
                ("*Euporie Termint Log*" . "~/euporie-debug.log"))))
    (dolist (log-pair logs)
      (let ((buffer-name (car log-pair))
            (file-path (expand-file-name (cdr log-pair))))
        (when (file-exists-p file-path)
          (with-current-buffer (get-buffer-create buffer-name)
            (erase-buffer)
            (insert-file-contents file-path)
            (goto-char (point-max))
            (view-mode 1)))))
    
    ;; Split window layout to show all logs
    (delete-other-windows)
    (split-window-right)
    (split-window-below)
    (other-window 2)
    (split-window-below)
    
    ;; Display logs in different windows
    (select-window (car (window-list)))
    (switch-to-buffer "*SAS Workflow Log*")
    (other-window 1)
    (switch-to-buffer "*Termint Org-Src Log*")
    (other-window 1)
    (switch-to-buffer "*Euporie Termint Log*")
    
    (message "SAS workflow logs displayed in split layout")))

;;; Function Availability Testing
(defun sas-workflow-test-function-availability ()
  "Test availability of all SAS workflow functions."
  (interactive)
  (sas-workflow-debug-log 'info "=== TESTING FUNCTION AVAILABILITY ===")
  
  (let ((required-functions '(smart-org-src-send
                             termint-org-src-send-line-or-paragraph-or-fun-or-region
                             termint-org-src-smart-sas-start
                             euporie-sas-start
                             euporie-sas-start-remote
                             euporie-sas-start-local
                             euporie-termint-send-code
                             tramp-wrds-termint))
        (available-count 0)
        (missing-functions '()))
    
    (dolist (func required-functions)
      (if (fboundp func)
          (progn
            (sas-workflow-debug-log 'info "✓ Function available: %s" func)
            (setq available-count (1+ available-count)))
        (progn
          (sas-workflow-debug-log 'error "✗ Function missing: %s" func)
          (push func missing-functions))))
    
    (sas-workflow-debug-log 'info "Function availability test complete: %d/%d available" 
                           available-count (length required-functions))
    
    (if missing-functions
        (message "⚠ Missing functions: %s" missing-functions)
      (message "✓ All required functions available (%d/%d)" available-count (length required-functions)))))

;;; Context Testing
(defun sas-workflow-test-context-detection ()
  "Test language detection and context detection in different buffer types."
  (interactive)
  (sas-workflow-debug-log 'info "=== TESTING CONTEXT DETECTION ===")
  
  (let ((current-buffer-name (buffer-name))
        (major-mode-name major-mode)
        (org-src-lang (bound-and-true-p org-src--lang))
        (org-babel-info (bound-and-true-p org-src--babel-info)))
    
    (sas-workflow-debug-log 'info "Current context - buffer: %s, mode: %s" current-buffer-name major-mode-name)
    (sas-workflow-debug-log 'info "Org-src context - lang: %s, babel-info: %s" org-src-lang org-babel-info)
    
    ;; Test language detection from different sources
    (condition-case err
        (let ((detected-kernel (termint-org-src-detect-kernel)))
          (sas-workflow-debug-log 'info "✓ Kernel detection successful: %s" detected-kernel))
      (error
       (sas-workflow-debug-log 'error "✗ Kernel detection failed: %s" err)))
    
    ;; Test buffer name detection
    (when (string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" current-buffer-name)
      (let ((buffer-lang (downcase (match-string 1 current-buffer-name))))
        (sas-workflow-debug-log 'info "Buffer name language detection: %s" buffer-lang)))
    
    (message "Context detection test completed - check debug log for details")))

;;; Remote Connection Testing  
(defun sas-workflow-test-remote-connection ()
  "Test remote WRDS connection establishment."
  (interactive)
  (sas-workflow-debug-log 'info "=== TESTING REMOTE CONNECTION ===")
  
  ;; Test tramp-wrds-termint function
  (if (fboundp 'tramp-wrds-termint)
      (progn
        (sas-workflow-debug-log 'info "Testing tramp-wrds-termint connection...")
        (condition-case err
            (let ((wrds-buffer (tramp-wrds-termint)))
              (if wrds-buffer
                  (progn
                    (sas-workflow-debug-log 'info "✓ Remote connection successful: %s" (buffer-name wrds-buffer))
                    (message "✓ Remote connection test passed"))
                (progn
                  (sas-workflow-debug-log 'error "✗ Remote connection returned nil")
                  (message "✗ Remote connection test failed"))))
          (error
           (sas-workflow-debug-log 'error "✗ Remote connection failed: %s" err)
           (message "✗ Remote connection error: %s" err))))
    (progn
      (sas-workflow-debug-log 'error "✗ Function tramp-wrds-termint not available")
      (message "✗ tramp-wrds-termint function not available"))))

;;; Buffer Management Testing
(defun sas-workflow-test-buffer-cleanup ()
  "Clean up all SAS-related buffers for fresh testing."
  (interactive)
  (sas-workflow-debug-log 'info "=== CLEANING UP SAS BUFFERS ===")
  
  (let ((sas-buffers '("*euporie-sas*" "*termint-wrds-qrsh*" "*WRDS-Shell*"))
        (cleaned-count 0))
    
    (dolist (buffer-name sas-buffers)
      (when (get-buffer buffer-name)
        (sas-workflow-debug-log 'info "Cleaning up buffer: %s" buffer-name)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer-name))
        (setq cleaned-count (1+ cleaned-count))))
    
    (sas-workflow-debug-log 'info "Buffer cleanup complete: %d buffers cleaned" cleaned-count)
    (message "SAS buffer cleanup complete: %d buffers cleaned" cleaned-count)))

;;; Full Workflow Testing
(defun sas-workflow-test-full-remote-workflow ()
  "Test complete remote SAS workflow from C-RET press to execution."
  (interactive)
  (sas-workflow-debug-log 'info "=== TESTING FULL REMOTE WORKFLOW ===")
  
  ;; Clean up first
  (sas-workflow-test-buffer-cleanup)
  
  ;; Test with simulated org-babel info
  (let ((org-src--babel-info `(nil nil ((:dir . ,sas-workflow-test-remote-dir)))))
    (sas-workflow-debug-log 'info "Simulating org-babel-info: %s" org-src--babel-info)
    
    ;; Test remote SAS start
    (condition-case err
        (progn
          (sas-workflow-debug-log 'info "Testing euporie-sas-start with remote dir...")
          (euporie-sas-start sas-workflow-test-remote-dir)
          (sas-workflow-debug-log 'info "✓ Remote SAS start completed"))
      (error
       (sas-workflow-debug-log 'error "✗ Remote SAS start failed: %s" err)
       (message "Remote SAS start failed: %s" err))))
  
  (message "Full remote workflow test completed - check debug log for details"))

(defun sas-workflow-test-full-local-workflow ()
  "Test complete local SAS workflow."
  (interactive)
  (sas-workflow-debug-log 'info "=== TESTING FULL LOCAL WORKFLOW ===")
  
  ;; Clean up first
  (sas-workflow-test-buffer-cleanup)
  
  ;; Test local SAS start
  (condition-case err
      (progn
        (sas-workflow-debug-log 'info "Testing euporie-sas-start-local...")
        (euporie-sas-start-local)
        (sas-workflow-debug-log 'info "✓ Local SAS start completed"))
    (error
     (sas-workflow-debug-log 'error "✗ Local SAS start failed: %s" err)
     (message "Local SAS start failed: %s" err)))
  
  (message "Full local workflow test completed - check debug log for details"))

;;; Interactive Testing Menu
(defun sas-workflow-test-menu ()
  "Interactive menu for SAS workflow testing."
  (interactive)
  (let ((choice (read-char-choice 
                 "SAS Workflow Tests:
1 - Clear all logs
2 - Show logs  
3 - Test functions
4 - Test context
5 - Test remote connection
6 - Clean buffers
7 - Test remote workflow
8 - Test local workflow
9 - Full test suite
q - Quit

Choice: " '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?q))))
    (pcase choice
      (?1 (sas-workflow-clear-all-logs))
      (?2 (sas-workflow-show-logs))
      (?3 (sas-workflow-test-function-availability))
      (?4 (sas-workflow-test-context-detection))
      (?5 (sas-workflow-test-remote-connection))
      (?6 (sas-workflow-test-buffer-cleanup))
      (?7 (sas-workflow-test-full-remote-workflow))
      (?8 (sas-workflow-test-full-local-workflow))
      (?9 (sas-workflow-test-full-suite))
      (?q (message "SAS workflow testing quit")))))

(defun sas-workflow-test-full-suite ()
  "Run the complete SAS workflow test suite."
  (interactive)
  (sas-workflow-debug-log 'info "=== STARTING FULL TEST SUITE ===")
  (message "Running full SAS workflow test suite...")
  
  (sas-workflow-clear-all-logs)
  (sleep-for 1)
  (sas-workflow-test-function-availability)
  (sleep-for 1)  
  (sas-workflow-test-context-detection)
  (sleep-for 1)
  (sas-workflow-test-remote-connection)
  (sleep-for 1)
  (sas-workflow-test-buffer-cleanup)
  (sleep-for 1)
  (sas-workflow-test-full-local-workflow)
  
  (sas-workflow-debug-log 'info "=== FULL TEST SUITE COMPLETED ===")
  (sas-workflow-show-logs)
  (message "Full SAS workflow test suite completed - logs displayed"))

;;; Key bindings
(define-key global-map (kbd "C-c s t") #'sas-workflow-test-menu)
(define-key global-map (kbd "C-c s l") #'sas-workflow-show-logs)
(define-key global-map (kbd "C-c s c") #'sas-workflow-clear-all-logs)

(provide 'sas-workflow-test)
;;; sas-workflow-test.el ends here