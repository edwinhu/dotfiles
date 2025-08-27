;;; test-euporie-critical-simple.el --- Simple critical test -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple critical test to verify that the current implementation fails
;; both requirements: clean output AND inline graphics display.

;;; Code:

(require 'ert)
(require 'euporie-termint nil t)

(defvar euporie-critical-log (expand-file-name "euporie-critical-test.log" "~/"))

(defun euporie-critical-log (level msg)
  "Log critical test message."
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [CRITICAL] %s\n" timestamp msg))
      (append-to-file (point-min) (point-max) euporie-critical-log))
    (message "[%s] %s" level msg)))

(defun euporie-critical-cleanup ()
  "Clean up test environment."
  (when (get-buffer "*euporie-stata*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*euporie-stata*"))))

(defun euporie-critical-has-counter-pollution (content)
  "Check if content has counter pollution."
  (when (stringp content)
    (or (string-match-p "stata_kernel_graph_counter" content)
        (string-match-p "global.*counter" content))))

(defun euporie-critical-has-inline-graphics (buffer-name)
  "Check if buffer has actual inline graphics (escape sequences)."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        ;; Look for graphics protocol escape sequences
        (or (string-match-p "\\\\eP[0-9;]*q" content)    ; Sixel start
            (string-match-p "\\\\e_G" content)           ; Kitty graphics  
            (string-match-p "\\\\e]1337;File=" content)  ; iTerm2 inline
            ;; Alternative: high ratio of non-printable chars suggests graphics
            (let* ((total-chars (length content))
                   (printable-chars (length (replace-regexp-in-string "[^\x20-\x7E]" "" content))))
              (and (> total-chars 100)
                   (< (/ (float printable-chars) total-chars) 0.7))))))))

(ert-deftest euporie-critical/stata-dual-requirements ()
  "CRITICAL: Test both clean output AND inline graphics display."
  (when (file-exists-p euporie-critical-log) (delete-file euporie-critical-log))
  (euporie-critical-log "INFO" "=== CRITICAL DUAL REQUIREMENTS TEST ===")
  (euporie-critical-cleanup)
  
  (unwind-protect
      (progn
        ;; Start Stata console
        (euporie-critical-log "INFO" "Starting Stata console...")
        (euporie-stata-start)
        (sleep-for 15)
        
        (should (get-buffer "*euporie-stata*"))
        (euporie-critical-log "INFO" "Stata console started successfully")
        
        ;; Test data loading (should be clean)
        (with-current-buffer "*euporie-stata*"
          (let ((proc (get-buffer-process (current-buffer)))
                (data-start (point-max)))
            
            (process-send-string proc "sysuse auto\n")
            (sleep-for 8)
            
            (let ((data-output (buffer-substring-no-properties data-start (point-max))))
              (should-not (euporie-critical-has-counter-pollution data-output))
              (euporie-critical-log "INFO" "✓ Data command has clean output"))))
        
        ;; CRITICAL TEST: Graphics command
        (with-current-buffer "*euporie-stata*"
          (let ((proc (get-buffer-process (current-buffer)))
                (graphics-start (point-max)))
            
            (euporie-critical-log "INFO" "Executing graphics command...")
            (process-send-string proc "scatter price mpg\n")
            (sleep-for 15)
            
            (let ((graphics-output (buffer-substring-no-properties graphics-start (point-max))))
              
              ;; REQUIREMENT 1: Clean console output
              (let ((has-pollution (euporie-critical-has-counter-pollution graphics-output)))
                (should-not has-pollution)
                (euporie-critical-log "INFO" 
                                    (format "Console cleanliness: %s" 
                                           (if has-pollution "FAILED" "PASSED"))))
              
              ;; REQUIREMENT 2: Actual inline graphics display
              (let ((has-graphics (euporie-critical-has-inline-graphics "*euporie-stata*")))
                (should has-graphics)  ; This should FAIL with current implementation
                (euporie-critical-log "ERROR" 
                                    (format "Inline graphics display: %s (EXPECTED TO FAIL)" 
                                           (if has-graphics "PASSED" "FAILED"))))))))
    
    ;; Cleanup
    (euporie-critical-cleanup))
  
  (euporie-critical-log "INFO" "=== CRITICAL TEST COMPLETED ==="))

(defun euporie-critical-run-test ()
  "Run the critical test interactively."
  (interactive)
  (message "Running critical dual requirements test...")
  (ert-run-tests-interactively "euporie-critical/stata-dual-requirements"))

(defun euporie-critical-view-log ()
  "View the critical test log."
  (interactive)
  (if (file-exists-p euporie-critical-log)
      (find-file euporie-critical-log)
    (message "Critical test log not found: %s" euporie-critical-log)))

(provide 'test-euporie-critical-simple)
;;; test-euporie-critical-simple.el ends here