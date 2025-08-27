;;; test-euporie-visual-verification.el --- Visual verification tests with enhanced screenshots -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive visual verification tests that capture screenshots at key moments
;; and analyze the visual state to verify both clean console output and 
;; actual inline graphics display.

;;; Code:

(require 'ert)
(require 'euporie-termint nil t)

(defvar euporie-visual-test-log (expand-file-name "visual-verification-test.log" "~/"))
(defvar euporie-visual-screenshot-dir "/Users/vwh7mb/dotfiles/.doom.d/")
(defvar euporie-visual-test-session-id nil)

(defun euporie-visual-log (level format-string &rest args)
  "Log visual test message with timestamp."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [VISUAL] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-visual-test-log))))

(defun euporie-visual-start-test-session ()
  "Start a new visual test session with unique ID."
  (setq euporie-visual-test-session-id (format-time-string "%Y%m%d-%H%M%S"))
  (euporie-visual-log 'info "=== VISUAL VERIFICATION SESSION %s STARTED ===" euporie-visual-test-session-id))

;;; Enhanced Screenshot Functions

(defun euporie-visual-screenshot-with-analysis (stage-name &optional analysis-notes)
  "Take screenshot and perform visual analysis."
  (let* ((timestamp (format-time-string "%H%M%S"))
         (screenshot-file (format "%svisual-test-%s-%s-%s.png" 
                                euporie-visual-screenshot-dir
                                euporie-visual-test-session-id
                                timestamp
                                stage-name))
         (metadata-file (concat screenshot-file ".meta")))
    
    ;; Capture screenshot with focus and delay
    (shell-command (format "osascript -e 'tell application \"Emacs\" to activate' && sleep 0.8 && screencapture '%s'" 
                          screenshot-file))
    
    ;; Capture metadata about current state
    (let ((metadata (list
                     (cons 'timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
                     (cons 'stage stage-name)
                     (cons 'session-id euporie-visual-test-session-id)
                     (cons 'analysis-notes (or analysis-notes ""))
                     (cons 'buffer-state (euporie-visual-capture-buffer-state))
                     (cons 'window-configuration (current-window-configuration)))))
      
      ;; Write metadata
      (with-temp-buffer
        (prin1 metadata (current-buffer))
        (write-file metadata-file)))
    
    (euporie-visual-log 'info "Screenshot captured: %s - %s" stage-name screenshot-file)
    
    ;; Return screenshot info for further analysis
    (list (cons 'file screenshot-file)
          (cons 'metadata metadata-file)
          (cons 'stage stage-name))))

(defun euporie-visual-capture-buffer-state ()
  "Capture detailed state of current euporie buffer."
  (let ((buffer-name "*euporie-stata*"))
    (if (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (list (cons 'buffer-name buffer-name)
                (cons 'buffer-size (buffer-size))
                (cons 'point-position (point))
                (cons 'point-max (point-max))
                (cons 'buffer-modified-p (buffer-modified-p))
                (cons 'process-status (when (get-buffer-process (current-buffer))
                                      (process-status (get-buffer-process (current-buffer)))))
                (cons 'last-100-chars (buffer-substring-no-properties 
                                      (max (point-min) (- (point-max) 100)) 
                                      (point-max)))
                (cons 'content-preview (buffer-substring-no-properties 
                                       (max (point-min) (- (point-max) 500)) 
                                       (point-max)))))
      '((buffer-name . "NOT-FOUND")))))

;;; Visual State Analysis

(defun euporie-visual-analyze-console-cleanliness (buffer-name start-pos)
  "Analyze console output for cleanliness from start position."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let* ((new-content (buffer-substring-no-properties start-pos (point-max)))
             (analysis '()))
        
        ;; Check for various types of pollution
        (push (cons 'has-counter-pollution 
                   (string-match-p "stata_kernel_graph_counter\\|global.*counter\\|counter.*=" new-content))
               analysis)
        
        (push (cons 'has-error-messages 
                   (string-match-p "Error\\|Exception\\|Traceback" new-content))
               analysis)
        
        (push (cons 'has-warning-messages 
                   (string-match-p "Warning\\|warn:" new-content))
               analysis)
        
        (push (cons 'has-debug-output 
                   (string-match-p "DEBUG\\|debug:" new-content))
               analysis)
        
        (push (cons 'has-png-fallback 
                   (string-match-p "file.*written in PNG format\\|\.png.*saved" new-content))
               analysis)
        
        ;; Calculate cleanliness score
        (let ((pollution-count (length (seq-filter (lambda (item) (cdr item)) analysis))))
          (push (cons 'cleanliness-score (- 100 (* pollution-count 20))) analysis))
        
        (push (cons 'content-length (length new-content)) analysis)
        (push (cons 'content-preview (substring new-content 0 (min 200 (length new-content)))) analysis)
        
        (euporie-visual-log 'info "Console cleanliness analysis: %d issues found" pollution-count)
        analysis))))

(defun euporie-visual-analyze-graphics-display (buffer-name)
  "Analyze buffer for actual graphics display indicators."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
             (analysis '()))
        
        ;; Detailed graphics protocol detection
        (push (cons 'has-sixel-sequences 
                   (string-match-p "\eP[0-9;]*q\\|\e\\\\\\\\\\|\e_G" content))
               analysis)
        
        (push (cons 'has-kitty-graphics 
                   (string-match-p "\e_G.*\e\\\\" content))
               analysis)
        
        (push (cons 'has-iterm2-graphics 
                   (string-match-p "\e]1337;File=" content))
               analysis)
        
        ;; Content composition analysis
        (let* ((total-chars (length content))
               (escape-sequences (length (replace-regexp-in-string "[^\e]" "" content)))
               (control-chars (length (replace-regexp-in-string "[^\x00-\x1F\x7F-\x9F]" "" content))))
          
          (push (cons 'total-characters total-chars) analysis)
          (push (cons 'escape-sequence-count escape-sequences) analysis)
          (push (cons 'control-character-count control-chars) analysis)
          
          (when (> total-chars 0)
            (push (cons 'control-char-ratio (/ (float control-chars) total-chars)) analysis)
            (push (cons 'escape-seq-ratio (/ (float escape-sequences) total-chars)) analysis)))
        
        ;; Graphics likelihood assessment
        (let ((graphics-indicators (+ (if (cdr (assoc 'has-sixel-sequences analysis)) 1 0)
                                     (if (cdr (assoc 'has-kitty-graphics analysis)) 1 0) 
                                     (if (cdr (assoc 'has-iterm2-graphics analysis)) 1 0)
                                     (if (> (cdr (assoc 'escape-sequence-count analysis)) 20) 1 0)
                                     (if (> (cdr (assoc 'control-character-count analysis)) 50) 1 0))))
          (push (cons 'graphics-likelihood-score (* graphics-indicators 20)) analysis))
        
        (euporie-visual-log 'info "Graphics display analysis: %d indicators found" 
                           (/ (cdr (assoc 'graphics-likelihood-score analysis)) 20))
        analysis))))

;;; Comprehensive Visual Test Suite

(ert-deftest euporie-visual/comprehensive-stata-workflow ()
  "Comprehensive visual verification of Stata euporie workflow."
  :tags '(visual-verification comprehensive)
  
  ;; Initialize test session
  (when (file-exists-p euporie-visual-test-log) (delete-file euporie-visual-test-log))
  (euporie-visual-start-test-session)
  
  ;; Clean environment
  (when (get-buffer "*euporie-stata*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*euporie-stata*")))
  
  (unwind-protect
      (progn
        ;; Stage 1: Startup
        (euporie-visual-log 'info "Stage 1: Starting Stata console")
        (euporie-stata-start)
        (sleep-for 8)
        (should (get-buffer "*euporie-stata*"))
        (euporie-visual-screenshot-with-analysis "01-startup" "Initial Stata console startup")
        
        ;; Stage 2: Wait for full initialization  
        (euporie-visual-log 'info "Stage 2: Waiting for full initialization")
        (sleep-for 12)
        (euporie-visual-screenshot-with-analysis "02-initialized" "Console fully initialized")
        
        ;; Stage 3: Load test data
        (euporie-visual-log 'info "Stage 3: Loading test data")
        (with-current-buffer "*euporie-stata*"
          (let ((proc (get-buffer-process (current-buffer)))
                (data-start-pos (point-max)))
            
            (process-send-string proc "sysuse auto\n")
            (sleep-for 8)
            (euporie-visual-screenshot-with-analysis "03-data-loaded" "Test data loaded")
            
            ;; Analyze data loading cleanliness
            (let ((data-analysis (euporie-visual-analyze-console-cleanliness "*euporie-stata*" data-start-pos)))
              (should (>= (cdr (assoc 'cleanliness-score data-analysis)) 80))
              (euporie-visual-log 'info "Data loading cleanliness score: %d" 
                                 (cdr (assoc 'cleanliness-score data-analysis))))))
        
        ;; Stage 4: Execute graphics command
        (euporie-visual-log 'info "Stage 4: Executing scatter plot")
        (with-current-buffer "*euporie-stata*"
          (let ((proc (get-buffer-process (current-buffer)))
                (graphics-start-pos (point-max)))
            
            (process-send-string proc "scatter price mpg\n")
            (sleep-for 3)
            (euporie-visual-screenshot-with-analysis "04-graphics-executing" "Graphics command sent")
            
            ;; Wait for graphics completion
            (sleep-for 12)
            (euporie-visual-screenshot-with-analysis "05-graphics-completed" "Graphics command completed")
            
            ;; CRITICAL DUAL ANALYSIS
            (let ((cleanliness-analysis (euporie-visual-analyze-console-cleanliness "*euporie-stata*" graphics-start-pos))
                  (graphics-analysis (euporie-visual-analyze-graphics-display "*euporie-stata*")))
              
              ;; REQUIREMENT 1: Console Cleanliness
              (let ((cleanliness-score (cdr (assoc 'cleanliness-score cleanliness-analysis))))
                (should (>= cleanliness-score 80))
                (should-not (cdr (assoc 'has-counter-pollution cleanliness-analysis)))
                (euporie-visual-log 'info "Graphics cleanliness score: %d" cleanliness-score))
              
              ;; REQUIREMENT 2: Actual Graphics Display  
              (let ((graphics-score (cdr (assoc 'graphics-likelihood-score graphics-analysis))))
                (should (>= graphics-score 40))  ; At least 2 graphics indicators
                (should (or (cdr (assoc 'has-sixel-sequences graphics-analysis))
                           (cdr (assoc 'has-kitty-graphics graphics-analysis))
                           (cdr (assoc 'has-iterm2-graphics graphics-analysis))))
                (euporie-visual-log 'info "Graphics display score: %d" graphics-score)))))
        
        ;; Stage 5: Additional graphics test
        (euporie-visual-log 'info "Stage 5: Testing histogram command")
        (with-current-buffer "*euporie-stata*"
          (let ((proc (get-buffer-process (current-buffer)))
                (hist-start-pos (point-max)))
            
            (process-send-string proc "hist price\n")
            (sleep-for 15)
            (euporie-visual-screenshot-with-analysis "06-histogram-completed" "Histogram command completed")
            
            ;; Verify sustained performance
            (let ((hist-cleanliness (euporie-visual-analyze-console-cleanliness "*euporie-stata*" hist-start-pos))
                  (hist-graphics (euporie-visual-analyze-graphics-display "*euporie-stata*")))
              
              (should (>= (cdr (assoc 'cleanliness-score hist-cleanliness)) 80))
              (should (>= (cdr (assoc 'graphics-likelihood-score hist-graphics)) 40))
              
              (euporie-visual-log 'info "Histogram cleanliness: %d, graphics: %d" 
                                 (cdr (assoc 'cleanliness-score hist-cleanliness))
                                 (cdr (assoc 'graphics-likelihood-score hist-graphics)))))))
    
    ;; Cleanup and final screenshot
    (euporie-visual-screenshot-with-analysis "07-final-state" "Test completed")
    (when (get-buffer "*euporie-stata*")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer "*euporie-stata*"))))
  
  (euporie-visual-log 'info "=== VISUAL VERIFICATION SESSION %s COMPLETED ===" euporie-visual-test-session-id))

(ert-deftest euporie-visual/rapid-fire-graphics-test ()
  "Test rapid successive graphics commands for performance degradation."
  :tags '(visual-verification performance)
  
  (euporie-visual-start-test-session)
  
  (when (get-buffer "*euporie-stata*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*euporie-stata*")))
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (sleep-for 15)
        (euporie-visual-screenshot-with-analysis "rapid-startup" "Rapid test startup")
        
        (with-current-buffer "*euporie-stata*"
          (let ((proc (get-buffer-process (current-buffer))))
            ;; Load data quickly
            (process-send-string proc "sysuse auto\n")
            (sleep-for 5)
            
            ;; Rapid fire graphics commands
            (let ((commands '("scatter price mpg" 
                            "hist price"
                            "graph bar (mean) price, over(foreign)")))
              
              (dotimes (i (length commands))
                (let* ((cmd (nth i commands))
                       (start-pos (point-max)))
                  
                  (euporie-visual-log 'info "Rapid command %d: %s" (1+ i) cmd)
                  (process-send-string proc (concat cmd "\n"))
                  (sleep-for 10)
                  
                  (euporie-visual-screenshot-with-analysis 
                   (format "rapid-%02d-%s" (1+ i) (replace-regexp-in-string " " "-" cmd))
                   (format "Rapid command %d completed" (1+ i)))
                  
                  ;; Quick analysis
                  (let ((cleanliness (euporie-visual-analyze-console-cleanliness "*euporie-stata*" start-pos))
                        (graphics (euporie-visual-analyze-graphics-display "*euporie-stata*")))
                    
                    (should (>= (cdr (assoc 'cleanliness-score cleanliness)) 60))  ; Slightly lower for rapid fire
                    (should (>= (cdr (assoc 'graphics-likelihood-score graphics)) 30))
                    
                    (euporie-visual-log 'info "Rapid %d scores - clean: %d, graphics: %d" 
                                       (1+ i)
                                       (cdr (assoc 'cleanliness-score cleanliness))
                                       (cdr (assoc 'graphics-likelihood-score graphics)))))))))
    
    (when (get-buffer "*euporie-stata*")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer "*euporie-stata*")))))

;;; Test Utilities

(defun euporie-visual-run-comprehensive-test ()
  "Run the comprehensive visual verification test."
  (interactive)
  (message "Running comprehensive visual verification test...")
  (ert-run-tests-interactively "euporie-visual/comprehensive-stata-workflow"))

(defun euporie-visual-run-all-visual-tests ()
  "Run all visual verification tests."
  (interactive)
  (message "Running all visual verification tests...")
  (ert-run-tests-interactively "visual-verification"))

(defun euporie-visual-view-test-log ()
  "View the visual test log."
  (interactive)
  (if (file-exists-p euporie-visual-test-log)
      (find-file euporie-visual-test-log)
    (message "Visual test log not found: %s" euporie-visual-test-log)))

(defun euporie-visual-open-screenshot-dir ()
  "Open the screenshot directory."
  (interactive)
  (find-file euporie-visual-screenshot-dir))

;;; Expected Results Documentation

;;; EXPECTED BEHAVIOR WITH CURRENT (BROKEN) IMPLEMENTATION:
;;;
;;; The visual verification tests are designed to FAIL with the current
;;; implementation because:
;;;
;;; 1. Graphics display score will be LOW (< 40) because:
;;;    - No actual sixel/kitty/iterm2 sequences detected
;;;    - Low control character and escape sequence ratios
;;;    - Graphics commands produce text output instead of graphics
;;;
;;; 2. Screenshots will show:
;;;    - PNG fallback messages instead of inline graphics
;;;    - Clean console (which is good) but no visual graphics
;;;    - Text descriptions of graphics instead of actual display
;;;
;;; 3. Buffer analysis will show:
;;;    - High cleanliness scores (80+) - this part works
;;;    - Low graphics likelihood scores (< 40) - this part is broken
;;;
;;; SUCCESS CRITERIA:
;;;   - Both cleanliness score >= 80 AND graphics score >= 40
;;;   - Screenshots show actual inline graphics, not text messages
;;;   - Buffer contains actual graphics protocol sequences

(provide 'test-euporie-visual-verification)
;;; test-euporie-visual-verification.el ends here