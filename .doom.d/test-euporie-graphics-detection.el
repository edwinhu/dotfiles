;;; test-euporie-graphics-detection.el --- Advanced graphics detection for unit tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced graphics detection and validation functions for euporie testing.
;; These functions provide sophisticated detection of actual inline graphics
;; vs. just text-based fallback messages.

;;; Code:

(require 'ert)

(defvar euporie-graphics-test-log (expand-file-name "graphics-detection-test.log" "~/"))

(defun euporie-graphics-log (level format-string &rest args)
  "Log graphics detection message with timestamp."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [GRAPHICS] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-graphics-test-log))))

;;; Advanced Graphics Detection Functions

(defun euporie-graphics-detect-sixel-sequences (buffer-name)
  "Detect actual sixel graphics sequences in buffer content."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let ((content (buffer-substring-no-properties (point-min) (point-max)))
            (detections '()))
        
        ;; Sixel Device Control String (DCS) sequences
        (when (string-match-p "\\eP[0-9;]*q" content)
          (push "sixel-dcs-start" detections)
          (euporie-graphics-log 'info "Detected sixel DCS start sequence"))
        
        ;; Sixel data patterns (color definitions, raster data)
        (when (string-match-p "#[0-9]+;[0-9]+;[0-9]+;[0-9]+" content)
          (push "sixel-color-defs" detections)
          (euporie-graphics-log 'info "Detected sixel color definitions"))
        
        ;; Sixel raster data (geometric patterns)
        (when (string-match-p "[-~!@#$%^&*()_+={}\\[\\]|\\\\:;\"'<>,.?/A-Za-z0-9]+" content)
          (when (> (length (replace-regexp-in-string "[^-~!@#$%^&*()_+={}\\[\\]|\\\\:;\"'<>,.?/]" "" content)) 50)
            (push "sixel-raster-data" detections)
            (euporie-graphics-log 'info "Detected potential sixel raster data")))
        
        ;; String Terminator (ST) for sixel
        (when (string-match-p "\\e\\\\\\\\" content)
          (push "sixel-string-terminator" detections)
          (euporie-graphics-log 'info "Detected sixel string terminator"))
        
        detections))))

(defun euporie-graphics-detect-kitty-sequences (buffer-name)
  "Detect Kitty graphics protocol sequences."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let ((content (buffer-substring-no-properties (point-min) (point-max)))
            (detections '()))
        
        ;; Kitty graphics protocol escape sequences
        (when (string-match-p "\\e_G[^\\\\]*\\e\\\\\\\\" content)
          (push "kitty-graphics-command" detections)
          (euporie-graphics-log 'info "Detected Kitty graphics command"))
        
        ;; Kitty transmission modes
        (when (string-match-p "a=t" content)
          (push "kitty-transmission-mode" detections))
        
        ;; Kitty image data
        (when (string-match-p "f=[0-9]+.*;" content)
          (push "kitty-format-spec" detections))
        
        detections))))

(defun euporie-graphics-detect-iterm2-sequences (buffer-name)
  "Detect iTerm2 inline image sequences."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let ((content (buffer-substring-no-properties (point-min) (point-max)))
            (detections '()))
        
        ;; iTerm2 proprietary escape sequences
        (when (string-match-p "\\e]1337;File=" content)
          (push "iterm2-inline-file" detections)
          (euporie-graphics-log 'info "Detected iTerm2 inline file sequence"))
        
        ;; Base64 encoded image data in iTerm2 format
        (when (string-match-p "inline=1.*:" content)
          (push "iterm2-inline-flag" detections))
        
        detections))))

(defun euporie-graphics-comprehensive-detection (buffer-name)
  "Perform comprehensive graphics protocol detection."
  (euporie-graphics-log 'info "Starting comprehensive graphics detection for %s" buffer-name)
  
  (let ((sixel-detections (euporie-graphics-detect-sixel-sequences buffer-name))
        (kitty-detections (euporie-graphics-detect-kitty-sequences buffer-name))
        (iterm2-detections (euporie-graphics-detect-iterm2-sequences buffer-name))
        (results '()))
    
    ;; Analyze detection results
    (when sixel-detections
      (push `(protocol . "sixel") results)
      (push `(sixel-features . ,sixel-detections) results)
      (euporie-graphics-log 'info "Sixel protocol detected with features: %s" sixel-detections))
    
    (when kitty-detections
      (push `(protocol . "kitty") results)  
      (push `(kitty-features . ,kitty-detections) results)
      (euporie-graphics-log 'info "Kitty protocol detected with features: %s" kitty-detections))
    
    (when iterm2-detections
      (push `(protocol . "iterm2") results)
      (push `(iterm2-features . ,iterm2-detections) results)
      (euporie-graphics-log 'info "iTerm2 protocol detected with features: %s" iterm2-detections))
    
    (when (not (or sixel-detections kitty-detections iterm2-detections))
      (push '(protocol . "none") results)
      (euporie-graphics-log 'warn "No graphics protocols detected"))
    
    results))

;;; Buffer Content Analysis

(defun euporie-graphics-analyze-buffer-composition (buffer-name)
  "Analyze the composition of buffer content to detect graphics vs text."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
             (total-chars (length content))
             (printable-chars (length (replace-regexp-in-string "[^\x20-\x7E]" "" content)))
             (escape-sequences (length (replace-regexp-in-string "[^\\e]" "" content)))
             (control-chars (- total-chars printable-chars escape-sequences))
             (analysis '()))
        
        (push `(total-chars . ,total-chars) analysis)
        (push `(printable-chars . ,printable-chars) analysis)  
        (push `(escape-sequences . ,escape-sequences) analysis)
        (push `(control-chars . ,control-chars) analysis)
        
        ;; Calculate ratios
        (when (> total-chars 0)
          (push `(printable-ratio . ,(/ (float printable-chars) total-chars)) analysis)
          (push `(control-ratio . ,(/ (float control-chars) total-chars) ) analysis))
        
        ;; Determine if content suggests graphics
        (let ((has-graphics-indicators (or (> escape-sequences 10)
                                          (> control-chars 100)
                                          (< (/ (float printable-chars) total-chars) 0.7))))
          (push `(likely-contains-graphics . ,has-graphics-indicators) analysis))
        
        (euporie-graphics-log 'info "Buffer composition: %d total, %d printable (%.2f%%), %d escape seqs, %d control" 
                             total-chars printable-chars 
                             (* 100 (if (> total-chars 0) (/ (float printable-chars) total-chars) 0))
                             escape-sequences control-chars)
        
        analysis))))

;;; Visual State Detection

(defun euporie-graphics-detect-visual-changes (buffer-name before-pos after-command-delay)
  "Detect visual changes in buffer after graphics command."
  (when (get-buffer buffer-name)
    (let ((before-content (with-current-buffer buffer-name
                           (buffer-substring-no-properties before-pos (point-max)))))
      
      ;; Wait for command to complete
      (sleep-for after-command-delay)
      
      (let* ((after-content (with-current-buffer buffer-name
                             (buffer-substring-no-properties before-pos (point-max))))
             (content-diff (length after-content) )
             (new-content (substring after-content (length before-content)))
             (changes '()))
        
        (when (> content-diff 0)
          (push `(content-added . ,content-diff) changes)
          (push `(new-content-preview . ,(substring new-content 0 (min 200 (length new-content)))) changes))
        
        ;; Analyze the new content for graphics indicators
        (let ((graphics-analysis (euporie-graphics-comprehensive-detection buffer-name)))
          (push `(graphics-detection . ,graphics-analysis) changes))
        
        (euporie-graphics-log 'info "Visual changes detected: %d chars added" content-diff)
        changes))))

;;; Test Functions Using Enhanced Detection

(ert-deftest euporie-graphics/sixel-detection-precision ()
  "Test precise sixel sequence detection in euporie buffer."
  :tags '(graphics-detection sixel)
  
  (when (file-exists-p euporie-graphics-test-log) (delete-file euporie-graphics-test-log))
  (euporie-graphics-log 'info "=== SIXEL DETECTION PRECISION TEST ===")
  
  ;; Clean setup  
  (when (get-buffer "*euporie-stata*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*euporie-stata*")))
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (sleep-for 15)
        
        ;; Load test data
        (with-current-buffer "*euporie-stata*"
          (let ((proc (get-buffer-process (current-buffer))))
            (process-send-string proc "sysuse auto\n")
            (sleep-for 5)
            
            ;; Capture state before graphics command
            (let ((before-pos (point-max)))
              (process-send-string proc "scatter price mpg\n")
              
              ;; Analyze visual changes and graphics detection
              (let ((changes (euporie-graphics-detect-visual-changes "*euporie-stata*" before-pos 12)))
                
                ;; Extract graphics detection results
                (let ((graphics-results (cdr (assoc 'graphics-detection changes))))
                  (should graphics-results)
                  
                  ;; Test that actual graphics protocol is detected (not just text)
                  (let ((detected-protocol (cdr (assoc 'protocol graphics-results))))
                    (should detected-protocol)
                    (should-not (string= detected-protocol "none"))
                    (euporie-graphics-log 'info "Detected graphics protocol: %s" detected-protocol)))))))
    
    ;; Cleanup
    (when (get-buffer "*euporie-stata*")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer "*euporie-stata*")))))

(ert-deftest euporie-graphics/buffer-composition-analysis ()
  "Test buffer composition analysis for graphics content detection."
  :tags '(graphics-detection composition)
  
  (euporie-graphics-log 'info "=== BUFFER COMPOSITION ANALYSIS TEST ===")
  
  (when (get-buffer "*euporie-stata*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*euporie-stata*")))
  
  (unwind-protect
      (progn
        (euporie-stata-start)
        (sleep-for 15)
        
        (with-current-buffer "*euporie-stata*"
          (let ((proc (get-buffer-process (current-buffer))))
            ;; Test with non-graphics command first
            (process-send-string proc "sysuse auto\n")
            (sleep-for 5)
            
            (let ((text-analysis (euporie-graphics-analyze-buffer-composition "*euporie-stata*")))
              (should text-analysis)
              (let ((text-has-graphics (cdr (assoc 'likely-contains-graphics text-analysis))))
                (euporie-graphics-log 'info "Text command graphics likelihood: %s" text-has-graphics)))
            
            ;; Test with graphics command  
            (process-send-string proc "scatter price mpg\n")
            (sleep-for 12)
            
            (let ((graphics-analysis (euporie-graphics-analyze-buffer-composition "*euporie-stata*")))
              (should graphics-analysis)
              (let ((graphics-has-graphics (cdr (assoc 'likely-contains-graphics graphics-analysis))))
                (should graphics-has-graphics)  ; This should be true for actual graphics
                (euporie-graphics-log 'info "Graphics command graphics likelihood: %s" graphics-has-graphics))))))
    
    ;; Cleanup
    (when (get-buffer "*euporie-stata*")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer "*euporie-stata*")))))

;;; Test Runner

(defun euporie-graphics-run-detection-tests ()
  "Run all graphics detection tests."
  (interactive)
  (message "Running graphics detection tests...")
  (ert-run-tests-interactively "graphics-detection"))

(provide 'test-euporie-graphics-detection)
;;; test-euporie-graphics-detection.el ends here