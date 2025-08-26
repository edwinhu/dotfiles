;;; r-graphics-debug-framework.el --- Debug framework for R graphics pipeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive debugging framework for investigating the R automatic graphics display issue.
;; Provides detailed logging, pipeline tracing, and diagnostic tools to identify why
;; R graphics are not displaying automatically in the euporie integration.

;;; Code:

(require 'euporie-termint)

(defvar r-graphics-debug-log-file (expand-file-name "r-graphics-debug.log" "~/")
  "Log file for R graphics debugging information.")

(defvar r-graphics-debug-session-id nil
  "Current debugging session ID.")

(defun r-graphics-debug-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to debug file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
        (session (or r-graphics-debug-session-id "DEFAULT")))
    (with-temp-buffer
      (insert (format "[%s] [%s] [%s] %s\n" timestamp session (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) r-graphics-debug-log-file))))

(defun r-graphics-debug-start-session ()
  "Start a new R graphics debugging session."
  (interactive)
  (setq r-graphics-debug-session-id (format "DEBUG-%s" (format-time-string "%Y%m%d-%H%M%S")))
  (r-graphics-debug-log 'info "========================================")
  (r-graphics-debug-log 'info "R GRAPHICS DEBUG SESSION STARTED")
  (r-graphics-debug-log 'info "Session ID: %s" r-graphics-debug-session-id)
  (r-graphics-debug-log 'info "========================================")
  (message "R graphics debug session started: %s" r-graphics-debug-session-id))

(defun r-graphics-debug-setup-monitoring ()
  "Set up comprehensive monitoring of R graphics pipeline."
  (interactive)
  (r-graphics-debug-log 'info "Setting up R graphics pipeline monitoring")
  
  ;; Ensure we have a clean R session
  (when (get-buffer "*euporie-r*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*euporie-r*")))
  
  ;; Start R with debugging
  (euporie-r-start)
  
  ;; Wait for startup
  (let ((max-wait 15) (wait-count 0))
    (while (and (< wait-count max-wait)
                (or (not (get-buffer "*euporie-r*"))
                    (not (get-buffer-process "*euporie-r*"))))
      (sleep-for 1)
      (setq wait-count (1+ wait-count))))
  
  (if (get-buffer "*euporie-r*")
      (progn
        (r-graphics-debug-log 'info "R session ready for monitoring")
        
        ;; Set up buffer monitoring
        (with-current-buffer "*euporie-r*"
          (add-hook 'after-change-functions #'r-graphics-debug-buffer-monitor nil t))
        
        ;; Load our R configuration
        (r-graphics-debug-load-r-config)
        
        (message "R graphics monitoring setup complete"))
    (error "Failed to setup R session for debugging")))

(defun r-graphics-debug-buffer-monitor (beg end len)
  "Monitor changes in R buffer for graphics-related output."
  (when (> (- end beg) 10)  ; Only log significant changes
    (let ((new-text (buffer-substring-no-properties beg end)))
      (when (or (string-match "\\(?:plot\\|ggplot\\|graphics\\|image\\|png\\)" new-text)
                (string-match "\033P" new-text)  ; Sixel escape sequence
                (string-match "IRdisplay" new-text))
        (r-graphics-debug-log 'debug "Graphics-related buffer change: %s" 
                              (substring new-text 0 (min 200 (length new-text))))))))

(defun r-graphics-debug-load-r-config ()
  "Load R autodisplay configuration in the debug session."
  (r-graphics-debug-log 'info "Loading R autodisplay configuration")
  
  (let ((config-file (expand-file-name "r-autodisplay-config.R" "~/.doom.d"))
        (buffer (get-buffer "*euporie-r*")))
    
    (if (file-exists-p config-file)
        (progn
          (r-graphics-debug-log 'info "Sourcing R config file: %s" config-file)
          (termint-euporie-r-send-string (format "source('%s')" config-file))
          (sleep-for 2))
      (r-graphics-debug-log 'error "R config file not found: %s" config-file))))

(defun r-graphics-debug-test-basic-display ()
  "Test basic graphics display functionality."
  (interactive)
  (r-graphics-debug-log 'info "Testing basic graphics display")
  
  (unless (get-buffer "*euporie-r*")
    (r-graphics-debug-setup-monitoring))
  
  ;; Test sequence with comprehensive logging
  (let ((tests '(
    ("Environment Check" . "cat('Testing environment...\\n'); is_jupyter()")
    ("IRdisplay Check" . "cat('IRdisplay check...\\n'); requireNamespace('IRdisplay')")
    ("ggplot2 Load" . "cat('Loading ggplot2...\\n'); library(ggplot2)")
    ("Simple ggplot" . "cat('Creating ggplot...\\n'); p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()")
    ("Manual Print" . "cat('Manual print test...\\n'); print(p)")
    ("Base Plot Test" . "cat('Base plot test...\\n'); plot(1:10, 1:10)"))))
    
    (dolist (test tests)
      (let ((test-name (car test))
            (test-code (cdr test)))
        (r-graphics-debug-log 'info "Running test: %s" test-name)
        (termint-euporie-r-send-string test-code)
        (sleep-for 3)  ; Allow time for execution and output
        (r-graphics-debug-capture-buffer-state test-name)))))

(defun r-graphics-debug-capture-buffer-state (test-name)
  "Capture current R buffer state for debugging."
  (when (get-buffer "*euporie-r*")
    (with-current-buffer "*euporie-r*"
      (let ((content (buffer-string)))
        (r-graphics-debug-log 'debug "Buffer state after '%s': %d chars, last 300: %s" 
                              test-name 
                              (length content)
                              (substring content (max 0 (- (length content) 300))))))))

(defun r-graphics-debug-test-automatic-display ()
  "Test automatic display functionality with detailed monitoring."
  (interactive)
  (r-graphics-debug-log 'info "Testing automatic display functionality")
  
  ;; Ensure monitoring is setup
  (unless (get-buffer "*euporie-r*")
    (r-graphics-debug-setup-monitoring))
  
  ;; Test automatic display vs manual display
  (let ((auto-test-code "
cat('=== AUTOMATIC DISPLAY TEST ===\\n')
library(ggplot2)
data(mtcars)
cat('Creating ggplot object...\\n')
p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + ggtitle('Auto Display Test')
cat('ggplot object created - should display automatically now\\n')
"))
    
    (r-graphics-debug-log 'info "Executing automatic display test")
    (termint-euporie-r-send-string auto-test-code)
    (sleep-for 5)
    (r-graphics-debug-capture-buffer-state "automatic-display-test")
    
    ;; Wait and then test manual display
    (r-graphics-debug-log 'info "Testing manual display for comparison")
    (termint-euporie-r-send-string "cat('=== MANUAL DISPLAY TEST ===\\n'); print(p)")
    (sleep-for 5)
    (r-graphics-debug-capture-buffer-state "manual-display-test")))

(defun r-graphics-debug-investigate-irkernel ()
  "Investigate IRkernel configuration and functionality."
  (interactive)
  (r-graphics-debug-log 'info "Investigating IRkernel configuration")
  
  (unless (get-buffer "*euporie-r*")
    (r-graphics-debug-setup-monitoring))
  
  (let ((irkernel-checks '(
    "cat('=== IRKERNEL INVESTIGATION ===\\n')"
    "cat('IRkernel loaded:', 'IRkernel' %in% loadedNamespaces(), '\\n')"
    "cat('IRdisplay loaded:', 'IRdisplay' %in% loadedNamespaces(), '\\n')" 
    "if(requireNamespace('IRdisplay')) { cat('IRdisplay functions:', ls('package:IRdisplay'), '\\n') }"
    "cat('Display options:', toString(getOption('IRdisplay.plot.mimetypes')), '\\n')"
    "cat('Device list:', toString(dev.list()), '\\n')"
    "cat('Current device:', dev.cur(), '\\n')"
    "cat('Capabilities:', toString(capabilities()), '\\n')"
    "cat('Platform GUI:', .Platform$GUI, '\\n')"
    "cat('TERM env:', Sys.getenv('TERM'), '\\n')"
    "cat('COLORTERM env:', Sys.getenv('COLORTERM'), '\\n')")))
    
    (dolist (check irkernel-checks)
      (termint-euporie-r-send-string check)
      (sleep-for 1))
    
    (sleep-for 3)
    (r-graphics-debug-capture-buffer-state "irkernel-investigation")))

(defun r-graphics-debug-test-display-methods ()
  "Test different display methods to identify what works."
  (interactive)
  (r-graphics-debug-log 'info "Testing different display methods")
  
  (unless (get-buffer "*euporie-r*")
    (r-graphics-debug-setup-monitoring))
  
  (let ((display-tests '(
    "# Test 1: Direct IRdisplay
     if(requireNamespace('IRdisplay')) {
       temp_file <- tempfile(fileext = '.png')
       png(temp_file, width = 400, height = 300)
       plot(1:10, 1:10, main = 'IRdisplay Test')
       dev.off()
       IRdisplay::display_png(temp_file)
       unlink(temp_file)
     }"
    "# Test 2: Standard ggsave approach
     library(ggplot2)
     p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
     temp_file <- tempfile(fileext = '.png')
     ggsave(temp_file, p, width = 6, height = 4)
     if(requireNamespace('IRdisplay')) { IRdisplay::display_png(temp_file) }
     unlink(temp_file)"
    "# Test 3: Default device output
     library(ggplot2)
     p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point()
     p")))
    
    (dolist (test display-tests)
      (r-graphics-debug-log 'info "Running display method test")
      (termint-euporie-r-send-string test)
      (sleep-for 4)
      (r-graphics-debug-capture-buffer-state "display-method-test"))))

(defun r-graphics-debug-analyze-results ()
  "Analyze debugging results and provide recommendations."
  (interactive)
  (r-graphics-debug-log 'info "========================================")
  (r-graphics-debug-log 'info "ANALYSIS AND RECOMMENDATIONS")
  (r-graphics-debug-log 'info "========================================")
  
  (let ((log-content (when (file-exists-p r-graphics-debug-log-file)
                       (with-temp-buffer
                         (insert-file-contents r-graphics-debug-log-file)
                         (buffer-string)))))
    
    (when log-content
      ;; Analyze log for common issues
      (let ((issues '()))
        
        ;; Check for IRdisplay availability
        (unless (string-match "IRdisplay loaded: TRUE" log-content)
          (push "IRdisplay not properly loaded in R session" issues))
        
        ;; Check for automatic display
        (unless (string-match "Auto-displaying ggplot object" log-content)
          (push "ggplot2 automatic display override not working" issues))
        
        ;; Check for manual display success
        (if (string-match "ggplot displayed via IRdisplay" log-content)
            (push "Manual display works - issue is in automatic triggering" issues)
          (push "Both automatic and manual display failing" issues))
        
        ;; Report issues
        (if issues
            (progn
              (r-graphics-debug-log 'error "IDENTIFIED ISSUES:")
              (dolist (issue issues)
                (r-graphics-debug-log 'error "- %s" issue)))
          (r-graphics-debug-log 'info "No obvious issues found in logs"))
        
        ;; Provide recommendations
        (r-graphics-debug-log 'info "RECOMMENDATIONS:")
        (cond
         ((member "IRdisplay not properly loaded in R session" issues)
          (r-graphics-debug-log 'info "- Install IRdisplay: install.packages('IRdisplay')")
          (r-graphics-debug-log 'info "- Verify Jupyter environment detection"))
         
         ((member "ggplot2 automatic display override not working" issues)
          (r-graphics-debug-log 'info "- Check R startup configuration loading")
          (r-graphics-debug-log 'info "- Verify print.ggplot method override installation"))
         
         ((member "Manual display works - issue is in automatic triggering" issues)
          (r-graphics-debug-log 'info "- Focus on assignment hooks and automatic triggers")
          (r-graphics-debug-log 'info "- Consider modifying ggplot2 evaluation context"))
         
         (t
          (r-graphics-debug-log 'info "- Review detailed logs for specific error patterns")
          (r-graphics-debug-log 'info "- Consider environment variable issues (TERM, COLORTERM)")
          (r-graphics-debug-log 'info "- Check euporie graphics protocol compatibility")))
        
        (r-graphics-debug-log 'info "========================================")
        (message "Debug analysis complete. Check log file: %s" r-graphics-debug-log-file)))))

(defun r-graphics-debug-full-pipeline ()
  "Run complete R graphics debugging pipeline."
  (interactive)
  (r-graphics-debug-start-session)
  (r-graphics-debug-setup-monitoring)
  (sleep-for 2)
  (r-graphics-debug-investigate-irkernel)
  (sleep-for 2)
  (r-graphics-debug-test-basic-display)
  (sleep-for 2)
  (r-graphics-debug-test-automatic-display)
  (sleep-for 2)
  (r-graphics-debug-test-display-methods)
  (sleep-for 2)
  (r-graphics-debug-analyze-results)
  (message "Full R graphics debugging pipeline completed. Check log: %s" r-graphics-debug-log-file))

(provide 'r-graphics-debug-framework)
;;; r-graphics-debug-framework.el ends here