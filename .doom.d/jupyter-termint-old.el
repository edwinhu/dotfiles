;;; jupyter-termint.el --- Jupyter console integration using termint and vterm -*- lexical-binding: t; -*-

;;; Commentary:
;; Jupyter console integration using termint.el with vterm backend
;; This replaces the comint-based approach to enable proper bracketed paste
;; and multi-line single-cell execution. Uses vterm for better sixel graphics support.
;; Stata graphics use simplified file monitoring approach instead of monkey-patching

;;; Code:

(require 'termint nil t)
(require 'org)
(require 'ob)
(require 'org-src)
(require 'cl-lib)

(defgroup jupyter-termint nil
  "Jupyter console integration using termint and vterm."
  :group 'org-babel)

(defcustom jupyter-termint-inline-images t
  "Display images inline in the console buffer instead of popup windows.
When t, images are embedded directly in the console buffer.
When nil, images are displayed in separate popup windows."
  :type 'boolean
  :group 'jupyter-termint)

(defcustom jupyter-termint-image-directory
  (or (bound-and-true-p org-babel-temporary-directory)
      temporary-file-directory)
  "Directory for generated image files."
  :type 'directory
  :group 'jupyter-termint)

(defcustom jupyter-termint-image-format "png"
  "Default format for auto-generated images."
  :type '(choice (const "png")
                 (const "pdf")
                 (const "svg"))
  :group 'jupyter-termint)

(defcustom jupyter-termint-image-dpi 150
  "Default DPI for generated images."
  :type 'number
  :group 'jupyter-termint)

;;; Stata File Monitoring Functions

(defvar jupyter-termint-stata-last-graph-time 0
  "Timestamp of last processed graph file.")

(defvar jupyter-termint-stata-displayed-graphs nil
  "List of already displayed graph files to prevent duplicates.")

(defvar jupyter-termint-stata-monitoring-timer nil
  "Timer for monitoring Stata graph files.")

(defvar jupyter-termint-stata-debug-log-file 
  (expand-file-name "jupyter-stata-debug.log" "~/")
  "Log file for Stata file monitoring debugging.")

;;; Cell Tracking for Image Positioning

(defvar jupyter-termint-cell-counter 0
  "Current cell execution counter for tracking jupyter cells.")

(defvar jupyter-termint-pending-images nil
  "List of images awaiting positioning after cell completion.")

(defvar jupyter-termint-cell-positions nil
  "Alist mapping cell numbers to their output positions in buffer.")

(defun jupyter-termint-stata-debug-log (level format-string &rest args)
  "Log Stata file monitoring messages to file with timestamp."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) jupyter-termint-stata-debug-log-file))))

;;; Pure Sixel Cell Tracking (Simplified)

(defun jupyter-termint-track-cell-execution (buffer-name)
  "Track cell execution for pure sixel graphics.
In pure sixel mode, no overlay management needed - terminal handles graphics directly."
  (when (get-buffer buffer-name)
    (setq jupyter-termint-cell-counter (1+ jupyter-termint-cell-counter))
    (jupyter-termint-stata-debug-log 'info "Tracking cell execution %d in %s (pure sixel mode)" 
                                   jupyter-termint-cell-counter buffer-name)))

(defun jupyter-termint-check-for-new-stata-graphs ()
  "Check for new graph files in ~/.stata_kernel_cache/ and display them inline."
  (condition-case err
      (let* ((cache-dir (expand-file-name "~/.stata_kernel_cache/"))
             (graph-files (when (file-directory-p cache-dir)
                           (directory-files cache-dir t "graph[0-9]+\\.png$"))))
        (dolist (file graph-files)
          (condition-case file-err
              (let* ((file-time (float-time (nth 5 (file-attributes file))))
                     (file-key (file-name-nondirectory file))
                     ;; Create unique key combining filename and modification time
                     (unique-key (format "%s-%s" file-key file-time)))
                (when (and (> file-time jupyter-termint-stata-last-graph-time)
                           (not (member unique-key jupyter-termint-stata-displayed-graphs)))
                  (jupyter-termint-stata-debug-log 'info "New graph detected: %s (time: %s)" file file-time)
                  ;; Use condition-case for the display operation to prevent timer crashes
                  (condition-case display-err
                      (progn
                        (jupyter-termint-display-image-at-cell file "*jupyter-stata*")
                        ;; Update last time to this file's time (but don't prevent newer files)
                        (when (> file-time jupyter-termint-stata-last-graph-time)
                          (setq jupyter-termint-stata-last-graph-time (- file-time 0.1))) ; Slight buffer for concurrent files
                        (push unique-key jupyter-termint-stata-displayed-graphs))
                    (error 
                     (jupyter-termint-stata-debug-log 'error "Failed to display graph %s: %s" file (error-message-string display-err))))))
            (error 
             (jupyter-termint-stata-debug-log 'warn "Error processing file %s: %s" file (error-message-string file-err))))))
    (error 
     (jupyter-termint-stata-debug-log 'error "Error in automatic graph monitoring: %s" (error-message-string err)))))

(defun jupyter-termint-start-stata-graph-monitoring ()
  "Start monitoring for new Stata graph files."
  (interactive)
  (jupyter-termint-stata-debug-log 'info "Starting Stata graph file monitoring")
  (setq jupyter-termint-stata-last-graph-time (float-time (current-time)))
  (setq jupyter-termint-stata-displayed-graphs nil) ; Clear previous session graphs
  (when jupyter-termint-stata-monitoring-timer
    (cancel-timer jupyter-termint-stata-monitoring-timer))
  (setq jupyter-termint-stata-monitoring-timer
        (run-with-timer 1 1 #'jupyter-termint-check-for-new-stata-graphs))
  (message "✓ Stata graph monitoring started"))

(defun jupyter-termint-stop-stata-graph-monitoring ()
  "Stop monitoring for Stata graph files."
  (interactive)
  (when jupyter-termint-stata-monitoring-timer
    (cancel-timer jupyter-termint-stata-monitoring-timer)
    (setq jupyter-termint-stata-monitoring-timer nil)
    (jupyter-termint-stata-debug-log 'info "Stata graph file monitoring stopped")
    (message "✓ Stata graph monitoring stopped")))

;;; Pure Sixel Graphics Display Function

;;;###autoload
(defun jupyter-termint-display-image-at-cell (image-path buffer-name)
  "Display image using sixel graphics in the terminal buffer.
For all languages: Execute img2sixel externally and insert raw sixel output.

Args:
  IMAGE-PATH: Full path to the PNG file 
  BUFFER-NAME: Name of vterm terminal buffer (e.g. \"*jupyter-stata*\")"
  (interactive "fImage file: \nbBuffer name: ")
  
  (jupyter-termint-stata-debug-log 'info "=== SIMPLE SIXEL DISPLAY START ===")
  (jupyter-termint-stata-debug-log 'info "Image path: %s" image-path)
  (jupyter-termint-stata-debug-log 'info "Buffer name: %s" buffer-name)
  
  (cond
   ((not (file-exists-p image-path))
    (jupyter-termint-stata-debug-log 'error "PNG file not found: %s" image-path)
    (message "ERROR: PNG file not found: %s" image-path)
    nil)
   
   (t
    (let ((buffer (get-buffer buffer-name)))
      (cond
       ((not buffer)
        (jupyter-termint-stata-debug-log 'error "Buffer not found: %s" buffer-name)
        (message "ERROR: Buffer not found: %s" buffer-name)
        nil)
        
       (t
        (with-current-buffer buffer
          (let ((current-process (get-buffer-process (current-buffer)))
                (chafa-path (executable-find "chafa"))
                (img2sixel-path (executable-find "img2sixel")))
            
            (cond
             ((not (or chafa-path img2sixel-path))
              (jupyter-termint-stata-debug-log 'error "Neither chafa nor img2sixel found")
              (message "ERROR: No sixel converter available (need chafa or img2sixel)")
              'no-sixel-converter)
             
             ((not current-process)
              (jupyter-termint-stata-debug-log 'error "No terminal process found in buffer %s" buffer-name)
              (message "ERROR: No terminal process for sixel output")
              'no-process)
             
             (t
              (condition-case err
                  (progn
                    (jupyter-termint-stata-debug-log 'info "Executing %s for sixel conversion..." (if chafa-path "chafa" "img2sixel"))
                    ;; Get sixel output using chafa (preferred) or img2sixel fallback
                    (let ((sixel-output (with-temp-buffer
                                          (let ((exit-code 
                                                 (if chafa-path
                                                     ;; Use chafa with sixel format
                                                     (call-process chafa-path nil t nil 
                                                                   "--format=sixel" "--size=800x600" image-path)
                                                   ;; Fallback to img2sixel
                                                   (call-process img2sixel-path nil t nil 
                                                                 "-w" "800" "-h" "600" image-path))))
                                            (if (= exit-code 0)
                                                (buffer-string)
                                              nil)))))
                      (if sixel-output
                          (progn
                            ;; Send sixel data through terminal process for proper rendering
                            (process-send-string current-process (concat "\n" sixel-output))
                            (jupyter-termint-stata-debug-log 'info "SUCCESS: Sixel data sent through terminal process")
                            (message "✅ Sixel graphics sent to terminal: %s" (file-name-nondirectory image-path))
                            'process-sixel)
                        (progn
                          (jupyter-termint-stata-debug-log 'error "img2sixel failed or produced no output")
                          (message "ERROR: img2sixel failed or produced no output")
                          'failed))))
                (error 
                 (jupyter-termint-stata-debug-log 'error "Sixel display error: %s" (error-message-string err))
                 (message "ERROR: Failed to display sixel graphics: %s" (error-message-string err))
                 'error))))))))))))

;;; Pure Sixel Alias Function

;;;###autoload
(defun jupyter-termint-display-image-inline (image-path buffer-name)
  "DEPRECATED: Use pure sixel display method only.
   
Args:
  IMAGE-PATH: Full path to the PNG file
  BUFFER-NAME: Name of vterm terminal buffer"
  (interactive "fImage file: \nbBuffer name: ")
  
  ;; Redirect to pure sixel implementation
  (jupyter-termint-display-image-at-cell image-path buffer-name))

;;;###autoload
(defun jupyter-stata-display-latest-graph ()
  "Display the most recent Stata graph inline."
  (interactive)
  (let* ((cache-dir (expand-file-name "~/.stata_kernel_cache"))
         (graph-files (when (file-directory-p cache-dir)
                        (directory-files cache-dir t "graph[0-9]+\\.png$")))
         (latest-file (when graph-files
                        (car (sort graph-files 
                                   (lambda (a b) 
                                     (> (float-time (nth 5 (file-attributes a)))
                                        (float-time (nth 5 (file-attributes b))))))))))
    (if latest-file
        (progn
          (message "Displaying graph: %s" latest-file)
          (jupyter-termint-stata-debug-log 'info "Manual display of latest graph: %s" latest-file)
          (jupyter-termint-display-image-at-cell latest-file "*jupyter-stata*"))
      (progn
        (jupyter-termint-stata-debug-log 'warn "No Stata graph files found in %s" cache-dir)
        (message "No Stata graph files found in %s" cache-dir)))))

;;;###autoload
(defun jupyter-stata-display-graph (graph-number)
  "Display a specific Stata graph by number."
  (interactive "nGraph number (0, 1, 2, etc.): ")
  (let ((graph-file (expand-file-name 
                     (format "graph%d.png" graph-number)
                     "~/.stata_kernel_cache")))
    (if (file-exists-p graph-file)
        (progn
          (message "Displaying graph %d: %s" graph-number graph-file)
          (jupyter-termint-stata-debug-log 'info "Manual display of graph %d: %s" graph-number graph-file)
          (jupyter-termint-display-image-at-cell graph-file "*jupyter-stata*"))
      (progn
        (jupyter-termint-stata-debug-log 'warn "Graph file not found: %s" graph-file)
        (message "Graph file not found: %s" graph-file)))))

;;;###autoload
(defun test-stata-image-display ()
  "Test native image display with latest Stata graph."
  (interactive)
  (let ((cache-dir (expand-file-name "~/.stata_kernel_cache"))
        (test-files (when (file-directory-p (expand-file-name "~/.stata_kernel_cache"))
                      (directory-files "~/.stata_kernel_cache" t "graph.*\\.png$"))))
    (if test-files
        (progn
          (message "Testing image display with: %s" (car test-files))
          (jupyter-termint-stata-debug-log 'info "Testing image display with: %s" (car test-files))
          (let ((result (jupyter-termint-display-image-at-cell (car test-files) "*jupyter-stata*")))
            (message "Test result: %s" result)
            result))
      (progn
        (jupyter-termint-stata-debug-log 'warn "No test image files found. Cache dir exists: %s" (file-directory-p cache-dir))
        (message "No test image files found. Run a Stata graphics command first.")
        nil))))

(defun jupyter-stata-display-image-popup (image-path)
  "REMOVED: No popup fallback in pure sixel implementation.
Pure sixel graphics must work directly in vterm terminal or fail completely."
  (jupyter-termint-stata-debug-log 'warn "Popup fallback called but disabled in pure sixel mode")
  (message "⚠️ Pure sixel mode: no popup fallback available")
  nil)

;;; Integration with Console Management

(defun jupyter-termint-smart-stata-start ()
  "Start Stata jupyter console with file monitoring."
  (interactive)
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-stata*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-stata*")))
  
  ;; Start monitoring before console starts
  (jupyter-termint-start-stata-graph-monitoring)
  
  ;; More robust environment variable setting with explicit command + eat backend for sixel
  (let ((smart-cmd "sh -c \"export TERM=xterm-kitty COLORTERM=truecolor JUPYTER_CONSOLE=1 && cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel stata\""))
    (jupyter-termint-stata-debug-log 'info "Starting Stata console with command: %s" smart-cmd)
    (termint-define "jupyter-stata" smart-cmd 
                    :bracketed-paste-p t
                    :backend 'eat
                    :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
    (termint-jupyter-stata-start)
    
    ;; Verify environment after startup
    (run-with-timer 3 nil 
                    (lambda ()
                      (when (get-buffer "*jupyter-stata*")
                        (with-current-buffer "*jupyter-stata*"
                          (let ((term-val (getenv "TERM"))
                                (colorterm-val (getenv "COLORTERM")))
                            (jupyter-termint-stata-debug-log 'info "Process environment: TERM=%s COLORTERM=%s" 
                                                           (or term-val "unset") (or colorterm-val "unset"))
                            (message "Stata console environment: TERM=%s COLORTERM=%s" 
                                   (or term-val "unset") (or colorterm-val "unset")))))))))

(defun jupyter-termint-ensure-stata-console (code)
  "Ensure Stata console is running with file monitoring, then send CODE."
  (let* ((buffer-name "*jupyter-stata*")
         (original-buffer (current-buffer))
         (original-window (selected-window)))
    
    ;; Check if console buffer exists and has a live process
    (let ((console-buffer (get-buffer buffer-name)))
      (if (and console-buffer 
               (buffer-live-p console-buffer)
               (get-buffer-process console-buffer)
               (process-live-p (get-buffer-process console-buffer)))
          (progn
            ;; Console exists, just send code
            (termint-jupyter-stata-send-string code))
        
        ;; Console doesn't exist, start it
        (progn
          (message "Starting Stata console with file monitoring...")
          (jupyter-termint-smart-stata-start)
          
          ;; Wait for buffer to be created
          (let ((max-wait 10) (wait-count 0))
            (while (and (< wait-count max-wait)
                       (not (get-buffer buffer-name)))
              (sleep-for 0.5)
              (setq wait-count (1+ wait-count)))
            
            (let ((new-buffer (get-buffer buffer-name)))
              (if new-buffer
                  (progn
                    (message "Stata console ready!")
                    (sleep-for 2)
                    (termint-jupyter-stata-send-string code))
                (error "Failed to create Stata console buffer")))))))))

;;; Python Console Integration

(defun jupyter-termint-smart-python-start ()
  "Start Python jupyter console with sixel support."
  (interactive)
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-python*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-python*")))
  
  ;; More robust environment variable setting with explicit command + vterm backend for sixel
  (let ((smart-cmd "sh -c \"export TERM=xterm-kitty COLORTERM=truecolor JUPYTER_CONSOLE=1 && cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel python3\""))
    (jupyter-termint-stata-debug-log 'info "Starting Python console with command: %s" smart-cmd)
    (termint-define "jupyter-python" smart-cmd 
                    :bracketed-paste-p t
                    :backend 'vterm
                    :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
    (termint-jupyter-python-start)
    
    ;; Verify environment after startup
    (run-with-timer 3 nil 
                    (lambda ()
                      (when (get-buffer "*jupyter-python*")
                        (with-current-buffer "*jupyter-python*"
                          (let ((term-val (getenv "TERM"))
                                (colorterm-val (getenv "COLORTERM")))
                            (jupyter-termint-stata-debug-log 'info "Python process environment: TERM=%s COLORTERM=%s" 
                                                           (or term-val "unset") (or colorterm-val "unset"))
                            (message "Python console environment: TERM=%s COLORTERM=%s" 
                                   (or term-val "unset") (or colorterm-val "unset")))))))))

(defun jupyter-termint-ensure-python-console (code)
  "Ensure Python console is running with sixel support, then send CODE."
  (let* ((buffer-name "*jupyter-python*")
         (original-buffer (current-buffer))
         (original-window (selected-window)))
    
    ;; Check if console buffer exists and has a live process
    (let ((console-buffer (get-buffer buffer-name)))
      (if (and console-buffer 
               (buffer-live-p console-buffer)
               (get-buffer-process console-buffer)
               (process-live-p (get-buffer-process console-buffer)))
          (progn
            ;; Console exists, just send code
            (termint-jupyter-python-send-string code))
        
        ;; Console doesn't exist, start it
        (progn
          (message "Starting Python console with sixel support...")
          (jupyter-termint-smart-python-start)
          
          ;; Wait for buffer to be created
          (let ((max-wait 10) (wait-count 0))
            (while (and (< wait-count max-wait)
                       (not (get-buffer buffer-name)))
              (sleep-for 0.5)
              (setq wait-count (1+ wait-count)))
            
            (let ((new-buffer (get-buffer buffer-name)))
              (if new-buffer
                  (progn
                    (message "Python console ready!")
                    (sleep-for 2)
                    (termint-jupyter-python-send-string code))
                (error "Failed to create Python console buffer")))))))))

;;; Split Window Layout Functions

(defun jupyter-termint-create-split-layout (buffer-name)
  "Create split window layout with jupyter buffer on the right."
  (when (get-buffer buffer-name)
    ;; Save current window configuration
    (let ((original-window (selected-window))
          (jupyter-window nil))
      
      ;; Check if jupyter buffer is already visible
      (setq jupyter-window (get-buffer-window buffer-name))
      
      (unless jupyter-window
        ;; Split window if not already done
        (when (= 1 (length (window-list)))
          (split-window-right))
        
        ;; Display jupyter buffer in right window
        (other-window 1)
        (switch-to-buffer buffer-name)
        (setq jupyter-window (selected-window))
        
        ;; Return to original window
        (select-window original-window))
      
      jupyter-window)))

;;; C-RET Integration

(defun jupyter-termint-send-stata ()
  "Send current region/line to Stata console with automatic graphics monitoring."
  (interactive)
  
  (let* ((code (cond
                ((use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end)))
                ((eq major-mode 'org-mode)
                 ;; In org-mode, extract the current code block
                 (let ((element (org-element-at-point)))
                   (if (eq (org-element-type element) 'src-block)
                       (org-element-property :value element)
                     (thing-at-point 'line t))))
                (t
                 (thing-at-point 'line t)))))
    
    ;; Track that we're about to execute a cell
    (jupyter-termint-track-cell-execution "*jupyter-stata*")
    (jupyter-termint-ensure-stata-console code)))

(defun jupyter-termint-send-python ()
  "Send current region/line to Python console with automatic graphics monitoring."
  (interactive)
  
  (let* ((code (cond
                ((use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end)))
                ((eq major-mode 'org-mode)
                 ;; In org-mode, extract the current code block
                 (let ((element (org-element-at-point)))
                   (if (eq (org-element-type element) 'src-block)
                       (org-element-property :value element)
                     (thing-at-point 'line t))))
                (t
                 (thing-at-point 'line t)))))
    
    ;; Track that we're about to execute a cell
    (jupyter-termint-track-cell-execution "*jupyter-python*")
    (jupyter-termint-ensure-python-console code)))

;;; Enhanced send functions with split layout

(defun jupyter-termint-send-stata-with-split ()
  "Send Stata code and create/show split layout."
  (interactive)
  (jupyter-termint-send-stata)
  (jupyter-termint-create-split-layout "*jupyter-stata*"))

(defun jupyter-termint-send-python-with-split ()
  "Send Python code and create/show split layout."
  (interactive)
  (jupyter-termint-send-python)
  (jupyter-termint-create-split-layout "*jupyter-python*"))

(provide 'jupyter-termint)
;;; jupyter-termint.el ends here