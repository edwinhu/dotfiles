;;; jupyter-termint-simplified.el --- Clean Jupyter console integration using termint and eat -*- lexical-binding: t; -*-

;;; Commentary:
;; This is a cleaned up version of jupyter-termint.el with simplified Stata graphics integration
;; using file monitoring instead of complex monkey-patching.

;;; Code:

(require 'termint nil t)
(require 'org)
(require 'ob)
(require 'org-src)

(defgroup jupyter-termint nil
  "Jupyter console integration using termint and eat."
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

(defvar jupyter-termint-stata-monitoring-timer nil
  "Timer for monitoring Stata graph files.")

(defvar jupyter-termint-stata-debug-log-file 
  (expand-file-name "jupyter-stata-debug.log" "~/")
  "Log file for Stata file monitoring debugging.")

(defun jupyter-termint-stata-debug-log (level format-string &rest args)
  "Log Stata file monitoring messages to file with timestamp."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) jupyter-termint-stata-debug-log-file))))

(defun jupyter-termint-check-for-new-stata-graphs ()
  "Check for new graph files in ~/.stata_kernel_cache/ and display them inline."
  (let* ((cache-dir (expand-file-name "~/.stata_kernel_cache/"))
         (graph-files (when (file-directory-p cache-dir)
                       (directory-files cache-dir t "graph[0-9]+\\.png$"))))
    (dolist (file graph-files)
      (let ((file-time (float-time (nth 5 (file-attributes file)))))
        (when (> file-time jupyter-termint-stata-last-graph-time)
          (jupyter-termint-stata-debug-log 'info "New graph detected: %s" file)
          (jupyter-termint-display-image-inline file "*jupyter-stata*")
          (setq jupyter-termint-stata-last-graph-time file-time))))))

(defun jupyter-termint-start-stata-graph-monitoring ()
  "Start monitoring for new Stata graph files."
  (interactive)
  (jupyter-termint-stata-debug-log 'info "Starting Stata graph file monitoring")
  (setq jupyter-termint-stata-last-graph-time (float-time (current-time)))
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

;;; Image Display Function (uses existing implementation)

(defun jupyter-termint-display-image-inline (image-file buffer-name)
  "Display IMAGE-FILE using existing image display functionality."
  (when (and image-file (file-exists-p image-file))
    (let ((target-buffer (get-buffer buffer-name)))
      (when target-buffer
        (with-current-buffer target-buffer
          (let ((inhibit-read-only t)
                (buffer-undo-list t))
            (goto-char (point-max))
            (insert "\n")
            ;; Use Emacs native image display
            (condition-case err
                (let ((image (create-image image-file nil nil :max-width 600 :max-height 400)))
                  (if image
                      (progn
                        (insert-image image)
                        (insert (format "\n[📊 %s - inline display]\n" (file-name-nondirectory image-file))))
                    (insert (format "[📊 %s - image creation failed]\n" (file-name-nondirectory image-file)))))
              (error 
               (insert (format "[📊 %s - image display error: %s]\n" 
                              (file-name-nondirectory image-file)
                              (error-message-string err)))))
            (goto-char (point-max))))))))

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
  
  ;; Define and start with smart direnv command
  (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel stata'"))
    (termint-define "jupyter-stata" smart-cmd 
                    :bracketed-paste-p t
                    :backend 'eat
                    :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
    (termint-jupyter-stata-start)))

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
    
    (jupyter-termint-ensure-stata-console code)))

(provide 'jupyter-termint-simplified)
;;; jupyter-termint-simplified.el ends here