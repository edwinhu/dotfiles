;;; jupyter-console.el --- Jupyter console integration for org-babel -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple Jupyter console integration for org-babel without external dependencies
;; Supports Python, R, and Stata kernels using pure comint

;;; Code:

(require 'comint)
(require 'org)
(require 'ob)
(require 'cl-lib)

(defgroup jupyter-console nil
  "Jupyter console integration for org-babel."
  :group 'org-babel)

;; (defcustom jupyter-console-log-file
;;   (expand-file-name "~/jupyter-console.log")
;;   "Path to the jupyter console debug log file."
;;   :type 'file
;;   :group 'jupyter-console)

(defcustom jupyter-console-prompt-regexp
  "\\(?:In \\[[0-9]+\\]: \\|>>> \\|\\.\\.\\. \\|> \\|\\.\\.\\.\\)"
  "Regular expression matching jupyter console prompts."
  :type 'string
  :group 'jupyter-console)

(defcustom jupyter-console-auto-display-images t
  "Automatically detect and display images from plotting code."
  :type 'boolean
  :group 'jupyter-console)

(defcustom jupyter-console-image-directory
  (or (bound-and-true-p org-babel-temporary-directory)
      temporary-file-directory)
  "Directory for generated image files."
  :type 'directory
  :group 'jupyter-console)

(defcustom jupyter-console-image-format "png"
  "Default format for auto-generated images."
  :type '(choice (const "png")
                 (const "pdf")
                 (const "svg"))
  :group 'jupyter-console)

(defcustom jupyter-console-image-width 8
  "Default width for generated images (in inches for R, figure size for Python)."
  :type 'number
  :group 'jupyter-console)

(defcustom jupyter-console-image-height 6
  "Default height for generated images (in inches for R, figure size for Python)."
  :type 'number
  :group 'jupyter-console)

(defcustom jupyter-console-image-dpi 150
  "Default DPI for generated images."
  :type 'number
  :group 'jupyter-console)

(defvar jupyter-console-buffers (make-hash-table :test 'equal)
  "Hash table mapping (file . kernel) pairs to their associated jupyter console buffers.")

(defvar jupyter-console-first-run (make-hash-table :test 'equal)
  "Track if this is the first command sent to a buffer.")

(defvar jupyter-console-generated-images nil
  "List of image files generated in the current session for cleanup.")

(defvar jupyter-console-image-counter 0
  "Counter for generating unique image filenames.")

;; (defun jupyter-console-log (level format-string &rest args)
;;   "Log to jupyter console debug file.
;; LEVEL is the log level (info, debug, warn, error).
;; FORMAT-STRING and ARGS are passed to `format'."
;;   (when jupyter-console-log-file
;;     (let ((message (apply #'format format-string args))
;;           (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
;;       (with-temp-buffer
;;         (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
;;         (append-to-file (point-min) (point-max) jupyter-console-log-file)))))

;; (defun jupyter-console-clear-log ()
;;   "Clear the jupyter console log file."
;;   (interactive)
;;   (when (file-exists-p jupyter-console-log-file)
;;     (delete-file jupyter-console-log-file))
;;   (jupyter-console-log 'info "=== Jupyter Console Log Started ==="))

;;; Graphics and Image Detection Functions

(defun jupyter-console-detect-graphics-code (code kernel)
  "Detect if CODE contains plotting commands for the given KERNEL.
Returns non-nil if graphics code is detected."
  (when jupyter-console-auto-display-images
    (let ((code-lower (downcase code)))
      (pcase kernel
        ("python3" (jupyter-console--detect-python-graphics code-lower))
        ("ir" (jupyter-console--detect-r-graphics code-lower))
        ("stata" (jupyter-console--detect-stata-graphics code-lower))
        (_ nil)))))

(defun jupyter-console--detect-python-graphics (code)
  "Detect matplotlib/plotting code in Python CODE."
  (or (string-match-p "\\bplt\\." code)
      (string-match-p "\\bmatplotlib\\b" code)
      (string-match-p "\\bplot(" code)
      (string-match-p "\\bhistogram\\b" code)
      (string-match-p "\\bscatter\\b" code)
      (string-match-p "\\bbar(" code)
      (string-match-p "\\bseaborn\\b" code)
      (string-match-p "\\bsns\\." code)
      (string-match-p "\\bplotly\\b" code)))

(defun jupyter-console--detect-r-graphics (code)
  "Detect ggplot2/base graphics code in R CODE."
  (or (string-match-p "\\bggplot\\b" code)
      (string-match-p "\\bplot(" code)
      (string-match-p "\\bhist(" code)
      (string-match-p "\\bbarplot\\b" code)
      (string-match-p "\\bbox(" code)
      (string-match-p "\\blines(" code)
      (string-match-p "\\bpoints(" code)
      (string-match-p "\\bgrid\\.arrange\\b" code)
      (string-match-p "\\bggplot2\\b" code)))

(defun jupyter-console--detect-stata-graphics (code)
  "Detect graphics commands in Stata CODE."
  (or (string-match-p "\\bgraph\\b" code)
      (string-match-p "\\btwoway\\b" code)
      (string-match-p "\\bhistogram\\b" code)
      (string-match-p "\\bscatter\\b" code)
      (string-match-p "\\bbar\\b" code)))

;;; Image File Management Functions

(defun jupyter-console-generate-image-filename (kernel)
  "Generate a unique image filename for KERNEL."
  (setq jupyter-console-image-counter (1+ jupyter-console-image-counter))
  (let ((filename (format "jupyter-console-%s-%d-%s.%s"
                         kernel
                         jupyter-console-image-counter
                         (format-time-string "%Y%m%d-%H%M%S")
                         jupyter-console-image-format)))
    (expand-file-name filename jupyter-console-image-directory)))

(defun jupyter-console-track-generated-image (filepath)
  "Add FILEPATH to the list of generated images for cleanup."
  (push filepath jupyter-console-generated-images))

(defun jupyter-console-cleanup-old-images (&optional keep-recent)
  "Clean up old generated image files.
If KEEP-RECENT is non-nil, keep the most recent images."
  (interactive)
  (let ((keep-count (or keep-recent 10)))
    (when (> (length jupyter-console-generated-images) keep-count)
      (let ((to-delete (nthcdr keep-count jupyter-console-generated-images)))
        (dolist (file to-delete)
          (when (and (file-exists-p file)
                     (string-match-p "jupyter-console-" (file-name-nondirectory file)))
            (delete-file file)))
        (setq jupyter-console-generated-images
              (cl-subseq jupyter-console-generated-images 0 keep-count))))))

;;; Code Injection Functions

(defun jupyter-console-inject-save-commands (code kernel)
  "Inject image-saving commands into CODE for KERNEL.
Returns modified code if graphics detected, original code otherwise."
  (if (jupyter-console-detect-graphics-code code kernel)
      (let ((image-file (jupyter-console-generate-image-filename kernel)))
        (jupyter-console-track-generated-image image-file)
        (pcase kernel
          ("python3" (jupyter-console--inject-python-save code image-file))
          ("ir" (jupyter-console--inject-r-save code image-file))
          ("stata" (jupyter-console--inject-stata-save code image-file))
          (_ code)))
    code))

(defun jupyter-console-inject-save-commands-to-file (code kernel target-file)
  "Inject image-saving commands into CODE for KERNEL using specific TARGET-FILE.
Returns modified code if graphics detected, original code otherwise."
  (if (jupyter-console-detect-graphics-code code kernel)
      (progn
        (jupyter-console-track-generated-image target-file)
        (pcase kernel
          ("python3" (jupyter-console--inject-python-save code target-file))
          ("ir" (jupyter-console--inject-r-save code target-file))
          ("stata" (jupyter-console--inject-stata-save code target-file))
          (_ code)))
    code))

(defun jupyter-console--inject-python-save (code image-file)
  "Inject matplotlib save commands into Python CODE."
  (let ((save-code (format "
# Set up matplotlib for headless operation
import matplotlib
matplotlib.use('Agg')  # Use non-GUI backend
import matplotlib.pyplot as plt
plt.ioff()  # Turn off interactive mode

# User code
%s

# Save plot to file
print('Saving plot to: %s')
plt.savefig(r'%s', bbox_inches='tight', dpi=%d, facecolor='white', edgecolor='none')
print('Plot saved successfully')
" code image-file image-file jupyter-console-image-dpi)))
    save-code))

(defun jupyter-console--inject-r-save (code image-file)
  "Inject R save commands into R CODE."
  (let ((save-code 
         (if (string-match-p "\\bggplot\\b" (downcase code))
             ;; ggplot2 code - use ggsave
             (format "%s
ggsave('%s', width=%d, height=%d, dpi=%d, bg='white')
" code image-file jupyter-console-image-width jupyter-console-image-height jupyter-console-image-dpi)
           ;; Base graphics - use device approach
           (format "%s
dev.copy(%s, filename='%s', width=%d, height=%d, res=%d, bg='white')
dev.off()
" code 
              (if (string= jupyter-console-image-format "png") "png" "pdf")
              image-file 
              (* jupyter-console-image-width jupyter-console-image-dpi)
              (* jupyter-console-image-height jupyter-console-image-dpi)
              jupyter-console-image-dpi))))
    save-code))

(defun jupyter-console--inject-stata-save (code image-file)
  "Inject Stata save commands into Stata CODE."
  (format "%s
graph export \"%s\", replace
" code image-file))

;;; Image Result Processing Functions

(defun jupyter-console-check-for-generated-images ()
  "Check if any images were recently generated and return the most recent one.
Returns the file path if found, nil otherwise."
  (when jupyter-console-generated-images
    (let ((recent-image (car jupyter-console-generated-images)))
      (when (and recent-image (file-exists-p recent-image))
        recent-image))))

(defun jupyter-console-send-string-with-images (buffer code kernel &optional image-file)
  "Enhanced version of jupyter-console-send-string that handles image generation.
BUFFER is the jupyter console buffer, CODE is the code to execute, 
KERNEL is the kernel type (python3, ir, stata).
IMAGE-FILE is the optional specific file path to save the image to."
  (let* ((original-code code)
         (has-graphics (jupyter-console-detect-graphics-code code kernel))
         (target-file (or image-file 
                         (when has-graphics (jupyter-console-generate-image-filename kernel))))
         (modified-code (if has-graphics 
                           (jupyter-console-inject-save-commands-to-file code kernel target-file)
                         code))
         (text-result (jupyter-console-send-string buffer modified-code)))
    
    (message "jupyter-console: has-graphics=%s, target-file=%s" has-graphics target-file)
    (message "jupyter-console: text-result=%s" text-result)
    
    ;; Check if file was created after a short delay
    (when (and has-graphics target-file)
      (sit-for 0.5)  ; Wait a bit for file creation
      (message "jupyter-console: File exists after wait: %s" (file-exists-p target-file)))
    
    ;; Return appropriate result based on what we found
    (cond
     ;; If we have a target file and graphics, return the file path regardless of existence for now
     ((and has-graphics target-file)
      (message "jupyter-console: Returning file path: %s" target-file)
      target-file)
     ;; Otherwise return text result
     (t 
      (message "jupyter-console: Returning text result")
      text-result))))

(defun jupyter-console-display-image-in-buffer (image-file buffer)
  "Display IMAGE-FILE in BUFFER or in a popup window."
  (when (and image-file (file-exists-p image-file))
    (let ((image-buffer (get-buffer-create "*Jupyter Console Image*")))
      (with-current-buffer image-buffer
        (erase-buffer)
        (insert-image (create-image image-file))
        (goto-char (point-min))
        (read-only-mode 1))
      ;; Display in a popup window
      (display-buffer image-buffer
                      '((display-buffer-reuse-window
                         display-buffer-pop-up-window)
                        (window-height . 0.6)
                        (window-width . 0.6))))))

(defun jupyter-console-buffer-name (kernel file)
  "Generate buffer name for KERNEL associated with FILE."
  (format "*jupyter-console[%s]:%s*" 
          kernel 
          (file-name-nondirectory (or file "global"))))

(defun jupyter-console-wait-for-prompt (buffer &optional timeout)
  "Wait for a prompt to appear in BUFFER.
TIMEOUT defaults to 3 seconds."
  (let ((timeout (or timeout 3))
        (start-time (current-time))
        (initial-point nil)
        (output-received nil))
    (with-current-buffer buffer
      (goto-char (point-max))
      (setq initial-point (point))
      ;; Wait for new output and a prompt
      (while (and (< (time-to-seconds (time-subtract (current-time) start-time)) timeout)
                  (progn
                    (goto-char (point-max))
                    ;; Check if we got output
                    (when (> (point) initial-point)
                      (setq output-received t))
                    ;; Keep waiting if we haven't seen a prompt yet
                    ;; But if we got output, wait a bit for more
                    (if output-received
                        ;; Got output, wait a little for prompt or more output
                        (and (< (time-to-seconds (time-subtract (current-time) start-time)) 
                                timeout)  ; Wait full timeout for R/Stata
                             (not (looking-back jupyter-console-prompt-regexp 
                                               (max (point-min) (- (point) 100)))))
                      ;; No output yet, keep waiting
                      t)))
        (sit-for 0.05))
      ;; Return true if we found a prompt or got output
      (or (looking-back jupyter-console-prompt-regexp 
                       (max (point-min) (- (point) 100)))
          output-received))))

(defun jupyter-console-start-process (kernel &optional file)
  "Start a jupyter console process for KERNEL.
Optionally associate it with FILE."
  (let* ((buffer-name (jupyter-console-buffer-name kernel file))
         (jupyter-cmd (or (and (fboundp 'find-pixi-jupyter) (find-pixi-jupyter))
                         (executable-find "jupyter")
                         "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter"))
         (process-buffer (get-buffer-create buffer-name))
         process)
    
    ;; Show user feedback
    (message "Starting Jupyter %s kernel... (this may take a few seconds)" kernel)
    
    ;; (jupyter-console-log 'info "Starting jupyter console for kernel: %s" kernel)
    ;; (jupyter-console-log 'debug "Buffer name: %s" buffer-name)
    ;; (jupyter-console-log 'debug "Jupyter command: %s" jupyter-cmd)
    
    (with-current-buffer process-buffer
      (unless (comint-check-proc process-buffer)
        (let* ((cmd-args (list jupyter-cmd "console" "--kernel" kernel "--simple-prompt"))
               (default-directory (or (and file (file-name-directory file))
                                     default-directory)))
          ;; (jupyter-console-log 'debug "Command: %s" (mapconcat 'identity cmd-args " "))
          ;; (jupyter-console-log 'debug "Directory: %s" default-directory)
          
          (setq process (apply 'make-comint-in-buffer
                              (format "jupyter-console-%s" kernel)
                              process-buffer
                              (car cmd-args)
                              nil
                              (cdr cmd-args)))
          
          (when process
            ;; (jupyter-console-log 'info "Process started successfully")
            (comint-mode)
            (setq-local comint-prompt-regexp jupyter-console-prompt-regexp)
            (setq-local comint-prompt-read-only t)
            (setq-local comint-scroll-to-bottom-on-output t)
            
            ;; Wait for the first prompt (kernels are usually ready in 2-5 seconds)
            (message "Waiting for %s kernel to be ready..." kernel)
            (if (jupyter-console-wait-for-prompt process-buffer 6)
                (progn
                  (message "%s kernel is ready!" kernel)
                  ;; (jupyter-console-log 'info "Console ready for kernel: %s" kernel)
                  )
              ;; Even if we don't see a perfect prompt, if we got output, continue
              (message "%s kernel started (may still be initializing)" kernel)
              ;; (jupyter-console-log 'info "Kernel %s started" kernel)
)))))
    
    process-buffer))

(defun jupyter-console-get-or-create (kernel &optional file)
  "Get existing or create new jupyter console for KERNEL.
Optionally associate with FILE."
  ;; In org-src-mode, use the original org file
  (let* ((actual-file (or file
                          (and (boundp 'org-src-source-file-name)
                               org-src-source-file-name)
                          (buffer-file-name)))
         (key (cons actual-file kernel))
         (existing-buffer (gethash key jupyter-console-buffers)))
    ;; (jupyter-console-log 'debug "Getting console for kernel=%s file=%s" kernel actual-file)
    (if (and existing-buffer 
             (buffer-live-p existing-buffer)
             (comint-check-proc existing-buffer))
        (progn
          ;; (jupyter-console-log 'debug "Using existing console buffer: %s" 
          ;;                     (buffer-name existing-buffer))
          existing-buffer)
      ;; Create new buffer and store it
      (let ((new-buffer (jupyter-console-start-process kernel actual-file)))
        (puthash key new-buffer jupyter-console-buffers)
        new-buffer))))

(defun jupyter-console-send-string (buffer code)
  "Send CODE to jupyter console BUFFER and wait for output."
  ;; (jupyter-console-log 'debug "Sending code to buffer %s:\n%s" 
  ;;                     (buffer-name buffer) code)
  
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer))
          (buffer-key (buffer-name buffer)))
      (unless proc
        (error "No process in buffer %s" (buffer-name buffer)))
      
      ;; Mark that we've used this buffer (but don't send dummy commands)
      (puthash buffer-key t jupyter-console-first-run)
      
      ;; Move to end of buffer
      (goto-char (point-max))
      
      ;; Mark where we are before sending
      (let ((output-start (point-marker)))
        
        ;; Send the code
        (comint-send-string proc (concat code "\n"))
        
        ;; Wait for output - matplotlib may need more time than regular Python
        (let ((wait-time (if (string-match-p "matplotlib\\|plt\\." code) 10 3)))
          (jupyter-console-wait-for-prompt buffer wait-time))
        
        ;; Get everything between our mark and the new prompt
        (goto-char (point-max))
        (let* ((output-end (point))
               (raw-output (buffer-substring-no-properties output-start output-end)))
          
          ;; (jupyter-console-log 'debug "Raw output:\n%s" raw-output)
          
          ;; Extract the actual result
          (jupyter-console-extract-output raw-output code))))))

(defun jupyter-console-extract-output (raw-output code)
  "Extract clean output from RAW-OUTPUT, removing CODE echo and prompts."
  ;; (jupyter-console-log 'debug "Extracting output from:\n%s" raw-output)
  (let ((output raw-output))
    ;; Remove the echoed command
    (setq output (replace-regexp-in-string 
                  (concat "^" (regexp-quote code) "\n*") "" output))
    
    ;; Remove prompts
    (setq output (replace-regexp-in-string jupyter-console-prompt-regexp "" output))
    
    ;; Remove In[n]: patterns
    (setq output (replace-regexp-in-string "^In \\[[0-9]+\\]: *" "" output))
    
    ;; Trim whitespace
    (setq output (string-trim output))
    
    ;; (jupyter-console-log 'debug "After cleaning: '%s'" output)
    
    ;; If we have any output after cleaning, return it
    (if (and output (not (string-empty-p output)))
        ;; Extract output based on patterns
        (cond
         ;; Python/IPython style output: Out[n]: value
         ((string-match "Out\\[[0-9]+\\]: \\(.*\\)" output)
          (match-string 1 output))
         
         ;; R style output: [1] "value"
         ((string-match "\\[1\\] \"\\(.*\\)\"" output)
          (match-string 1 output))
         
         ;; R style output without quotes: [1] value
         ((string-match "\\[1\\] \\(.*\\)" output)
          (match-string 1 output))
         
         ;; Plain output - just return as is
         (t output))
      ;; Return nil for empty output, but log it
      (progn
        ;; (jupyter-console-log 'warn "No output extracted from execution")
        nil))))

;; Define org-babel execution functions directly
(defun org-babel-execute:python (body params)
  "Execute Python BODY with PARAMS using jupyter console."
  (message "jupyter-console: Executing Python code block")
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "python3" file))
         (result (jupyter-console-send-string-with-images buffer body "python3")))
    (message "jupyter-console: Python execution completed, result type: %s, result: %s" 
             (type-of result) result)
    result))

(defun org-babel-execute:R (body params)
  "Execute R BODY with PARAMS using jupyter console."
  ;; (jupyter-console-log 'info "Executing R code block")
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "ir" file))
         (result (jupyter-console-send-string-with-images buffer body "ir")))
    ;; (jupyter-console-log 'info "R execution completed: %s" result)
    result))

(defun org-babel-execute:stata (body params)
  "Execute Stata BODY with PARAMS using jupyter console."
  ;; (jupyter-console-log 'info "Executing Stata code block")
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "stata" file))
         (result (jupyter-console-send-string-with-images buffer body "stata")))
    ;; (jupyter-console-log 'info "Stata execution completed: %s" result)
    result))

;;; Window management

(defvar jupyter-console--org-src-window-config nil
  "Window configuration saved when first showing jupyter console in org-src mode.")

(defun jupyter-console-display-buffer (buffer)
  "Display jupyter console BUFFER in a side window."
  ;; Save window config in org-src mode before first split
  (when (and (bound-and-true-p org-src-mode)
             (not jupyter-console--org-src-window-config))
    (setq jupyter-console--org-src-window-config (current-window-configuration)))
  (display-buffer buffer
                  '((display-buffer-reuse-window
                     display-buffer-in-side-window)
                    (side . right)
                    (window-width . 0.5)
                    (inhibit-same-window . t))))

;;; Evaluation functions with stepping

(defun jupyter-console-get-function-bounds ()
  "Get the bounds of the current function.
Returns a cons cell (START . END) or nil if not in a function."
  ;; Only try to detect functions in modes that have proper function detection
  (when (or (derived-mode-p 'python-mode 'python-ts-mode)
            (derived-mode-p 'ess-r-mode 'ess-mode))
    (condition-case nil
        (save-excursion
          (let ((orig-point (point))
                (start (progn (beginning-of-defun) (point)))
                (end (progn (end-of-defun) (point))))
            ;; Make sure we found a real function and we're inside it
            (when (and (> end start)
                       (>= orig-point start)
                       (<= orig-point end)
                       ;; Ensure it's actually a function (has reasonable size)
                       (> (- end start) 10))
              (cons start end))))
      (error nil))))

(defun jupyter-console-get-paragraph-bounds ()
  "Get the bounds of the current paragraph.
Returns a cons cell (START . END)."
  (save-excursion
    ;; In org-src buffers, just use the whole buffer if it's small
    (if (and (bound-and-true-p org-src-mode)
             (< (buffer-size) 1000))  ; Small buffer, likely single code block
        (cons (point-min) (point-max))
      ;; Otherwise use normal paragraph detection
      (let ((start (progn (backward-paragraph) 
                         (skip-chars-forward " \t\n")
                         (point)))
            (end (progn (forward-paragraph)
                       (skip-chars-backward " \t\n")
                       (point))))
        (cons start end)))))

(defun jupyter-console-send-region (beg end &optional kernel file show-buffer)
  "Send region from BEG to END to jupyter console.
KERNEL and FILE are used to identify the console buffer.
If SHOW-BUFFER is non-nil, display the console buffer."
  (let* ((code (buffer-substring-no-properties beg end))
         (trimmed-code (string-trim code))
         (kernel (or kernel (jupyter-console-detect-kernel)))
         ;; Use org-src-source-file-name when in org-src-mode
         (file (or file 
                   (and (boundp 'org-src-source-file-name)
                        org-src-source-file-name)
                   (buffer-file-name)))
         (buffer (if (string= kernel "sas-console")
                     ;; Use SAS console system for sas-console kernel
                     (when (fboundp 'sas-console-get-or-create)
                       (if (bound-and-true-p org-src-mode)
                           ;; In org-src-mode, get session and use source file
                           (let* ((source-file (or (and (boundp 'org-src-source-file-name)
                                                        org-src-source-file-name)
                                                   file))
                                  (session (or (when (fboundp 'sas-console-get-org-src-session)
                                                 (sas-console-get-org-src-session))
                                               ;; Fallback: scan source file directly
                                               (when source-file
                                                 (with-current-buffer (find-file-noselect source-file)
                                                   (save-excursion
                                                     (goto-char (point-min))
                                                     (when (re-search-forward "^\\s-*#\\+begin_src\\s-+sas\\b.*:session\\s-+\\([^[:space:]]+\\)" nil t)
                                                       (match-string 1))))))))
                             ;; Debug logging for C-RET workflow
                             (when (fboundp 'wrds-debug-log)
                               (wrds-debug-log 'info "C-RET SAS workflow: session=%s, source-file=%s" session source-file))
                             (sas-console-get-or-create session source-file))
                         ;; Not in org-src-mode, use file directly
                         (sas-console-get-or-create nil file)))
                   ;; Use standard jupyter console for other kernels
                   (jupyter-console-get-or-create kernel file))))
    ;; (jupyter-console-log 'debug "Sending region [%d-%d] to %s kernel: '%s'" beg end kernel trimmed-code)
    ;; Don't send empty code
    (when (and trimmed-code (not (string-empty-p trimmed-code)))
      (when show-buffer
        (jupyter-console-display-buffer buffer))
      (if (string= kernel "sas-console")
          ;; Use SAS console send function
          (when (fboundp 'sas-console-send-string)
            (sas-console-send-string buffer trimmed-code))
        ;; Use standard jupyter console send function with image support
        (let ((result (jupyter-console-send-string-with-images buffer trimmed-code kernel)))
          ;; If we got an image result in interactive mode, show it
          (when (and show-buffer result (file-exists-p result))
            (jupyter-console-display-image-in-buffer result buffer))
          result)))
    ;; Return result even if empty to avoid errors
    trimmed-code))

(defun jupyter-console-detect-kernel ()
  "Detect the appropriate kernel based on the current major mode."
  (cond
   ;; For org-src-mode, check the language first (takes priority)
   ((and (bound-and-true-p org-src-mode)
         (boundp 'org-src--babel-type))
    (pcase org-src--babel-type
      ((or "python" "jupyter-python") "python3")
      ((or "R" "jupyter-R") "ir")
      ((or "stata" "jupyter-stata") "stata")
      ((or "sas" "jupyter-sas") "sas-console")
      (_ (error "Unknown language: %s" org-src--babel-type))))
   ;; Check for SAS before general ESS mode
   ((and (derived-mode-p 'ess-mode)
         (boundp 'ess-dialect)
         (string= ess-dialect "SAS")) "sas-console")
   ((derived-mode-p 'SAS-mode 'ess-sas-mode) "sas-console")
   ;; Python modes
   ((derived-mode-p 'python-mode 'python-ts-mode) "python3")
   ;; R and other ESS modes
   ((derived-mode-p 'ess-r-mode 'ess-mode) "ir")
   ((derived-mode-p 'stata-mode) "stata")
   (t (error "Cannot detect kernel for mode: %s" major-mode))))

(defun jupyter-console-next-code-line ()
  "Move to the next line containing code (skip comments and blank lines)."
  (forward-line 1)
  (while (and (not (eobp))
              (or (looking-at "^[[:space:]]*$")  ; blank line
                  (looking-at "^[[:space:]]*#")   ; Python/R comment
                  (looking-at "^[[:space:]]*\\*")  ; Stata comment
                  (looking-at "^[[:space:]]*//")   ; C-style comment
                  ))
    (forward-line 1))
  (beginning-of-line))

(defun jupyter-console-eval-region-or-function-or-paragraph-and-step ()
  "Send region, function, or paragraph to jupyter console and step.
If region is active, send region. Otherwise try function, then paragraph.
After evaluation, step to the next code line."
  (interactive)
  (let ((kernel (jupyter-console-detect-kernel))
        (file (or (and (boundp 'org-src-source-file-name)
                       org-src-source-file-name)
                  (buffer-file-name)))
        (stepped nil))
    (cond
     ;; Region is active
     ((use-region-p)
      (jupyter-console-send-region (region-beginning) (region-end) kernel file t)
      (goto-char (region-end))
      (jupyter-console-next-code-line))
     
     ;; Try function bounds
     ((jupyter-console-get-function-bounds)
      (let ((bounds (jupyter-console-get-function-bounds)))
        (jupyter-console-send-region (car bounds) (cdr bounds) kernel file t)
        (goto-char (cdr bounds))
        (jupyter-console-next-code-line)))
     
     ;; Fall back to paragraph
     (t
      (let ((bounds (jupyter-console-get-paragraph-bounds)))
        (jupyter-console-send-region (car bounds) (cdr bounds) kernel file t)
        (goto-char (cdr bounds))
        (jupyter-console-next-code-line))))))

(defun jupyter-console-eval-line-and-step ()
  "Send current line to jupyter console and step to next line."
  (interactive)
  (let ((kernel (jupyter-console-detect-kernel))
        (file (or (and (boundp 'org-src-source-file-name)
                       org-src-source-file-name)
                  (buffer-file-name))))
    (jupyter-console-send-region
     (line-beginning-position)
     (line-end-position)
     kernel file t)
    (jupyter-console-next-code-line)))

(defun jupyter-console-eval-region-or-line-and-step ()
  "Send region if active, otherwise current line, then step."
  (interactive)
  (if (use-region-p)
      (let ((kernel (jupyter-console-detect-kernel))
            (file (or (and (boundp 'org-src-source-file-name)
                           org-src-source-file-name)
                      (buffer-file-name))))
        (jupyter-console-send-region (region-beginning) (region-end) kernel file t)
        (goto-char (region-end))
        (jupyter-console-next-code-line))
    (jupyter-console-eval-line-and-step)))

;;; Language-specific wrapper functions

(defun jupyter-console-python-eval-and-step ()
  "Python-specific eval and step function for C-RET."
  (interactive)
  (jupyter-console-eval-region-or-function-or-paragraph-and-step))

(defun jupyter-console-r-eval-and-step ()
  "R-specific eval and step function for C-RET."
  (interactive)
  (jupyter-console-eval-region-or-function-or-paragraph-and-step))

(defun jupyter-console-stata-eval-and-step ()
  "Stata-specific eval and step function for C-RET."
  (interactive)
  (jupyter-console-eval-region-or-function-or-paragraph-and-step))

;;; Keybinding setup functions

(defun jupyter-console-setup-python-keybindings ()
  "Set up C-RET keybinding for Python mode."
  (define-key python-mode-map (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
  (when (boundp 'python-ts-mode-map)
    (define-key python-ts-mode-map (kbd "C-<return>") #'jupyter-console-python-eval-and-step))
  ;; Evil mode support - bind in all states to avoid conflicts
  (with-eval-after-load 'evil
    (evil-define-key 'normal python-mode-map (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
    (evil-define-key 'insert python-mode-map (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
    (evil-define-key 'visual python-mode-map (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
    (when (boundp 'python-ts-mode-map)
      (evil-define-key 'normal python-ts-mode-map (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
      (evil-define-key 'insert python-ts-mode-map (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
      (evil-define-key 'visual python-ts-mode-map (kbd "C-<return>") #'jupyter-console-python-eval-and-step))))

(defun jupyter-console-setup-r-keybindings ()
  "Set up C-RET keybinding for R/ESS mode."
  (when (boundp 'ess-r-mode-map)
    (define-key ess-r-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
    ;; Evil mode support
    (with-eval-after-load 'evil
      (evil-define-key 'normal ess-r-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
      (evil-define-key 'insert ess-r-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
      (evil-define-key 'visual ess-r-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)))
  (when (boundp 'ess-mode-map)
    (define-key ess-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
    ;; Evil mode support
    (with-eval-after-load 'evil
      (evil-define-key 'normal ess-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
      (evil-define-key 'insert ess-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
      (evil-define-key 'visual ess-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step))))

(defun jupyter-console-setup-sas-keybindings ()
  "Set up C-RET keybinding for SAS/ESS mode."
  ;; ESS SAS mode
  (when (boundp 'ess-sas-mode-map)
    (define-key ess-sas-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
    ;; Evil mode support
    (with-eval-after-load 'evil
      (when (fboundp 'evil-define-key)
        (evil-define-key 'normal ess-sas-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
        (evil-define-key 'insert ess-sas-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
        (evil-define-key 'visual ess-sas-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step))))
  ;; Also handle SAS-mode (non-ESS)
  (when (boundp 'SAS-mode-map)
    (define-key SAS-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
    ;; Evil mode support
    (with-eval-after-load 'evil
      (when (fboundp 'evil-define-key)
        (evil-define-key 'normal SAS-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
        (evil-define-key 'insert SAS-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
        (evil-define-key 'visual SAS-mode-map (kbd "C-<return>") #'jupyter-console-r-eval-and-step)))))

(defun jupyter-console-setup-stata-keybindings ()
  "Set up C-RET keybinding for Stata mode."
  ;; Stata mode may not have an explicit keymap, so we use a hook
  (add-hook 'stata-mode-hook
            (lambda ()
              (local-set-key (kbd "C-<return>") #'jupyter-console-stata-eval-and-step)
              ;; Evil mode support
              (with-eval-after-load 'evil
                (when (fboundp 'evil-local-set-key)
                  (evil-local-set-key 'normal (kbd "C-<return>") #'jupyter-console-stata-eval-and-step)
                  (evil-local-set-key 'insert (kbd "C-<return>") #'jupyter-console-stata-eval-and-step)
                  (evil-local-set-key 'visual (kbd "C-<return>") #'jupyter-console-stata-eval-and-step))))))

(defun jupyter-console-org-src-restore-windows ()
  "Restore window configuration when exiting org-src mode."
  (when jupyter-console--org-src-window-config
    (set-window-configuration jupyter-console--org-src-window-config)
    (setq jupyter-console--org-src-window-config nil)))

(defun jupyter-console-setup-org-src-keybindings ()
  "Set up keybindings when in org-src-mode."
  (when (and (bound-and-true-p org-src-mode)
             (boundp 'org-src--babel-type))
    ;; Set up window restoration on exit
    (add-hook 'kill-buffer-hook #'jupyter-console-org-src-restore-windows nil t)
    (let ((lang org-src--babel-type))
      (cond
       ((member lang '("python" "jupyter-python"))
        (local-set-key (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
        ;; Evil mode support in org-src buffers
        (when (and (boundp 'evil-mode) evil-mode (fboundp 'evil-local-set-key))
          (evil-local-set-key 'normal (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
          (evil-local-set-key 'insert (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
          (evil-local-set-key 'visual (kbd "C-<return>") #'jupyter-console-python-eval-and-step)))
       ((member lang '("R" "jupyter-R"))
        (local-set-key (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
        ;; Evil mode support in org-src buffers
        (when (and (boundp 'evil-mode) evil-mode (fboundp 'evil-local-set-key))
          (evil-local-set-key 'normal (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
          (evil-local-set-key 'insert (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
          (evil-local-set-key 'visual (kbd "C-<return>") #'jupyter-console-r-eval-and-step)))
       ((member lang '("stata" "jupyter-stata"))
        (local-set-key (kbd "C-<return>") #'jupyter-console-stata-eval-and-step)
        ;; Evil mode support in org-src buffers
        (when (and (boundp 'evil-mode) evil-mode (fboundp 'evil-local-set-key))
          (evil-local-set-key 'normal (kbd "C-<return>") #'jupyter-console-stata-eval-and-step)
          (evil-local-set-key 'insert (kbd "C-<return>") #'jupyter-console-stata-eval-and-step)
          (evil-local-set-key 'visual (kbd "C-<return>") #'jupyter-console-stata-eval-and-step)))
       ((member lang '("sas" "jupyter-sas"))
        (local-set-key (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
        ;; Evil mode support in org-src buffers
        (when (and (boundp 'evil-mode) evil-mode (fboundp 'evil-local-set-key))
          (evil-local-set-key 'normal (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
          (evil-local-set-key 'insert (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
          (evil-local-set-key 'visual (kbd "C-<return>") #'jupyter-console-r-eval-and-step)))))))

;;; Mode hooks setup

(defun jupyter-console-setup-keybindings ()
  "Set up all keybindings for jupyter-console."
  ;; Python mode
  (with-eval-after-load 'python
    (jupyter-console-setup-python-keybindings))
  
  ;; ESS/R mode
  (with-eval-after-load 'ess-r-mode
    (jupyter-console-setup-r-keybindings))
  
  ;; Stata mode - load immediately since it's a simple mode
  (jupyter-console-setup-stata-keybindings)
  ;; Also ensure it's loaded if stata-mode is loaded later
  (with-eval-after-load 'ob-stata
    (jupyter-console-setup-stata-keybindings))
  
  ;; SAS mode - load immediately and also after ESS loads
  (jupyter-console-setup-sas-keybindings)
  (with-eval-after-load 'ess
    (jupyter-console-setup-sas-keybindings))
  
  ;; Org-src-mode hook
  (add-hook 'org-src-mode-hook #'jupyter-console-setup-org-src-keybindings))

;; Initialize log
;; (jupyter-console-clear-log)
;; (jupyter-console-log 'info "Jupyter console module loaded")
;; (jupyter-console-log 'info "Emacs version: %s" emacs-version)
;; (jupyter-console-log 'info "Running in batch mode: %s" (if noninteractive "yes" "no"))

;; Set up keybindings
(jupyter-console-setup-keybindings)
;; (jupyter-console-log 'info "Keybindings configured for C-RET in Python, R, and Stata modes")

(provide 'jupyter-console)
;;; jupyter-console.el ends here