;;; jupyter-console.el --- Jupyter console integration for org-babel -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple Jupyter console integration for org-babel without external dependencies
;; Supports Python, R, and Stata kernels using pure comint

;;; Code:

(require 'comint)
(require 'org)
(require 'ob)

(defgroup jupyter-console nil
  "Jupyter console integration for org-babel."
  :group 'org-babel)

(defcustom jupyter-console-log-file
  (expand-file-name "~/jupyter-console.log")
  "Path to the jupyter console debug log file."
  :type 'file
  :group 'jupyter-console)

(defcustom jupyter-console-prompt-regexp
  "\\(?:In \\[[0-9]+\\]: \\|>>> \\|\\.\\.\\. \\|> \\|\\.\\.\\.\\)"
  "Regular expression matching jupyter console prompts."
  :type 'string
  :group 'jupyter-console)

(defvar jupyter-console-buffers (make-hash-table :test 'equal)
  "Hash table mapping (file . kernel) pairs to their associated jupyter console buffers.")

(defvar jupyter-console-first-run (make-hash-table :test 'equal)
  "Track if this is the first command sent to a buffer.")

(defun jupyter-console-log (level format-string &rest args)
  "Log to jupyter console debug file.
LEVEL is the log level (info, debug, warn, error).
FORMAT-STRING and ARGS are passed to `format'."
  (when jupyter-console-log-file
    (let ((message (apply #'format format-string args))
          (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
      (with-temp-buffer
        (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
        (append-to-file (point-min) (point-max) jupyter-console-log-file)))))

(defun jupyter-console-clear-log ()
  "Clear the jupyter console log file."
  (interactive)
  (when (file-exists-p jupyter-console-log-file)
    (delete-file jupyter-console-log-file))
  (jupyter-console-log 'info "=== Jupyter Console Log Started ==="))

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
                                (min timeout 1.0))  ; Wait up to 1 second after first output
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
    
    (jupyter-console-log 'info "Starting jupyter console for kernel: %s" kernel)
    (jupyter-console-log 'debug "Buffer name: %s" buffer-name)
    (jupyter-console-log 'debug "Jupyter command: %s" jupyter-cmd)
    
    (with-current-buffer process-buffer
      (unless (comint-check-proc process-buffer)
        (let* ((cmd-args (list jupyter-cmd "console" "--kernel" kernel "--simple-prompt"))
               (default-directory (or (and file (file-name-directory file))
                                     default-directory)))
          (jupyter-console-log 'debug "Command: %s" (mapconcat 'identity cmd-args " "))
          (jupyter-console-log 'debug "Directory: %s" default-directory)
          
          (setq process (apply 'make-comint-in-buffer
                              (format "jupyter-console-%s" kernel)
                              process-buffer
                              (car cmd-args)
                              nil
                              (cdr cmd-args)))
          
          (when process
            (jupyter-console-log 'info "Process started successfully")
            (comint-mode)
            (setq-local comint-prompt-regexp jupyter-console-prompt-regexp)
            (setq-local comint-prompt-read-only t)
            (setq-local comint-scroll-to-bottom-on-output t)
            
            ;; Wait for the first prompt (kernels are usually ready in 2-5 seconds)
            (message "Waiting for %s kernel to be ready..." kernel)
            (if (jupyter-console-wait-for-prompt process-buffer 6)
                (progn
                  (message "%s kernel is ready!" kernel)
                  (jupyter-console-log 'info "Console ready for kernel: %s" kernel))
              ;; Even if we don't see a perfect prompt, if we got output, continue
              (message "%s kernel started (may still be initializing)" kernel)
              (jupyter-console-log 'info "Kernel %s started" kernel))))))
    
    process-buffer))

(defun jupyter-console-get-or-create (kernel &optional file)
  "Get existing or create new jupyter console for KERNEL.
Optionally associate with FILE."
  (let* ((key (cons file kernel))
         (existing-buffer (gethash key jupyter-console-buffers)))
    (if (and existing-buffer 
             (buffer-live-p existing-buffer)
             (comint-check-proc existing-buffer))
        (progn
          (jupyter-console-log 'debug "Using existing console buffer: %s" 
                              (buffer-name existing-buffer))
          existing-buffer)
      ;; Create new buffer and store it
      (let ((new-buffer (jupyter-console-start-process kernel file)))
        (puthash key new-buffer jupyter-console-buffers)
        new-buffer))))

(defun jupyter-console-send-string (buffer code)
  "Send CODE to jupyter console BUFFER and wait for output."
  (jupyter-console-log 'debug "Sending code to buffer %s:\n%s" 
                      (buffer-name buffer) code)
  
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer))
          (buffer-key (buffer-name buffer)))
      (unless proc
        (error "No process in buffer %s" (buffer-name buffer)))
      
      ;; Check if this is the first command to this buffer
      (let ((is-first (not (gethash buffer-key jupyter-console-first-run))))
        (when is-first
          (puthash buffer-key t jupyter-console-first-run)
          ;; For Stata, send a dummy command first to clear banner
          (when (string-match "stata" buffer-key)
            (jupyter-console-log 'debug "Clearing Stata banner with dummy command")
            (goto-char (point-max))
            (comint-send-string proc "di \"\"\n")
            (sit-for 1)
            (goto-char (point-max))))
      
      ;; Move to end of buffer
      (goto-char (point-max))
      
      ;; Mark where we are before sending
      (let ((output-start (point-marker)))
        
        ;; Send the code
        (comint-send-string proc (concat code "\n"))
        
        ;; Wait for output - simple commands are fast but some kernels need more time
        (jupyter-console-wait-for-prompt buffer 3)
        
        ;; Get everything between our mark and the new prompt
        (goto-char (point-max))
        (let* ((output-end (point))
               (raw-output (buffer-substring-no-properties output-start output-end)))
          
          (jupyter-console-log 'debug "Raw output:\n%s" raw-output)
          
          ;; Extract the actual result
          (jupyter-console-extract-output raw-output code)))))))

(defun jupyter-console-extract-output (raw-output code)
  "Extract clean output from RAW-OUTPUT, removing CODE echo and prompts."
  (let ((output raw-output))
    ;; Remove the echoed command
    (setq output (replace-regexp-in-string 
                  (concat "^" (regexp-quote code) "\n*") "" output))
    
    ;; Remove prompts
    (setq output (replace-regexp-in-string jupyter-console-prompt-regexp "" output))
    
    ;; Remove In[n]: patterns
    (setq output (replace-regexp-in-string "^In \\[[0-9]+\\]: *" "" output))
    
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
     
     ;; Plain output (for print statements)
     ((string-match "\\([^\n]+\\)" output)
      (let ((result (match-string 1 output)))
        ;; Clean up any remaining whitespace
        (string-trim result)))
     
     ;; Empty output
     (t nil))))

;; Define org-babel execution functions directly
(defun org-babel-execute:python (body params)
  "Execute Python BODY with PARAMS using jupyter console."
  (jupyter-console-log 'info "Executing Python code block")
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "python3" file))
         (result (jupyter-console-send-string buffer body)))
    (jupyter-console-log 'info "Python execution completed: %s" result)
    result))

(defun org-babel-execute:R (body params)
  "Execute R BODY with PARAMS using jupyter console."
  (jupyter-console-log 'info "Executing R code block")
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "ir" file))
         (result (jupyter-console-send-string buffer body)))
    (jupyter-console-log 'info "R execution completed: %s" result)
    result))

(defun org-babel-execute:stata (body params)
  "Execute Stata BODY with PARAMS using jupyter console."
  (jupyter-console-log 'info "Executing Stata code block")
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "stata" file))
         (result (jupyter-console-send-string buffer body)))
    (jupyter-console-log 'info "Stata execution completed: %s" result)
    result))

;; Initialize log
(jupyter-console-clear-log)
(jupyter-console-log 'info "Jupyter console module loaded")
(jupyter-console-log 'info "Emacs version: %s" emacs-version)
(jupyter-console-log 'info "Running in batch mode: %s" (if noninteractive "yes" "no"))

(provide 'jupyter-console)
;;; jupyter-console.el ends here