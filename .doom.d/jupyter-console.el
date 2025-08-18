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
  ;; In org-src-mode, use the original org file
  (let* ((actual-file (or file
                          (and (boundp 'org-src-source-file-name)
                               org-src-source-file-name)
                          (buffer-file-name)))
         (key (cons actual-file kernel))
         (existing-buffer (gethash key jupyter-console-buffers)))
    (jupyter-console-log 'debug "Getting console for kernel=%s file=%s" kernel actual-file)
    (if (and existing-buffer 
             (buffer-live-p existing-buffer)
             (comint-check-proc existing-buffer))
        (progn
          (jupyter-console-log 'debug "Using existing console buffer: %s" 
                              (buffer-name existing-buffer))
          existing-buffer)
      ;; Create new buffer and store it
      (let ((new-buffer (jupyter-console-start-process kernel actual-file)))
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
      
      ;; Mark that we've used this buffer (but don't send dummy commands)
      (puthash buffer-key t jupyter-console-first-run)
      
      ;; Move to end of buffer
      (goto-char (point-max))
      
      ;; Mark where we are before sending
      (let ((output-start (point-marker)))
        
        ;; Send the code
        (comint-send-string proc (concat code "\n"))
        
        ;; Wait for output - R and Stata may need more time
        (let ((wait-time (if (string-match-p "\\(ir\\|stata\\)" buffer-key) 5 3)))
          (jupyter-console-wait-for-prompt buffer wait-time))
        
        ;; Get everything between our mark and the new prompt
        (goto-char (point-max))
        (let* ((output-end (point))
               (raw-output (buffer-substring-no-properties output-start output-end)))
          
          (jupyter-console-log 'debug "Raw output:\n%s" raw-output)
          
          ;; Extract the actual result
          (jupyter-console-extract-output raw-output code))))))

(defun jupyter-console-extract-output (raw-output code)
  "Extract clean output from RAW-OUTPUT, removing CODE echo and prompts."
  (jupyter-console-log 'debug "Extracting output from:\n%s" raw-output)
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
    
    (jupyter-console-log 'debug "After cleaning: '%s'" output)
    
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
        (jupyter-console-log 'warn "No output extracted from execution")
        nil))))

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
         (buffer (jupyter-console-get-or-create kernel file)))
    (jupyter-console-log 'debug "Sending region [%d-%d] to %s kernel: '%s'" beg end kernel trimmed-code)
    ;; Don't send empty code
    (when (and trimmed-code (not (string-empty-p trimmed-code)))
      (when show-buffer
        (jupyter-console-display-buffer buffer))
      (jupyter-console-send-string buffer trimmed-code))
    ;; Return result even if empty to avoid errors
    trimmed-code))

(defun jupyter-console-detect-kernel ()
  "Detect the appropriate kernel based on the current major mode."
  (cond
   ((derived-mode-p 'python-mode 'python-ts-mode) "python3")
   ((derived-mode-p 'ess-r-mode 'ess-mode) "ir")
   ((derived-mode-p 'stata-mode) "stata")
   ;; For org-src-mode, check the language
   ((and (bound-and-true-p org-src-mode)
         (boundp 'org-src--babel-type))
    (pcase org-src--babel-type
      ((or "python" "jupyter-python") "python3")
      ((or "R" "jupyter-R") "ir")
      ((or "stata" "jupyter-stata") "stata")
      (_ (error "Unknown language: %s" org-src--babel-type))))
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
    (add-hook 'org-src-mode-hook
              (lambda ()
                (add-hook 'kill-buffer-hook #'jupyter-console-org-src-restore-windows nil t))
              nil t)
    (let ((lang org-src--babel-type))
      (cond
       ((member lang '("python" "jupyter-python"))
        (local-set-key (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
        ;; Evil mode support in org-src buffers
        (with-eval-after-load 'evil
          (when (fboundp 'evil-local-set-key)
            (evil-local-set-key 'normal (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
            (evil-local-set-key 'insert (kbd "C-<return>") #'jupyter-console-python-eval-and-step)
            (evil-local-set-key 'visual (kbd "C-<return>") #'jupyter-console-python-eval-and-step))))
       ((member lang '("R" "jupyter-R"))
        (local-set-key (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
        ;; Evil mode support in org-src buffers
        (with-eval-after-load 'evil
          (when (fboundp 'evil-local-set-key)
            (evil-local-set-key 'normal (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
            (evil-local-set-key 'insert (kbd "C-<return>") #'jupyter-console-r-eval-and-step)
            (evil-local-set-key 'visual (kbd "C-<return>") #'jupyter-console-r-eval-and-step))))
       ((member lang '("stata" "jupyter-stata"))
        (local-set-key (kbd "C-<return>") #'jupyter-console-stata-eval-and-step)
        ;; Evil mode support in org-src buffers
        (with-eval-after-load 'evil
          (when (fboundp 'evil-local-set-key)
            (evil-local-set-key 'normal (kbd "C-<return>") #'jupyter-console-stata-eval-and-step)
            (evil-local-set-key 'insert (kbd "C-<return>") #'jupyter-console-stata-eval-and-step)
            (evil-local-set-key 'visual (kbd "C-<return>") #'jupyter-console-stata-eval-and-step))))))))

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
  
  ;; Org-src-mode hook
  (add-hook 'org-src-mode-hook #'jupyter-console-setup-org-src-keybindings))

;; Initialize log
(jupyter-console-clear-log)
(jupyter-console-log 'info "Jupyter console module loaded")
(jupyter-console-log 'info "Emacs version: %s" emacs-version)
(jupyter-console-log 'info "Running in batch mode: %s" (if noninteractive "yes" "no"))

;; Set up keybindings
(jupyter-console-setup-keybindings)
(jupyter-console-log 'info "Keybindings configured for C-RET in Python, R, and Stata modes")

(provide 'jupyter-console)
;;; jupyter-console.el ends here