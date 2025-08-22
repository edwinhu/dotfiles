;;; jupyter-termint.el --- Jupyter console integration using termint and vterm -*- lexical-binding: t; -*-

;;; Commentary:
;; Jupyter console integration using termint.el with vterm backend
;; This replaces the comint-based approach to enable proper bracketed paste
;; and multi-line single-cell execution

;;; Code:

(require 'termint)
(require 'org)
(require 'ob)
(require 'org-src)

;;; File-based logging for debugging
(defvar jupyter-termint-debug-log-file (expand-file-name "jupyter-termint-debug.log" "~/"))

(defun jupyter-termint-debug-log (level format-string &rest args)
  "Log with timestamp to file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) jupyter-termint-debug-log-file))))

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

;;; Image Detection Functions

(defun jupyter-termint-detect-graphics-code (code kernel)
  "Detect if CODE contains plotting commands for the given KERNEL."
  (let ((code-lower (downcase code)))
    (pcase kernel
      ("python3" (jupyter-termint--detect-python-graphics code-lower))
      ("ir" (jupyter-termint--detect-r-graphics code-lower))
      ("stata" (jupyter-termint--detect-stata-graphics code-lower))
      (_ nil))))

(defun jupyter-termint--detect-python-graphics (code)
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

(defun jupyter-termint--detect-r-graphics (code)
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

(defun jupyter-termint--detect-stata-graphics (code)
  "Detect graphics commands in Stata CODE."
  (or (string-match-p "\\bgraph\\b" code)
      (string-match-p "\\btwoway\\b" code)
      (string-match-p "\\bhistogram\\b" code)
      (string-match-p "\\bscatter\\b" code)
      (string-match-p "\\bbar\\b" code)))

;;; Image File Generation

(defvar jupyter-termint-image-counter 0
  "Counter for generating unique image filenames.")

(defun jupyter-termint-generate-image-filename (kernel)
  "Generate a unique image filename for KERNEL."
  (setq jupyter-termint-image-counter (1+ jupyter-termint-image-counter))
  (let ((filename (format "jupyter-%s-%d-%s.%s"
                         kernel
                         jupyter-termint-image-counter
                         (format-time-string "%Y%m%d-%H%M%S")
                         jupyter-termint-image-format)))
    (expand-file-name filename jupyter-termint-image-directory)))

;;; Code Injection for Image Generation

(defun jupyter-termint-inject-image-code (code kernel image-file)
  "Inject image-saving commands into CODE for KERNEL using IMAGE-FILE."
  (pcase kernel
    ("python3" (jupyter-termint--inject-python-image code image-file))
    ("ir" (jupyter-termint--inject-r-image code image-file))
    ("stata" (jupyter-termint--inject-stata-image code image-file))
    (_ code)))

(defun jupyter-termint--inject-python-image (code image-file)
  "Inject matplotlib save commands into Python CODE."
  (format "# Force matplotlib to use non-interactive backend
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
plt.ioff()
plt.close('all')

# User code
%s

# Save the current figure
fig_list = plt.get_fignums()
if fig_list:
    plt.savefig(r'%s', bbox_inches='tight', dpi=%d, facecolor='white', edgecolor='none')
    print('Plot saved to: %s')
else:
    print('No figure to save')" 
          code image-file jupyter-termint-image-dpi image-file))

(defun jupyter-termint--inject-r-image (code image-file)
  "Inject R save commands into R CODE."
  (if (string-match-p "\\bggplot\\b" (downcase code))
      ;; ggplot2 code
      (format "%s
ggsave('%s', width=8, height=6, dpi=%d, bg='white')"
              code image-file jupyter-termint-image-dpi)
    ;; Base graphics
    (format "%s
dev.copy(png, filename='%s', width=8*%d, height=6*%d, res=%d, bg='white')
dev.off()"
            code image-file jupyter-termint-image-dpi jupyter-termint-image-dpi jupyter-termint-image-dpi)))

(defun jupyter-termint--inject-stata-image (code image-file)
  "Inject Stata save commands into Stata CODE."
  (format "%s
graph export \"%s\", replace"
          code image-file))

;;; Termint Jupyter Definitions

(defun jupyter-termint--check-direnv-allowed (directory)
  "Check if direnv is already allowed for DIRECTORY.
Returns t if allowed, nil otherwise."
  (jupyter-termint-debug-log 'info "Checking direnv status for directory: %s" directory)
  (when (and (boundp 'envrc-direnv-executable) envrc-direnv-executable)
    (let* ((default-directory directory)
           (status-output (with-temp-buffer
                           (when (zerop (call-process envrc-direnv-executable nil t nil "status"))
                             (buffer-string)))))
      (jupyter-termint-debug-log 'debug "Direnv status output: %s" (or status-output "nil"))
      (let ((allowed (and status-output
                          (string-match-p "Found RC allowPath" status-output)
                          t))) ; Force boolean return value
        (jupyter-termint-debug-log 'info "Direnv allowed status for %s: %s" directory allowed)
        allowed))))

(defun jupyter-termint--build-smart-command (project-dir base-command)
  "Build a smart command that handles direnv properly for PROJECT-DIR.
If direnv is already allowed, use direnv exec to avoid permission prompts.
Otherwise, use normal cd command (which may prompt)."
  (let ((command (if (jupyter-termint--check-direnv-allowed project-dir)
                     ;; Direnv already allowed - use direnv exec to avoid prompts
                     (format "sh -c 'cd %s && direnv exec . %s'" project-dir base-command)
                   ;; Not allowed yet - use normal cd (may prompt user)
                   (format "sh -c 'cd %s && %s'" project-dir base-command))))
    (jupyter-termint-debug-log 'info "Built command for %s: %s" project-dir command)
    command))

(defun jupyter-termint-setup ()
  "Set up termint definitions for Jupyter kernels."
  (message "jupyter-termint: Setting up termint definitions")
  (jupyter-termint-debug-log 'info "Starting jupyter-termint setup")
  
  ;; Configure termint backend
  (setq termint-backend 'vterm)
  (jupyter-termint-debug-log 'info "Set termint backend to vterm")
  
  ;; Find the best jupyter executable path
  (let ((jupyter-path (or (executable-find "jupyter")
                          (when (file-executable-p ".pixi/envs/default/bin/jupyter")
                            ".pixi/envs/default/bin/jupyter")
                          (when (file-executable-p "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter")
                            "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter"))))
    
    (jupyter-termint-debug-log 'info "Found jupyter path: %s" (or jupyter-path "nil"))
    
    (if jupyter-path
        (progn
          (message "jupyter-termint: Using jupyter at: %s" jupyter-path)
          (jupyter-termint-debug-log 'info "Using jupyter at: %s" jupyter-path)
          
          ;; Define termint sessions with smart direnv handling
          (let ((pixi-project-dir "/Users/vwh7mb/projects/wander2"))
            ;; Check direnv status once for logging
            (if (jupyter-termint--check-direnv-allowed pixi-project-dir)
                (progn
                  (message "jupyter-termint: Direnv already allowed for %s - using direnv exec" pixi-project-dir)
                  (jupyter-termint-debug-log 'info "Direnv already allowed - will use direnv exec"))
              (progn
                (message "jupyter-termint: Direnv not yet allowed for %s - may prompt on first run" pixi-project-dir)
                (jupyter-termint-debug-log 'warn "Direnv not yet allowed - may prompt user")))
            
            ;; Build smart commands with direnv handling
            (let ((python-cmd (jupyter-termint--build-smart-command 
                              pixi-project-dir 
                              "pixi run jupyter console --kernel python3"))
                  (r-cmd (jupyter-termint--build-smart-command 
                         pixi-project-dir 
                         "pixi run jupyter console --kernel ir"))
                  (stata-cmd (format "%s console --kernel stata" jupyter-path)))
              
              ;; Define Python Jupyter console with smart direnv handling
              (termint-define "jupyter-python" python-cmd
                              :bracketed-paste-p t)
              (jupyter-termint-debug-log 'info "Defined Python console with command: %s" python-cmd)
              
              ;; Define R Jupyter console with smart direnv handling
              (termint-define "jupyter-r" r-cmd
                              :bracketed-paste-p t)
              (jupyter-termint-debug-log 'info "Defined R console with command: %s" r-cmd)
              
              ;; Define Stata Jupyter console
              (termint-define "jupyter-stata" stata-cmd
                              :bracketed-paste-p t)
              (jupyter-termint-debug-log 'info "Defined Stata console with command: %s" stata-cmd))))
      
      (progn
        (message "jupyter-termint: ERROR - No jupyter executable found. Check your PATH or pixi environment.")
        (jupyter-termint-debug-log 'error "No jupyter executable found"))))
  
  (message "jupyter-termint: Termint definitions completed")
  (jupyter-termint-debug-log 'info "Jupyter-termint setup completed"))

;;; Org-babel Integration

;; Output capture functions for vterm buffers
(defun jupyter-termint-wait-for-prompt (buffer &optional timeout)
  "Wait for a jupyter prompt to appear in vterm BUFFER.
TIMEOUT defaults to 5 seconds for vterm as it may be slower."
  (let ((timeout (or timeout 5))
        (start-time (current-time))
        (prompt-regexp "In \\[[0-9]+\\]: \\|>>> \\|\\.\\.\\. \\|> \\|\\.\\.\\.\\.")
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
                    (if output-received
                        ;; Got output, wait a bit for prompt or more output
                        (not (looking-back prompt-regexp 
                                          (max (point-min) (- (point) 200))))
                      ;; No output yet, keep waiting
                      t)))
        (sit-for 0.1))
      ;; Return true if we found a prompt or got output
      (or (looking-back prompt-regexp (max (point-min) (- (point) 200)))
          output-received))))

(defun jupyter-termint-extract-output (raw-output code)
  "Extract clean output from RAW-OUTPUT, removing CODE echo and prompts."
  (let ((output raw-output))
    ;; Remove ANSI escape sequences first
    (setq output (replace-regexp-in-string "\033\\[[0-9;]*m" "" output))
    
    ;; Split into lines for better processing
    (let ((lines (split-string output "\n"))
          (code-lines (split-string code "\n"))
          (result-lines '())
          (in-output nil)
          (seen-current-code nil))
      
      ;; Process each line
      (dolist (line lines)
        (let ((trimmed-line (string-trim line)))
          (cond
           ;; Skip startup messages and info
           ((string-match-p "Type 'copyright'\\|IPython\\|enhanced Interactive\\|Tip: Use\\|\\] *$" line)
            nil)
           
           ;; Skip prompts and continuation lines
           ((string-match-p "^In \\[[0-9]+\\]: *$\\|^>>> *$\\|^\\.\\.\\. *$\\|^   \\.\\.\\.: *$" line)
            nil)
           
           ;; Check if this line matches our code (start of our execution)
           ((and (not seen-current-code)
                 (cl-some (lambda (code-line)
                           (and (not (string-empty-p (string-trim code-line)))
                                (string-match-p (regexp-quote (string-trim code-line)) trimmed-line)))
                         code-lines))
            (setq seen-current-code t)
            (setq in-output t))
           
           ;; If we've seen our code, start collecting output
           ((and seen-current-code in-output (not (string-empty-p trimmed-line)))
            ;; Skip lines that are just our code echoed back
            (unless (cl-some (lambda (code-line)
                              (and (not (string-empty-p (string-trim code-line)))
                                   (string= (string-trim code-line) trimmed-line)))
                            code-lines)
              (push trimmed-line result-lines))))))
      
      ;; Join the result lines
      (let ((clean-output (string-join (reverse result-lines) "\n")))
        (setq clean-output (string-trim clean-output))
        
        ;; If we have any output after cleaning, return it
        (if (and clean-output (not (string-empty-p clean-output)))
            clean-output
          nil)))))

(defun jupyter-termint-send-string-with-output (buffer code)
  "Send CODE to vterm BUFFER and capture the output."
  (jupyter-termint-debug-log 'info "Sending code to %s for output capture" buffer)
  
  (with-current-buffer buffer
    ;; Make sure we're at the end
    (goto-char (point-max))
    
    ;; Mark where we are before sending
    (let ((output-start (point-marker)))
      
      ;; Send the code using termint's send function
      (let ((send-func (cond
                       ((string-match-p "python" buffer) 'termint-jupyter-python-send-string)
                       ((string-match-p "r" buffer) 'termint-jupyter-r-send-string)
                       ((string-match-p "stata" buffer) 'termint-jupyter-stata-send-string)
                       (t (error "Unknown buffer type: %s" buffer)))))
        (when (fboundp send-func)
          (funcall send-func code)))
      
      ;; Wait for output with longer timeout for vterm
      (jupyter-termint-wait-for-prompt buffer 8)
      
      ;; Get everything between our mark and the current position
      (goto-char (point-max))
      (let* ((output-end (point))
             (raw-output (buffer-substring-no-properties output-start output-end)))
        
        (jupyter-termint-debug-log 'debug "Raw output captured: %s" raw-output)
        
        ;; Extract the actual result
        (let ((clean-output (jupyter-termint-extract-output raw-output code)))
          (jupyter-termint-debug-log 'debug "Clean output: %s" clean-output)
          clean-output)))))

(defun jupyter-termint-execute-python (body params)
  "Execute Python BODY with PARAMS using termint jupyter."
  (message "jupyter-termint: Executing Python code block")
  (let* ((has-graphics (jupyter-termint-detect-graphics-code body "python3"))
         (results-assoc (assq :results params))
         (results-type (when results-assoc (cdr results-assoc)))
         (image-file (when (and has-graphics (string-match-p "file" (or results-type "")))
                       (jupyter-termint-generate-image-filename "python3")))
         (final-code (if image-file
                        (jupyter-termint-inject-image-code body "python3" image-file)
                      body)))
    
    (when has-graphics
      (message "jupyter-termint: Generating plot%s..." 
               (if image-file (format " to %s" image-file) "")))
    
    ;; Execute code and capture output
    (if image-file
        ;; Graphics mode - send to console and return image file
        (progn
          (termint-jupyter-python-send-string final-code)
          (when has-graphics
            (message "jupyter-termint: Python execution completed%s"
                     (if image-file " - plot saved" "")))
          ;; Return image file path for :results file
          (if (file-exists-p image-file)
              image-file
            nil))
      ;; Text mode - send code and capture output
      (progn
        (message "jupyter-termint: Executing Python code for text output...")
        ;; Ensure console is running first
        (jupyter-termint-ensure-console-with-features "python" "")
        ;; Send code to console and capture output
        (let ((result (jupyter-termint-send-string-with-output "*jupyter-python*" final-code)))
          (message "jupyter-termint: Python execution completed with result")
          (message "jupyter-termint: Returning result: '%s'" result)
          (message "jupyter-termint: Result type: %s, length: %d" (type-of result) (length result))
          result)))))

(defun jupyter-termint-execute-r (body params)
  "Execute R BODY with PARAMS using termint jupyter."
  (message "jupyter-termint: Executing R code block")
  (message "jupyter-termint: R params = %S" params)
  (let* ((has-graphics (jupyter-termint-detect-graphics-code body "ir"))
         (results-assoc (assq :results params))
         (results-type (when results-assoc (cdr results-assoc)))
         (image-file (when (and has-graphics (string-match-p "file" (or results-type "")))
                       (jupyter-termint-generate-image-filename "r")))
         (final-code (if image-file
                        (jupyter-termint-inject-image-code body "ir" image-file)
                      body)))
    
    (when has-graphics
      (message "jupyter-termint: Generating plot%s..." 
               (if image-file (format " to %s" image-file) "")))
    
    ;; Execute code and capture output
    (if image-file
        ;; Graphics mode - send to console and return image file
        (progn
          (termint-jupyter-r-send-string final-code)
          (when has-graphics
            (message "jupyter-termint: R execution completed%s"
                     (if image-file " - plot saved" "")))
          ;; Return image file path for :results file
          (if (file-exists-p image-file)
              image-file
            nil))
      ;; Text mode - send code and capture output
      (progn
        (message "jupyter-termint: Executing R code for text output...")
        ;; Ensure console is running first
        (jupyter-termint-ensure-console-with-features "r" "")
        ;; Send code to console and capture output
        (let ((result (jupyter-termint-send-string-with-output "*jupyter-r*" final-code)))
          (message "jupyter-termint: R execution completed with result")
          (message "jupyter-termint: R Returning result: '%s'" result)
          (message "jupyter-termint: R Result type: %s, length: %d" (type-of result) (length result))
          result)))))

(defun jupyter-termint-execute-stata (body params)
  "Execute Stata BODY with PARAMS using termint jupyter."
  (message "jupyter-termint: Executing Stata code block")
  (let* ((has-graphics (jupyter-termint-detect-graphics-code body "stata"))
         (results-assoc (assq :results params))
         (results-type (when results-assoc (cdr results-assoc)))
         (image-file (when (and has-graphics (string-match-p "file" (or results-type "")))
                       (jupyter-termint-generate-image-filename "stata")))
         (final-code (if image-file
                        (jupyter-termint-inject-image-code body "stata" image-file)
                      body)))
    
    (when has-graphics
      (message "jupyter-termint: Generating plot%s..." 
               (if image-file (format " to %s" image-file) "")))
    
    ;; Execute code and capture output
    (if image-file
        ;; Graphics mode - send to console and return image file
        (progn
          (termint-jupyter-stata-send-string final-code)
          (when has-graphics
            (message "jupyter-termint: Stata execution completed%s"
                     (if image-file " - plot saved" "")))
          ;; Return image file path for :results file
          (if (file-exists-p image-file)
              image-file
            nil))
      ;; Text mode - send code and capture output
      (progn
        (message "jupyter-termint: Executing Stata code for text output...")
        ;; Ensure console is running first
        (jupyter-termint-ensure-console-with-features "stata" "")
        ;; Send code to console and capture output
        (let ((result (jupyter-termint-send-string-with-output "*jupyter-stata*" final-code)))
          (message "jupyter-termint: Stata execution completed with result")
          result)))))

;;; Override org-babel functions

(defun jupyter-termint-setup-babel-integration ()
  "Setup jupyter-termint org-babel integration."
  (message "jupyter-termint: Setting up babel integration")
  
  ;; Override org-babel functions to use termint
  (defun org-babel-execute:python (body params)
    "Execute Python BODY with PARAMS using jupyter-termint."
    (jupyter-termint-execute-python body params))
  
  (defun org-babel-execute:R (body params)
    "Execute R BODY with PARAMS using jupyter-termint."
    (jupyter-termint-execute-r body params))
  
  (defun org-babel-execute:stata (body params)
    "Execute Stata BODY with PARAMS using jupyter-termint."
    (jupyter-termint-execute-stata body params))
  
  (message "jupyter-termint: Babel integration setup complete"))

;;; Allow risky local variables to be remembered permanently
;; This fixes direnv permission prompts by allowing Emacs to remember
;; risky local variable permissions instead of asking every time
(advice-add 'risky-local-variable-p :override #'ignore)

;;; C-RET org-src Integration

(defun jupyter-termint-clear-log ()
  "Clear the debug log file."
  (with-temp-buffer
    (write-file jupyter-termint-debug-log-file)))

;;; Language detection
(defun jupyter-termint-detect-kernel ()
  "Detect kernel from org-src buffer language."
  (let ((lang-from-buffer-name (when (string-match "\\*Org Src.*\\[ \\([^]]+\\) \\]\\*" (buffer-name))
                                 (match-string 1 (buffer-name))))
        (lang-from-variable (bound-and-true-p org-src--lang)))
    (jupyter-termint-debug-log 'debug "Buffer name: %s" (buffer-name))
    (jupyter-termint-debug-log 'debug "Language from buffer name: %s" lang-from-buffer-name)
    (jupyter-termint-debug-log 'debug "Language from org-src--lang: %s" lang-from-variable)
    (jupyter-termint-debug-log 'debug "Major mode: %s" major-mode)
    
    (let ((detected-lang (or lang-from-variable lang-from-buffer-name)))
      (cond
       ((or (equal detected-lang "python") (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode)) "python")
       ((or (equal detected-lang "r") (equal detected-lang "R") (eq major-mode 'ess-r-mode)) "r") 
       ((or (equal detected-lang "stata") (eq major-mode 'stata-mode)) "stata")
       (t (progn 
            (jupyter-termint-debug-log 'info "Using fallback kernel: python")
            "python"))))))

;;; Console management with all features
(defun jupyter-termint-smart-python-start ()
  "Start Python jupyter console with smart direnv command."
  (interactive)
  (jupyter-termint-debug-log 'info "Starting smart Python console with direnv")
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-python*")
    (jupyter-termint-debug-log 'info "Killing existing hung *jupyter-python* buffer")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-python*")))
  
  ;; Define and start with smart direnv command
  (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel python3'"))
    (jupyter-termint-debug-log 'info "Defining termint with smart command: %s" smart-cmd)
    (termint-define "jupyter-python" smart-cmd :bracketed-paste-p t)
    (jupyter-termint-debug-log 'info "Calling termint-jupyter-python-start...")
    (termint-jupyter-python-start)
    (jupyter-termint-debug-log 'info "Smart Python console started")))

(defun jupyter-termint-smart-r-start ()
  "Start R jupyter console with smart direnv command."
  (interactive)
  (jupyter-termint-debug-log 'info "Starting smart R console with direnv")
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-r*")
    (jupyter-termint-debug-log 'info "Killing existing hung *jupyter-r* buffer")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-r*")))
  
  ;; Define and start with smart direnv command
  (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel ir'"))
    (jupyter-termint-debug-log 'info "Defining termint with smart command: %s" smart-cmd)
    (termint-define "jupyter-r" smart-cmd :bracketed-paste-p t)
    (jupyter-termint-debug-log 'info "Calling termint-jupyter-r-start...")
    (termint-jupyter-r-start)
    (jupyter-termint-debug-log 'info "Smart R console started")))

(defun jupyter-termint-smart-stata-start ()
  "Start Stata jupyter console with smart direnv command."
  (interactive)
  (jupyter-termint-debug-log 'info "Starting smart Stata console with direnv")
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-stata*")
    (jupyter-termint-debug-log 'info "Killing existing hung *jupyter-stata* buffer")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-stata*")))
  
  ;; Define and start with smart direnv command
  (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel stata'"))
    (jupyter-termint-debug-log 'info "Defining termint with smart command: %s" smart-cmd)
    (termint-define "jupyter-stata" smart-cmd :bracketed-paste-p t)
    (jupyter-termint-debug-log 'info "Calling termint-jupyter-stata-start...")
    (termint-jupyter-stata-start)
    (jupyter-termint-debug-log 'info "Smart Stata console started")))

(defun jupyter-termint-display-console-right (buffer &optional original-buffer original-window)
  "Display console BUFFER in a right split window, preserving focus on ORIGINAL-BUFFER in ORIGINAL-WINDOW."
  (jupyter-termint-debug-log 'info "Displaying console in right split, preserving focus")
  
  (let ((initial-window (or original-window (selected-window)))
        (initial-buffer (or original-buffer (current-buffer))))
    
    (jupyter-termint-debug-log 'info "Initial buffer: %s" (buffer-name initial-buffer))
    
    ;; Use display-buffer with specific parameters for right split
    (let ((console-window (display-buffer buffer
                                          '((display-buffer-reuse-window
                                             display-buffer-in-side-window)
                                            (side . right)
                                            (window-width . 0.5)
                                            (inhibit-same-window . t)))))
      
      (jupyter-termint-debug-log 'info "Console window created: %s" console-window)
      
      (when console-window
        ;; Briefly switch to console window to scroll to bottom
        (with-selected-window console-window
          (goto-char (point-max)))
        
        ;; Force restore focus to original window and buffer
        (when (window-live-p initial-window)
          (jupyter-termint-debug-log 'info "Restoring focus to initial window")
          (select-window initial-window))
        
        (when (buffer-live-p initial-buffer)
          (jupyter-termint-debug-log 'info "Restoring buffer to: %s" (buffer-name initial-buffer))
          (set-window-buffer (selected-window) initial-buffer))
        
        (jupyter-termint-debug-log 'info "Focus restored to org-src buffer")))))

(defun jupyter-termint-ensure-console-with-features (kernel code)
  "Ensure console for KERNEL is running with direnv and window management, then send CODE."
  (let* ((kernel-config (cond
                        ((string= kernel "python")
                         '("*jupyter-python*" jupyter-termint-smart-python-start termint-jupyter-python-send-string))
                        ((string= kernel "r")
                         '("*jupyter-r*" jupyter-termint-smart-r-start termint-jupyter-r-send-string))
                        ((string= kernel "stata")
                         '("*jupyter-stata*" jupyter-termint-smart-stata-start termint-jupyter-stata-send-string))
                        (t (error "Unsupported kernel: %s" kernel))))
         (buffer-name (nth 0 kernel-config))
         (start-func (nth 1 kernel-config))
         (send-func (nth 2 kernel-config))
         ;; CRITICAL: Capture org-src buffer/window BEFORE any console operations
         (original-buffer (current-buffer))
         (original-window (selected-window)))
    
    (jupyter-termint-debug-log 'info "Captured original state - Buffer: %s, Window: %s" 
                              (buffer-name original-buffer) original-window)
    (jupyter-termint-debug-log 'info "Checking for console buffer: %s" buffer-name)
    
    ;; Check if console buffer exists and has a live process
    (let ((console-buffer (get-buffer buffer-name)))
      (if (and console-buffer 
               (buffer-live-p console-buffer)
               (get-buffer-process console-buffer)
               (process-live-p (get-buffer-process console-buffer)))
          (progn
            (jupyter-termint-debug-log 'info "Console already running: %s" buffer-name)
            ;; Console exists, just display it and send code
            (jupyter-termint-display-console-right console-buffer original-buffer original-window)
            (funcall send-func code))
        
        ;; Console doesn't exist or is dead, start it
        (progn
          (jupyter-termint-debug-log 'info "Starting new console for %s kernel" kernel)
          (message "Starting %s console with direnv..." kernel)
          
          ;; Call the smart start function
          (funcall start-func)
          
          ;; Wait for buffer to be created
          (let ((max-wait 10) (wait-count 0))
            (while (and (< wait-count max-wait)
                       (not (get-buffer buffer-name)))
              (sleep-for 0.5)
              (setq wait-count (1+ wait-count)))
            
            (let ((new-buffer (get-buffer buffer-name)))
              (if new-buffer
                  (progn
                    (jupyter-termint-debug-log 'info "Console buffer created: %s" buffer-name)
                    (message "%s console ready!" kernel)
                    ;; Display in right split
                    (jupyter-termint-display-console-right new-buffer original-buffer original-window)
                    ;; Give it a moment to fully initialize, then send code
                    (sleep-for 1)
                    (funcall send-func code))
                (progn
                  (jupyter-termint-debug-log 'error "Console buffer not created after %d seconds" max-wait)
                  (error "Failed to create %s console buffer" kernel))))))))))

;;; Main function
(defun jupyter-termint-send-simple ()
  "Simple function to send region/line to termint console."
  (interactive)
  (jupyter-termint-debug-log 'info "Starting simple send function")
  
  (let* ((kernel (jupyter-termint-detect-kernel))
         (code (cond
                ((use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end)))
                ((> (point-max) (point-min))
                 (buffer-substring-no-properties (point-min) (point-max)))
                (t
                 (thing-at-point 'line t)))))
    
    (jupyter-termint-debug-log 'info "Kernel: %s" kernel)
    (jupyter-termint-debug-log 'info "Code: %s" code)
    
    (cond
     ((string= kernel "python")
      (jupyter-termint-debug-log 'info "Sending to Python console with direnv + window management")
      (jupyter-termint-ensure-console-with-features "python" code))
     ((string= kernel "r")
      (jupyter-termint-debug-log 'info "Sending to R console with direnv + window management")
      (jupyter-termint-ensure-console-with-features "r" code))
     ((string= kernel "stata")
      (jupyter-termint-debug-log 'info "Sending to Stata console with direnv + window management")
      (jupyter-termint-ensure-console-with-features "stata" code))
     (t 
      (jupyter-termint-debug-log 'error "Unsupported kernel: %s" kernel)
      (message "Unsupported kernel: %s" kernel)))))

;;; Keybinding setup
(defun jupyter-termint-setup-keybinding ()
  "Setup C-RET keybinding by unbinding Doom keys first."
  (jupyter-termint-debug-log 'info "Setting up keybinding by unbinding Doom defaults")
  
  ;; Unbind the Doom default C-RET keybinding globally
  (map! "C-<return>" nil)
  
  ;; Common function to set up keybindings in org-src buffers
  (defun jupyter-termint-setup-buffer-keybinding ()
    "Set up C-RET keybinding for the current org-src buffer."
    (when (string-match "\\*Org Src.*\\[ \\([^]]+\\) \\]\\*" (buffer-name))
      (jupyter-termint-debug-log 'info "Setting up C-<return> in org-src buffer: %s" (buffer-name))
      
      ;; Unbind in all evil states for this buffer
      (evil-local-set-key 'insert (kbd "C-<return>") nil)
      (evil-local-set-key 'normal (kbd "C-<return>") nil)
      (evil-local-set-key 'visual (kbd "C-<return>") nil)
      
      ;; Now bind our function
      (evil-local-set-key 'insert (kbd "C-<return>") #'jupyter-termint-send-simple)
      (evil-local-set-key 'normal (kbd "C-<return>") #'jupyter-termint-send-simple)
      (evil-local-set-key 'visual (kbd "C-<return>") #'jupyter-termint-send-simple)
      (local-set-key (kbd "C-<return>") #'jupyter-termint-send-simple)
      
      (jupyter-termint-debug-log 'info "Unbound Doom C-<return> and bound jupyter-termint-send-simple")))

  ;; Set up hooks for all supported languages
  (add-hook 'python-mode-hook #'jupyter-termint-setup-buffer-keybinding)
  (add-hook 'ess-r-mode-hook #'jupyter-termint-setup-buffer-keybinding)
  (add-hook 'stata-mode-hook #'jupyter-termint-setup-buffer-keybinding)
  (add-hook 'ess-stata-mode-hook #'jupyter-termint-setup-buffer-keybinding)
  
  (jupyter-termint-debug-log 'info "Added keybinding hooks with Doom unbinding"))

;;; Initialization

(with-eval-after-load 'termint
  (jupyter-termint-clear-log)
  (jupyter-termint-debug-log 'info "jupyter-termint.el loaded")
  (jupyter-termint-setup)
  (jupyter-termint-setup-babel-integration)
  (jupyter-termint-setup-keybinding))

(provide 'jupyter-termint)
;;; jupyter-termint.el ends here