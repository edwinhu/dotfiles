;;; jupyter-termint.el --- Jupyter console integration using termint and vterm -*- lexical-binding: t; -*-

;;; Commentary:
;; Jupyter console integration using termint.el with vterm backend
;; This replaces the comint-based approach to enable proper bracketed paste
;; and multi-line single-cell execution

;;; Code:

(require 'termint)
(require 'org)
(require 'ob)

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

(defun jupyter-termint-execute-python (body params)
  "Execute Python BODY with PARAMS using termint jupyter."
  (message "jupyter-termint: Executing Python code block")
  (let* ((has-graphics (jupyter-termint-detect-graphics-code body "python3"))
         (results-type (cdr (assq :results params)))
         (image-file (when (and has-graphics (string-match-p "file" (or results-type "")))
                       (jupyter-termint-generate-image-filename "python3")))
         (final-code (if image-file
                        (jupyter-termint-inject-image-code body "python3" image-file)
                      body)))
    
    (when has-graphics
      (message "jupyter-termint: Generating plot%s..." 
               (if image-file (format " to %s" image-file) "")))
    
    ;; Send code to termint jupyter-python
    (termint-jupyter-python-send-string final-code)
    
    (when has-graphics
      (message "jupyter-termint: Python execution completed%s"
               (if image-file " - plot saved" "")))
    
    ;; Return image file path for :results file
    (if (and image-file (file-exists-p image-file))
        image-file
      nil)))

(defun jupyter-termint-execute-r (body params)
  "Execute R BODY with PARAMS using termint jupyter."
  (message "jupyter-termint: Executing R code block")
  (let* ((has-graphics (jupyter-termint-detect-graphics-code body "ir"))
         (results-type (cdr (assq :results params)))
         (image-file (when (and has-graphics (string-match-p "file" (or results-type "")))
                       (jupyter-termint-generate-image-filename "r")))
         (final-code (if image-file
                        (jupyter-termint-inject-image-code body "ir" image-file)
                      body)))
    
    (when has-graphics
      (message "jupyter-termint: Generating plot%s..." 
               (if image-file (format " to %s" image-file) "")))
    
    ;; Send code to termint jupyter-r
    (termint-jupyter-r-send-string final-code)
    
    (when has-graphics
      (message "jupyter-termint: R execution completed%s"
               (if image-file " - plot saved" "")))
    
    ;; Return image file path for :results file
    (if (and image-file (file-exists-p image-file))
        image-file
      nil)))

(defun jupyter-termint-execute-stata (body params)
  "Execute Stata BODY with PARAMS using termint jupyter."
  (message "jupyter-termint: Executing Stata code block")
  (let* ((has-graphics (jupyter-termint-detect-graphics-code body "stata"))
         (results-type (cdr (assq :results params)))
         (image-file (when (and has-graphics (string-match-p "file" (or results-type "")))
                       (jupyter-termint-generate-image-filename "stata")))
         (final-code (if image-file
                        (jupyter-termint-inject-image-code body "stata" image-file)
                      body)))
    
    (when has-graphics
      (message "jupyter-termint: Generating plot%s..." 
               (if image-file (format " to %s" image-file) "")))
    
    ;; Send code to termint jupyter-stata
    (termint-jupyter-stata-send-string final-code)
    
    (when has-graphics
      (message "jupyter-termint: Stata execution completed%s"
               (if image-file " - plot saved" "")))
    
    ;; Return image file path for :results file
    (if (and image-file (file-exists-p image-file))
        image-file
      nil)))

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

;;; Initialization

(with-eval-after-load 'termint
  (jupyter-termint-setup)
  (jupyter-termint-setup-babel-integration))

(provide 'jupyter-termint)
;;; jupyter-termint.el ends here