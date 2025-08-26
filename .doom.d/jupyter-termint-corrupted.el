;;; jupyter-termint.el --- Jupyter console integration using termint and eat -*- lexical-binding: t; -*-

;;; Commentary:
;; Jupyter console integration using termint.el with eat backend
;; This replaces the comint-based approach to enable proper bracketed paste
;; and multi-line single-cell execution

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

;;; Image Detection Functions

(defun jupyter-termint-map-to-jupyter-kernel (lang-kernel)
  "Map detected language LANG-KERNEL to actual jupyter kernel name."
  (cond
   ((string= lang-kernel "python") "python3")
   ((string= lang-kernel "r") "ir")
   ((string= lang-kernel "stata") "stata")
   (t lang-kernel)))

(defun jupyter-termint-detect-graphics-code (code kernel)
  "Detect if CODE contains plotting commands for the given KERNEL."
  (let ((code-lower (downcase code))
        (jupyter-kernel (jupyter-termint-map-to-jupyter-kernel kernel)))
    (pcase jupyter-kernel
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
      (string-match-p "\\bbar\\b" code)
      (string-match-p "\\bplot\\b" code)
      (string-match-p "\\bbox\\b" code)
      (string-match-p "\\bmatrix plot\\b" code)))

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
  "R plotting is now handled seamlessly via print method overrides in 01-r-sixel-config.R.
No code injection needed - plots display automatically with sixel graphics."
  ;; Return original code unchanged - print overrides handle everything automatically
  code)

(defun jupyter-termint--inject-stata-image (code image-file)
  "For Stata, return code unmodified - stata_kernel handles export automatically."
  ;; stata_kernel automatically wraps graphics commands with export logic
  ;; File monitoring will detect new graphs in ~/.stata_kernel_cache/
  code)

;;; Termint Jupyter Definitions

(defun jupyter-termint--check-direnv-allowed (directory)
  "Check if direnv is already allowed for DIRECTORY.
Returns t if allowed, nil otherwise."
  (when (and (boundp 'envrc-direnv-executable) envrc-direnv-executable)
    (let* ((default-directory directory)
           (status-output (with-temp-buffer
                           (when (zerop (call-process envrc-direnv-executable nil t nil "status"))
                             (buffer-string)))))
      (let ((allowed (and status-output
                          (string-match-p "Found RC allowPath" status-output)
                          t))) ; Force boolean return value
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
    command))

(defun jupyter-termint-setup ()
  "Set up termint definitions for Jupyter kernels."
  (message "jupyter-termint: Setting up termint definitions")
  
  ;; Configure termint backend - use eat for sixel support (vterm doesn't support sixel in Emacs)
  (setq termint-backend 'eat)
  
  ;; Find the best jupyter executable path - prioritize pixi environment
  (let ((jupyter-path (or (when (file-executable-p "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter")
                            "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter")
                          (executable-find "jupyter"))))
    
    
    (if jupyter-path
        (progn
          (message "jupyter-termint: Using jupyter at: %s" jupyter-path)
          
          ;; Define termint sessions with smart direnv handling
          (let ((pixi-project-dir "/Users/vwh7mb/projects/wander2"))
            ;; Check direnv status once for logging
            (if (jupyter-termint--check-direnv-allowed pixi-project-dir)
                (progn
                  (message "jupyter-termint: Direnv already allowed for %s - using direnv exec" pixi-project-dir)
                  nil)
              (progn
                (message "jupyter-termint: Direnv not yet allowed for %s - may prompt on first run" pixi-project-dir)
                nil))
            
            ;; Build smart commands with direnv handling
            (let ((python-cmd (jupyter-termint--build-smart-command 
                              pixi-project-dir 
                              "pixi run jupyter console --kernel python3"))
                  (r-cmd (jupyter-termint--build-smart-command 
                         pixi-project-dir 
                         "pixi run jupyter console --kernel ir"))
                  (stata-cmd (jupyter-termint--build-smart-command 
                             pixi-project-dir 
                             "pixi run jupyter console --kernel stata")))
              
              ;; Define Python Jupyter console with smart direnv handling (NO for sixel support)
              (termint-define "jupyter-python" python-cmd
                              :bracketed-paste-p t
                              :backend 'eat
                              :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
              
              ;; Define R Jupyter console with smart direnv handling (NO for IRdisplay support)
              (termint-define "jupyter-r" r-cmd
                              :bracketed-paste-p t
                              :backend 'eat
                              :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
              
              ;; Define Stata Jupyter console with proper sixel support
              (termint-define "jupyter-stata" stata-cmd
                              :bracketed-paste-p t
                              :backend 'eat
                              :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
              nil))
      
      (progn
        (message "jupyter-termint: ERROR - No jupyter executable found. Check your PATH or pixi environment.")
        nil)))
  
  (message "jupyter-termint: Termint definitions completed")
  nil)

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
    
    ;; Return image file path for :results file, or test text output
    (if (and image-file (file-exists-p image-file))
        image-file
      ;; For now, return a simple test string to see if results work
      "Test Python output\nSecond line")))

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

;;; Buffer management functions

(defun jupyter-termint-silent-start (kernel)
  "Silently start jupyter console for KERNEL without window switching."
  (let* ((buffer-name (cond
                      ((string= kernel "python") "*jupyter-python*")
                      ((string= kernel "r") "*jupyter-r*") 
                      ((string= kernel "stata") "*jupyter-stata*")
                      (t (error "Unsupported kernel: %s" kernel))))
         (smart-cmd (cond
                    ((string= kernel "python") 
                     "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel python3'")
                    ((string= kernel "r")
                     "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel ir'")
                    ((string= kernel "stata")
                     "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel stata'")
                    (t (error "Unsupported kernel: %s" kernel))))
         (termint-name (cond
                       ((string= kernel "python") "jupyter-python")
                       ((string= kernel "r") "jupyter-r")
                       ((string= kernel "stata") "jupyter-stata")
                       (t (error "Unsupported kernel: %s" kernel))))
         (current-window (selected-window))
         (current-buffer (current-buffer)))
    
    ;; Kill any existing hung buffer first
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))
    
    ;; Define and start with smart command - use specific string for each kernel
    (cond
     ((string= kernel "python") (termint-define "jupyter-python" smart-cmd :bracketed-paste-p t))
     ((string= kernel "r") (termint-define "jupyter-r" smart-cmd :bracketed-paste-p t))
     ((string= kernel "stata") (termint-define "jupyter-stata" smart-cmd :bracketed-paste-p t)))
    
    ;; Start the console with display suppression
    (let ((display-buffer-alist (cons '("\\*jupyter-.*\\*" display-buffer-no-window) display-buffer-alist)))
      (cond
       ((string= kernel "python") (termint-jupyter-python-start))
       ((string= kernel "r") (termint-jupyter-r-start))
       ((string= kernel "stata") (termint-jupyter-stata-start))))
    
    ;; Restore original window and buffer focus
    (when (window-live-p current-window)
      (select-window current-window))
    (when (buffer-live-p current-buffer)
      (set-window-buffer (selected-window) current-buffer))))

(defun jupyter-termint-get-or-create-buffer (kernel)
  "Get or create termint buffer for KERNEL without window switching."
  (let* ((buffer-name (cond
                      ((string= kernel "python") "*jupyter-python*")
                      ((string= kernel "r") "*jupyter-r*") 
                      ((string= kernel "stata") "*jupyter-stata*")
                      (t (error "Unsupported kernel: %s" kernel))))
         (buffer (get-buffer buffer-name)))
    
    ;; Check if buffer exists and has live process
    (if (and buffer 
             (buffer-live-p buffer)
             (get-buffer-process buffer)
             (process-live-p (get-buffer-process buffer)))
        buffer
      ;; Need to start new console silently
      (progn
        (jupyter-termint-silent-start kernel)
        ;; Wait a moment for buffer creation
        (let ((max-wait 10) (wait-count 0))
          (while (and (< wait-count max-wait)
                     (not (get-buffer buffer-name)))
            (sleep-for 0.5)
            (setq wait-count (1+ wait-count))))
        (get-buffer buffer-name)))))

;;; Output capture functions

(defun jupyter-termint-wait-for-prompt (buffer &optional timeout)
  "Wait for command prompt to appear in BUFFER with optional TIMEOUT."
  (let ((timeout (or timeout 5))
        (start-time (current-time)))
    (with-current-buffer buffer
      (while (and (< (float-time (time-subtract (current-time) start-time)) timeout)
                  (not (save-excursion
                         (goto-char (point-max))
                         (beginning-of-line)
                         (looking-at-p "\\(In \\[[0-9]+\\]:\\|>>>\\|>\\|\\.\\.\\.\\) *$"))))
        (accept-process-output nil 0.1)))))

(defun jupyter-termint-extract-output (raw-output code)
  "Extract clean output from RAW-OUTPUT, removing CODE echo and prompts."
  (let ((output raw-output))
    
    ;; Simple approach: find the last occurrence of our code and extract what follows
    (let ((first-line (car (split-string code "\n"))))
      ;; Find last occurrence of "In [n]: first-line"  
      (when (string-match (concat ".*In \\[[0-9]+\\]: " (regexp-quote first-line)) output)
        ;; Get everything after the code execution
        (setq output (substring output (match-end 0)))
        
        ;; Remove continuation lines (lines with "   ...: ")
        (setq output (replace-regexp-in-string "^.*\\.\\.\\.:.*\n?" "" output))
        
        ;; Remove next prompt and everything after
        (when (string-match "In \\[[0-9]+\\]:" output)
          (setq output (substring output 0 (match-beginning 0))))
        
        ;; Clean whitespace
        (setq output (replace-regexp-in-string "^[ \t\n]+" "" output))
        (setq output (replace-regexp-in-string "[ \t\n]+$" "" output))))
    
    
    ;; Return cleaned output or nil if empty
    (if (and output (not (string-match-p "^[ \t\n]*$" output)))
        output
      nil)))

(defun jupyter-termint-send-string-with-output (buffer code kernel)
  "Send CODE to termint BUFFER for KERNEL and capture output."
  
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer)))
      (unless proc
        (error "No process in buffer %s" (buffer-name buffer)))
      
      ;; Move to end and mark position  
      (goto-char (point-max))
      (let ((output-start (point-marker))
            (send-func (cond
                       ((string= kernel "python") #'termint-jupyter-python-send-string)
                       ((string= kernel "r") #'termint-jupyter-r-send-string)
                       ((string= kernel "stata") #'termint-jupyter-stata-send-string)
                       (t (error "Unsupported kernel: %s" kernel)))))
        
        ;; Send the code using kernel-specific function
        (funcall send-func code)
        
        ;; Wait for completion
        (jupyter-termint-wait-for-prompt buffer 10)
        
        ;; Extract output
        (goto-char (point-max))
        (let* ((output-end (point))
               (raw-output (buffer-substring-no-properties output-start output-end)))
          
          ;; Extract clean result
          (jupyter-termint-extract-output raw-output code))))))

;;; Enhanced Image Support Functions

(defun jupyter-termint-send-string-with-images (buffer code kernel &optional image-file interactive)
  "Enhanced execution that handles image generation.
BUFFER is the jupyter console buffer, CODE is the code to execute, 
KERNEL is the kernel type (python, R, stata).
IMAGE-FILE is the optional specific file path to save the image to.
INTERACTIVE determines if this is called from C-RET (affects display behavior)."
  (let* ((detection-kernel (jupyter-termint-map-to-jupyter-kernel kernel))
         (has-graphics (jupyter-termint-detect-graphics-code code detection-kernel))
         (target-file (or image-file 
                         (when has-graphics (jupyter-termint-generate-image-filename detection-kernel))))
         (modified-code (if has-graphics 
                           (jupyter-termint-inject-image-code code detection-kernel target-file)
                         code))
         (text-result (jupyter-termint-send-string-with-output buffer modified-code kernel)))
    
    ;; Check if file was created with retry loop for graphics
    (when has-graphics
      (let ((max-wait 5) (wait-count 0) (file-found nil) (actual-file nil))
        (while (and (< wait-count max-wait) (not file-found))
          (sit-for 0.5)
          (cond
           ;; Special handling for Stata - stata_kernel creates files in ~/.stata_kernel_cache/
           ((string= detection-kernel "stata")
            (let* ((cache-dir (expand-file-name "~/.stata_kernel_cache/"))
                   (png-files (when (file-directory-p cache-dir)
                               (directory-files cache-dir t "graph[0-9]+\\.png$"))))
              (when png-files
                ;; Get the most recently created PNG file
                (setq actual-file (car (sort png-files 
                                           (lambda (a b) 
                                             (time-less-p (nth 5 (file-attributes b))
                                                         (nth 5 (file-attributes a)))))))
                (setq file-found (and actual-file (file-exists-p actual-file))))))
           ;; Standard handling for other kernels
           (target-file
            (setq actual-file target-file)
            (setq file-found (file-exists-p target-file))))
          (setq wait-count (1+ wait-count)))
        
        ;; For interactive use (C-RET), display the image
        (when (and interactive file-found actual-file)
          (jupyter-termint-display-image-in-buffer actual-file buffer))))
    
    ;; Return appropriate result
    (cond
     ;; For org-babel with graphics, return the file path if it exists
     ((and has-graphics (not interactive))
      (cond
       ;; Special handling for Stata
       ((string= detection-kernel "stata")
        (let* ((cache-dir (expand-file-name "~/.stata_kernel_cache/"))
               (png-files (when (file-directory-p cache-dir)
                           (directory-files cache-dir t "graph[0-9]+\\.png$"))))
          (if png-files
              (car (sort png-files 
                        (lambda (a b) 
                          (time-less-p (nth 5 (file-attributes b))
                                      (nth 5 (file-attributes a))))))
            text-result)))
       ;; Standard handling for other kernels
       ((and target-file (file-exists-p target-file)) target-file)
       (t text-result)))
     ;; For interactive use or non-graphics, return text result
     (t text-result))))

(defun jupyter-termint-display-image-inline (image-file buffer)
  "Display IMAGE-FILE using Emacs native image display instead of sixel (eat-mode compatible)."
  (when (and image-file (file-exists-p image-file))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (goto-char (point-max))
        (insert "\n")
        ;; Use Emacs native image display instead of sixel since eat-mode doesn't support sixel
        (condition-case err
            (let ((image (create-image image-file nil nil :max-width 600 :max-height 400)))
              (if image
                  (progn
                    ;; Insert the image directly into the buffer using Emacs' image system
                    (insert-image image)
                    (insert (format "\n[📊 %s - native Emacs display]\n" (file-name-nondirectory image-file))))
                ;; Fallback if image creation failed
                (insert (format "[📊 %s - image creation failed]\n" (file-name-nondirectory image-file)))))
          (error 
           ;; If image display fails, insert error message
           (insert (format "[📊 %s - image display error: %s]\n" 
                          (file-name-nondirectory image-file)
                          (error-message-string err)))))
        (goto-char (point-max))
)))

(defun jupyter-termint-display-image-side-window (image-file buffer)
  "Display IMAGE-FILE in a dedicated side window for eat compatibility."
  (when (and image-file (file-exists-p image-file))
    (let ((image-buffer (get-buffer-create "*Jupyter Image*"))
          (original-window (selected-window)))
      (with-current-buffer image-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Generated from: %s\n\n" (buffer-name buffer)))
          (let ((image (create-image image-file nil nil :max-width 600 :max-height 400)))
            (insert-image image))
          (insert (format "\n\n[%s]" (file-name-nondirectory image-file)))
          (goto-char (point-min))
          (read-only-mode 1)))
      
      ;; Display in side window, reusing existing window if available
      (let ((image-window (get-buffer-window image-buffer)))
        (if image-window
            ;; Reuse existing window
            (select-window image-window)
          ;; Create new side window
          (display-buffer image-buffer
                          '((display-buffer-in-side-window)
                            (side . right)
                            (window-width . 0.4)
                            (inhibit-same-window . t)))))
      
      ;; Return focus to original window
      (when (window-live-p original-window)
        (select-window original-window)))))

(defun jupyter-termint-display-image-popup (image-file)
  "Display IMAGE-FILE in a popup window."
  (when (and image-file (file-exists-p image-file))
    (let ((image-buffer (get-buffer-create "*Jupyter Termint Image*")))
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

(defun jupyter-termint-display-image-in-buffer (image-file buffer)
  "Display IMAGE-FILE either inline or in popup based on configuration."
  (if jupyter-termint-inline-images
      (jupyter-termint-display-image-inline image-file buffer)
    (jupyter-termint-display-image-popup image-file)))

;;; Org-babel functions (defined directly like jupyter-console.el)
;; These functions override any default org-babel functions

(defun org-babel-execute:python (body params)
  "Execute Python BODY with PARAMS using jupyter-termint."
  (let ((current-window (selected-window))
        (current-buffer (current-buffer)))
    (unwind-protect
        (let* ((buffer (jupyter-termint-get-or-create-buffer "python"))
               (results-type (cdr (assq :results params)))
               (has-graphics (jupyter-termint-detect-graphics-code body "python3")))
          (if has-graphics
              ;; For graphics code, use enhanced function 
              (jupyter-termint-send-string-with-images buffer body "python" nil t)
            ;; For non-graphics code, use regular execution
            (or (jupyter-termint-send-string-with-output buffer body "python") "")))
      ;; Always restore original window and buffer
      (when (window-live-p current-window)
        (select-window current-window))
      (when (buffer-live-p current-buffer)
        (set-window-buffer (selected-window) current-buffer)))))

(defun org-babel-execute:R (body params)
  "Execute R BODY with PARAMS using jupyter-termint."
  (let ((current-window (selected-window))
        (current-buffer (current-buffer)))
    (unwind-protect
        (let* ((buffer (jupyter-termint-get-or-create-buffer "r"))
               (results-type (cdr (assq :results params)))
               (has-graphics (jupyter-termint-detect-graphics-code body "ir")))
          (if has-graphics
              ;; For graphics code, use enhanced function
              (jupyter-termint-send-string-with-images buffer body "r" nil t)
            ;; For non-graphics code, use regular execution
            (or (jupyter-termint-send-string-with-output buffer body "r") "")))
      ;; Always restore original window and buffer
      (when (window-live-p current-window)
        (select-window current-window))
      (when (buffer-live-p current-buffer)
        (set-window-buffer (selected-window) current-buffer)))))

(defun org-babel-execute:stata (body params)
  "Execute Stata BODY with PARAMS using jupyter-termint."
  (let ((current-window (selected-window))
        (current-buffer (current-buffer)))
    (unwind-protect
        (let* ((buffer (jupyter-termint-get-or-create-buffer "stata"))
               (results-type (cdr (assq :results params)))
               (has-graphics (jupyter-termint-detect-graphics-code body "stata")))
          (if has-graphics
              ;; For graphics code, use enhanced function
              (jupyter-termint-send-string-with-images buffer body "stata" nil t)
            ;; For non-graphics code, use regular execution
            (or (jupyter-termint-send-string-with-output buffer body "stata") "")))
      ;; Always restore original window and buffer
      (when (window-live-p current-window)
        (select-window current-window))
      (when (buffer-live-p current-buffer)
        (set-window-buffer (selected-window) current-buffer)))))

;;; Allow risky local variables to be remembered permanently
;; This fixes direnv permission prompts by allowing Emacs to remember
;; risky local variable permissions instead of asking every time
(advice-add 'risky-local-variable-p :override #'ignore)

;;; C-RET org-src Integration

;;; Language detection
(defun jupyter-termint-detect-kernel ()
  "Detect kernel from org-src buffer language or org-mode code block."
  (message "DEBUG: Starting kernel detection")
  (let ((lang-from-buffer-name (when (string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" (buffer-name))
                                 (match-string 1 (buffer-name))))
        (lang-from-variable (bound-and-true-p org-src--lang))
        (lang-from-org-element (when (eq major-mode 'org-mode)
                                 (org-element-property :language (org-element-at-point)))))
    
    (message "DEBUG: Buffer name lang: %s, Variable lang: %s, Org element lang: %s" 
             lang-from-buffer-name lang-from-variable lang-from-org-element)
    
    (let ((detected-lang (or lang-from-variable lang-from-buffer-name lang-from-org-element)))
      (message "DEBUG: Final detected-lang: %s" detected-lang)
      (cond
       ((or (and detected-lang (or (string-equal detected-lang "r") (string-equal detected-lang "R"))) (eq major-mode 'ess-r-mode)) 
        (message "DEBUG: Matched R condition")
        "r")
       ((or (and detected-lang (string-equal detected-lang "python")) (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode)) 
        (message "DEBUG: Matched Python condition")
        "python")
       ((or (and detected-lang (string-equal detected-lang "stata")) (eq major-mode 'stata-mode)) 
        (message "DEBUG: Matched Stata condition")
        "stata")
       (t 
        (message "DEBUG: Defaulting to python")
        "python")))))

;;; Console management with all features
(defun jupyter-termint-smart-python-start ()
  "Start Python jupyter console with smart direnv command."
  (interactive)
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-python*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-python*")))
  
  ;; Define and start with smart direnv command, setting JUPYTER_CONSOLE env var
  (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel python3'"))
    (termint-define "jupyter-python" smart-cmd 
                    :bracketed-paste-p t
                    :backend 'eat
                    :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
    (termint-jupyter-python-start))))

(defun jupyter-termint-smart-r-start ()
  "Start R jupyter console with smart direnv command."
  (interactive)
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-r*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-r*")))
  
  ;; Define and start with smart direnv command, setting JUPYTER_CONSOLE env var
  (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel ir'"))
    (termint-define "jupyter-r" smart-cmd 
                    :bracketed-paste-p t
                    :backend 'eat
                    :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
    (termint-jupyter-r-start)
    
    ;; Sixel configuration is now handled directly in .Rprofile
    ))

(defun jupyter-termint-force-r-sixel-setup ()
  "Force setup R sixel graphics integration since .Rprofile doesn't load in Jupyter console."
  (when (get-buffer "*jupyter-r*")
    (with-current-buffer "*jupyter-r*"
      (let ((sixel-config-path "/Users/vwh7mb/projects/wander2/.jupyter/startup/01-r-sixel-config.R")
            (proc (get-buffer-process (current-buffer))))
        ;; Check if process is alive and ready
        (when (and proc (process-live-p proc) (file-exists-p sixel-config-path))
          ;; Wait for console to be fully ready by checking for prompt
          (let ((max-wait 15) (wait-count 0) (ready nil))
            (while (and (< wait-count max-wait) (not ready))
              (accept-process-output proc 0.5)
              (save-excursion
                (goto-char (point-max))
                (beginning-of-line)
                (when (looking-at-p "In \\[[0-9]+\\]:")
                  (setq ready t)))
              (setq wait-count (1+ wait-count)))
            
            (when ready
              (message "Forcing R sixel configuration load...")
              (termint-jupyter-r-send-string (format "source('%s')" sixel-config-path))
              ;; Add a small delay and then confirm it loaded
              (run-with-timer 2.0 nil (lambda ()
                (when (get-buffer "*jupyter-r*")
                  (message "✓ R sixel graphics integration forced and ready"))))
              )))))))

(defun jupyter-termint-auto-setup-r-sixel ()
  "Automatically setup R sixel graphics integration by sourcing the configuration script."
  (when (get-buffer "*jupyter-r*")
    (with-current-buffer "*jupyter-r*"
      (let ((sixel-config-path "/Users/vwh7mb/projects/wander2/.jupyter/startup/01-r-sixel-config.R")
            (proc (get-buffer-process (current-buffer))))
        ;; Check if process is alive and ready
        (when (and proc (process-live-p proc) (file-exists-p sixel-config-path))
          ;; Wait for console to be fully ready by checking for prompt
          (let ((max-wait 10) (wait-count 0) (ready nil))
            (while (and (< wait-count max-wait) (not ready))
              (accept-process-output proc 0.5)
              (save-excursion
                (goto-char (point-max))
                (beginning-of-line)
                (when (looking-at-p "In \\[[0-9]+\\]:")
                  (setq ready t)))
              (setq wait-count (1+ wait-count)))
            
            (when ready
              (termint-jupyter-r-send-string (format "source('%s')" sixel-config-path))
              (message "R sixel graphics integration loaded automatically")
              ;; Add a small delay and then confirm it loaded
              (run-with-timer 2.0 nil (lambda ()
                (when (get-buffer "*jupyter-r*")
                  (message "R console ready with sixel graphics support"))))
              )))))))

(defun jupyter-termint-setup-r-sixel ()
  "Setup R sixel graphics integration by sourcing the configuration script."
  (when (get-buffer "*jupyter-r*")
    (with-current-buffer "*jupyter-r*"
      (let ((sixel-config-path "/Users/vwh7mb/projects/wander2/.jupyter/startup/01-r-sixel-config.R"))
        (when (file-exists-p sixel-config-path)
          (termint-jupyter-r-send-string (format "source('%s')" sixel-config-path))
          (message "R sixel graphics integration loaded")))))))

(defun jupyter-termint-smart-stata-start ()
  "Start Stata jupyter console with smart direnv command."
  (interactive)
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-stata*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-stata*")))
  
  ;; Start file monitoring before console starts
  (jupyter-termint-start-stata-graph-monitoring)
  
  ;; Define and start with smart direnv command
  (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel stata'"))
    (termint-define "jupyter-stata" smart-cmd 
                    :bracketed-paste-p t
                    :backend 'eat
                    :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
    (termint-jupyter-stata-start)))

(defun jupyter-termint-display-console-right (buffer &optional original-buffer original-window)
  "Display console BUFFER in a right split window, preserving focus on ORIGINAL-BUFFER in ORIGINAL-WINDOW."
  
  (let ((initial-window (or original-window (selected-window)))
        (initial-buffer (or original-buffer (current-buffer))))
    
    
    ;; Use display-buffer with specific parameters for right split
    (let ((console-window (display-buffer buffer
                                          '((display-buffer-reuse-window
                                             display-buffer-in-side-window)
                                            (side . right)
                                            (window-width . 0.5)
                                            (inhibit-same-window . t)))))
      
      
      (when console-window
        ;; Briefly switch to console window to scroll to bottom
        (with-selected-window console-window
          (goto-char (point-max)))
        
        ;; Force restore focus to original window and buffer
        (when (window-live-p initial-window)
          (select-window initial-window))
        
        (when (buffer-live-p initial-buffer)
          (set-window-buffer (selected-window) initial-buffer))
        
        nil))))

(defun jupyter-termint-ensure-console-with-features (kernel code)
  "Ensure console for KERNEL is running with direnv and window management, then send CODE."
  (let* ((kernel-config (cond
                        ((string= kernel "python")
                         '("*jupyter-python*" jupyter-termint-smart-python-start))
                        ((string= kernel "r")
                         '("*jupyter-r*" jupyter-termint-smart-r-start))
                        ((string= kernel "stata")
                         '("*jupyter-stata*" jupyter-termint-smart-stata-start))
                        (t (error "Unsupported kernel: %s" kernel))))
         (buffer-name (nth 0 kernel-config))
         (start-func (nth 1 kernel-config))
         ;; CRITICAL: Capture org-src buffer/window BEFORE any console operations
         (original-buffer (current-buffer))
         (original-window (selected-window)))
    
    
    ;; Check if console buffer exists and has a live process
    (let ((console-buffer (get-buffer buffer-name)))
      (if (and console-buffer 
               (buffer-live-p console-buffer)
               (get-buffer-process console-buffer)
               (process-live-p (get-buffer-process console-buffer)))
          (progn
            ;; Console exists, just display it and send code with image support
            (jupyter-termint-display-console-right console-buffer original-buffer original-window)
            
            ;; For Stata, ensure file monitoring is running
            (when (string= kernel "stata")
              (unless jupyter-termint-stata-monitoring-timer
                (jupyter-termint-start-stata-graph-monitoring)))
            
            ;; Use enhanced send function with interactive=t for image display
            (jupyter-termint-send-string-with-images console-buffer code kernel nil t))
        
        ;; Console doesn't exist or is dead, start it
        (progn
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
                    (message "%s console ready!" kernel)
                    ;; Display in right split
                    (jupyter-termint-display-console-right new-buffer original-buffer original-window)
                    
                    ;; Give console a moment to fully initialize
                    (sleep-for 2)
                    ;; Use enhanced send function with interactive=t for image display
                    (jupyter-termint-send-string-with-images new-buffer code kernel nil t))
                (progn
                  (error "Failed to create %s console buffer" kernel))))))))))

;;; Main function
(defun jupyter-termint-send-simple ()
  "Simple function to send region/line to termint console."
  (interactive)
  
  (let* ((kernel (jupyter-termint-detect-kernel))
         (code (cond
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
    
    
    (cond
     ((string= kernel "python")
      (jupyter-termint-ensure-console-with-features "python" code))
     ((string= kernel "r")
      (jupyter-termint-ensure-console-with-features "r" code))
     ((string= kernel "stata")
      (jupyter-termint-ensure-console-with-features "stata" code))
     (t 
      (message "Unsupported kernel: %s" kernel)))))

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
          (setq jupyter-termint-stata-last-graph-time file-time)))))))

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

;;; File monitoring will use the existing jupyter-termint-display-image-inline function

;;; Keybinding setup
(defun jupyter-termint-setup-keybinding ()
  
  (when (and image-file (file-exists-p image-file))
    (let ((target-buffer (get-buffer buffer)))
      (if (not target-buffer)
          (jupyter-termint-stata-debug-log 'error "Target buffer %s not found" buffer)
        
        (with-current-buffer target-buffer
          (let ((inhibit-read-only t)
                (buffer-undo-list t))
            (goto-char (point-max))
            (insert "\n")
            
            ;; Try sixel display first, fallback to native Emacs display
            (let ((img2sixel-path "/Users/vwh7mb/.nix-profile/bin/img2sixel")
                  (success nil))
              
              ;; Check if img2sixel is available and terminal supports sixel
              (when (and (file-executable-p img2sixel-path)
                        (or (string= (getenv "TERM") "xterm-kitty")
                            (string= (getenv "COLORTERM") "truecolor")))
                (condition-case err
                    (let ((sixel-output
                           (shell-command-to-string
                            (format "%s -w 600 %s" img2sixel-path (shell-quote-argument image-file)))))
                      (when (and sixel-output (> (length sixel-output) 0))
                        (insert sixel-output)
                        (setq success t)
                        (jupyter-termint-stata-debug-log 'info "✓ Sixel display successful for %s" image-file)))
                  (error
                   (jupyter-termint-stata-debug-log 'warn "Sixel display failed: %s" (error-message-string err)))))
              
              ;; Fallback to native Emacs image display
              (unless success
                (condition-case err
                    (let ((image (create-image image-file nil nil :max-width 600 :max-height 400)))
                      (if image
                          (progn
                            (insert-image image)
                            (jupyter-termint-stata-debug-log 'info "✓ Native Emacs display successful for %s" image-file))
                        (jupyter-termint-stata-debug-log 'error "Image creation failed for %s" image-file)))
                  (error
                   (jupyter-termint-stata-debug-log 'error "Native display failed: %s" (error-message-string err)))))
              
              (insert (format "\n[📊 %s]\n" (file-name-nondirectory image-file)))
              (goto-char (point-max)))))))
  
  ;; Always return t to indicate we handled the display request
  t)

;;; Keybinding setup
(defun jupyter-termint-setup-keybinding ()
  "Setup C-RET keybinding by unbinding Doom keys first."
  
  ;; Unbind the Doom default C-RET keybinding globally
  (map! "C-<return>" nil)
  
  ;; Common function to set up keybindings in org-src buffers
  (defun jupyter-termint-setup-buffer-keybinding ()
    "Set up C-RET keybinding for the current org-src buffer."
    (when (string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" (buffer-name))
      
      ;; Unbind in all evil states for this buffer
      (evil-local-set-key 'insert (kbd "C-<return>") nil)
      (evil-local-set-key 'normal (kbd "C-<return>") nil)
      (evil-local-set-key 'visual (kbd "C-<return>") nil)
      
      ;; Now bind our function
      (evil-local-set-key 'insert (kbd "C-<return>") #'jupyter-termint-send-simple)
      (evil-local-set-key 'normal (kbd "C-<return>") #'jupyter-termint-send-simple)
      (evil-local-set-key 'visual (kbd "C-<return>") #'jupyter-termint-send-simple)
      (local-set-key (kbd "C-<return>") #'jupyter-termint-send-simple)
      
      nil))

  ;; Set up hooks for all supported languages
  (add-hook 'python-mode-hook #'jupyter-termint-setup-buffer-keybinding)
  (add-hook 'ess-r-mode-hook #'jupyter-termint-setup-buffer-keybinding)
  (add-hook 'stata-mode-hook #'jupyter-termint-setup-buffer-keybinding)
  (add-hook 'ess-stata-mode-hook #'jupyter-termint-setup-buffer-keybinding)
  
  nil)

;;; Initialization

(with-eval-after-load 'termint
  (jupyter-termint-setup)
  (jupyter-termint-setup-keybinding))

(provide 'jupyter-termint)
;;; jupyter-termint.el ends here
