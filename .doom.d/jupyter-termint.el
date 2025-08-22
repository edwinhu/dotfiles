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
  
  ;; Configure termint backend
  (setq termint-backend 'vterm)
  
  ;; Find the best jupyter executable path
  (let ((jupyter-path (or (executable-find "jupyter")
                          (when (file-executable-p ".pixi/envs/default/bin/jupyter")
                            ".pixi/envs/default/bin/jupyter")
                          (when (file-executable-p "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter")
                            "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter"))))
    
    
    (if jupyter-path
        (progn
          (message "jupyter-termint: Using jupyter at: %s" jupyter-path)
          
          ;; Define termint sessions with smart direnv handling
          (let ((pixi-project-dir "/Users/vwh7mb/projects/wander2"))
            ;; Check direnv status once for logging
            (if (jupyter-termint--check-direnv-allowed pixi-project-dir)
                (progn
                  (message "jupyter-termint: Direnv already allowed for %s - using direnv exec" pixi-project-dir)
                  nil))
              (progn
                (message "jupyter-termint: Direnv not yet allowed for %s - may prompt on first run" pixi-project-dir)
                nil))
            
            ;; Build smart commands with direnv handling
            (let ((python-cmd (jupyter-termint--build-smart-command 
                              pixi-project-dir 
                              "pixi run jupyter console --kernel python3 --simple-prompt"))
                  (r-cmd (jupyter-termint--build-smart-command 
                         pixi-project-dir 
                         "pixi run jupyter console --kernel ir --simple-prompt"))
                  (stata-cmd (format "%s console --kernel stata --simple-prompt" jupyter-path)))
              
              ;; Define Python Jupyter console with smart direnv handling
              (termint-define "jupyter-python" python-cmd
                              :bracketed-paste-p t)
              
              ;; Define R Jupyter console with smart direnv handling
              (termint-define "jupyter-r" r-cmd
                              :bracketed-paste-p t)
              
              ;; Define Stata Jupyter console
              (termint-define "jupyter-stata" stata-cmd
                              :bracketed-paste-p t)
              nil)))
      
      (progn
        (message "jupyter-termint: ERROR - No jupyter executable found. Check your PATH or pixi environment.")
        nil))))
  
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
                      ((string= kernel "R") "*jupyter-r*") 
                      ((string= kernel "stata") "*jupyter-stata*")
                      (t (error "Unsupported kernel: %s" kernel))))
         (smart-cmd (cond
                    ((string= kernel "python") 
                     "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel python3 --simple-prompt'")
                    ((string= kernel "R")
                     "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel ir --simple-prompt'")
                    ((string= kernel "stata")
                     "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel stata --simple-prompt'")
                    (t (error "Unsupported kernel: %s" kernel))))
         (termint-name (cond
                       ((string= kernel "python") "jupyter-python")
                       ((string= kernel "R") "jupyter-r")
                       ((string= kernel "stata") "jupyter-stata")
                       (t (error "Unsupported kernel: %s" kernel))))
         (current-window (selected-window))
         (current-buffer (current-buffer)))
    
    ;; Kill any existing hung buffer first
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))
    
    ;; Define and start with smart command
    (termint-define termint-name smart-cmd :bracketed-paste-p t)
    
    ;; Start the console with display suppression
    (let ((display-buffer-alist (cons '("\\*jupyter-.*\\*" display-buffer-no-window) display-buffer-alist)))
      (cond
       ((string= kernel "python") (termint-jupyter-python-start))
       ((string= kernel "R") (termint-jupyter-r-start))
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
                      ((string= kernel "R") "*jupyter-r*") 
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
                       ((string= kernel "R") #'termint-jupyter-r-send-string)
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
  (let* ((detection-kernel (cond
                           ((string= kernel "python") "python3")
                           ((string= kernel "R") "ir") 
                           ((string= kernel "stata") "stata")
                           (t kernel)))
         (has-graphics (jupyter-termint-detect-graphics-code code detection-kernel))
         (target-file (or image-file 
                         (when has-graphics (jupyter-termint-generate-image-filename detection-kernel))))
         (modified-code (if has-graphics 
                           (jupyter-termint-inject-image-code code detection-kernel target-file)
                         code))
         (text-result (jupyter-termint-send-string-with-output buffer modified-code kernel)))
    
    ;; Check if file was created with retry loop for graphics
    (when (and has-graphics target-file)
      (let ((max-wait 5) (wait-count 0) (file-found nil))
        (while (and (< wait-count max-wait) (not file-found))
          (sit-for 0.5)
          (setq file-found (file-exists-p target-file))
          (setq wait-count (1+ wait-count)))
        
        ;; For interactive use (C-RET), display the image
        (when (and interactive file-found)
          (jupyter-termint-display-image-in-buffer target-file buffer))))
    
    ;; Return appropriate result
    (cond
     ;; For org-babel with graphics, return the file path if it exists
     ((and has-graphics target-file (not interactive) (file-exists-p target-file))
      target-file)
     ;; For interactive use or non-graphics, return text result
     (t text-result))))

(defun jupyter-termint-display-image-inline (image-file buffer)
  "Display IMAGE-FILE inline or in side window for vterm buffers."
  (when (and image-file (file-exists-p image-file))
    ;; For vterm buffers, images don't render properly inline
    ;; Use side window display with buffer notification
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n📊 [Image generated: %s]\n" (file-name-nondirectory image-file)))
        (goto-char (point-max))))
    
    ;; Display image in side window for better vterm compatibility
    (jupyter-termint-display-image-side-window image-file buffer)))

(defun jupyter-termint-display-image-side-window (image-file buffer)
  "Display IMAGE-FILE in a dedicated side window for vterm compatibility."
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
              (jupyter-termint-send-string-with-images buffer body "python" nil nil)
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
        (let* ((buffer (jupyter-termint-get-or-create-buffer "R"))
               (results-type (cdr (assq :results params)))
               (has-graphics (jupyter-termint-detect-graphics-code body "ir")))
          (if has-graphics
              ;; For graphics code, use enhanced function
              (jupyter-termint-send-string-with-images buffer body "R" nil nil)
            ;; For non-graphics code, use regular execution
            (or (jupyter-termint-send-string-with-output buffer body "R") "")))
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
              (jupyter-termint-send-string-with-images buffer body "stata" nil nil)
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
  "Detect kernel from org-src buffer language."
  (let ((lang-from-buffer-name (when (string-match "\\*Org Src.*\\[ \\([^]]+\\) \\]\\*" (buffer-name))
                                 (match-string 1 (buffer-name))))
        (lang-from-variable (bound-and-true-p org-src--lang)))
    
    (let ((detected-lang (or lang-from-variable lang-from-buffer-name)))
      (cond
       ((or (equal detected-lang "python") (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode)) "python")
       ((or (equal detected-lang "r") (equal detected-lang "R") (eq major-mode 'ess-r-mode)) "r") 
       ((or (equal detected-lang "stata") (eq major-mode 'stata-mode)) "stata")
       (t "python")))))

;;; Console management with all features
(defun jupyter-termint-smart-python-start ()
  "Start Python jupyter console with smart direnv command."
  (interactive)
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-python*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-python*")))
  
  ;; Define and start with smart direnv command
  (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel python3 --simple-prompt'"))
    (termint-define "jupyter-python" smart-cmd :bracketed-paste-p t)
    (termint-jupyter-python-start)))

(defun jupyter-termint-smart-r-start ()
  "Start R jupyter console with smart direnv command."
  (interactive)
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-r*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-r*")))
  
  ;; Define and start with smart direnv command
  (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel ir --simple-prompt'"))
    (termint-define "jupyter-r" smart-cmd :bracketed-paste-p t)
    (termint-jupyter-r-start)))

(defun jupyter-termint-smart-stata-start ()
  "Start Stata jupyter console with smart direnv command."
  (interactive)
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-stata*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-stata*")))
  
  ;; Define and start with smart direnv command
  (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel stata --simple-prompt'"))
    (termint-define "jupyter-stata" smart-cmd :bracketed-paste-p t)
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
                    ;; Give it a moment to fully initialize, then send code with image support
                    (sleep-for 1)
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
                ((> (point-max) (point-min))
                 (buffer-substring-no-properties (point-min) (point-max)))
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

;;; Keybinding setup
(defun jupyter-termint-setup-keybinding ()
  "Setup C-RET keybinding by unbinding Doom keys first."
  
  ;; Unbind the Doom default C-RET keybinding globally
  (map! "C-<return>" nil)
  
  ;; Common function to set up keybindings in org-src buffers
  (defun jupyter-termint-setup-buffer-keybinding ()
    "Set up C-RET keybinding for the current org-src buffer."
    (when (string-match "\\*Org Src.*\\[ \\([^]]+\\) \\]\\*" (buffer-name))
      
      ;; Unbind in all evil states for this buffer
      (evil-local-set-key 'insert (kbd "C-<return>") nil)
      (evil-local-set-key 'normal (kbd "C-<return>") nil)
      (evil-local-set-key 'visual (kbd "C-<return>") nil)
      
      ;; Now bind our function
      (evil-local-set-key 'insert (kbd "C-<return>") #'jupyter-termint-send-simple)
      (evil-local-set-key 'normal (kbd "C-<return>") #'jupyter-termint-send-simple)
      (evil-local-set-key 'visual (kbd "C-<return>") #'jupyter-termint-send-simple)
      (local-set-key (kbd "C-<return>") #'jupyter-termint-send-simple)
      
      nil)))

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