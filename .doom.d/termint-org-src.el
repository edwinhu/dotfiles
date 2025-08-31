;;; termint-org-src.el --- Termint integration for org-src edit buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; This provides C-RET functionality in org-src edit buffers that sends
;; code to termint jupyter consoles. It detects regions, functions, paragraphs
;; or lines and sends them with bracketed paste support.

;;; Code:

(require 'org)
(require 'org-src)
(require 'termint)

;;; File-based logging for debugging
(defvar termint-org-src-debug-log-file (expand-file-name "termint-org-src-debug.log" "~/"))

(defun termint-org-src-debug-log (level format-string &rest args)
  "Log with timestamp to file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) termint-org-src-debug-log-file))))

(defun termint-org-src-clear-log ()
  "Clear the termint org-src debug log file."
  (interactive)
  (when (file-exists-p termint-org-src-debug-log-file)
    (delete-file termint-org-src-debug-log-file))
  (termint-org-src-debug-log 'info "=== Termint Org-Src Log Started ==="))

;;; Detection and Region Functions

(defun termint-org-src-detect-kernel ()
  "Detect the appropriate termint kernel based on org-src language."
  (termint-org-src-debug-log 'debug "Detecting kernel for org-src buffer")
  (cond
   ;; Check if we're in org-src-mode with language detection
   ((and (bound-and-true-p org-src-mode)
         (boundp 'org-src--babel-type))
    (let ((lang org-src--babel-type))
      (termint-org-src-debug-log 'debug "Found org-src language: %s" lang)
      (pcase lang
        ((or "python" "jupyter-python") "python")
        ((or "R" "jupyter-R") "r") 
        ((or "stata" "jupyter-stata") "stata")
        ((or "sas" "jupyter-sas") "sas")
        (_ (error "Unsupported language: %s" lang)))))
   ;; Fallback to major mode detection
   ((derived-mode-p 'python-mode 'python-ts-mode) "python")
   ((derived-mode-p 'ess-r-mode 'ess-mode) "r")
   ((derived-mode-p 'stata-mode) "stata")
   ((derived-mode-p 'SAS-mode 'ess-sas-mode) "sas")
   (t (error "Cannot detect kernel for mode: %s" major-mode))))

(defun termint-org-src-get-function-bounds ()
  "Get the bounds of the current function.
Returns a cons cell (START . END) or nil if not in a function."
  (termint-org-src-debug-log 'debug "Searching for function bounds")
  ;; Only try to detect functions in modes that have proper function detection
  (when (or (derived-mode-p 'python-mode 'python-ts-mode)
            (derived-mode-p 'ess-r-mode 'ess-mode))
    (condition-case err
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
              (termint-org-src-debug-log 'debug "Found function bounds: %d-%d" start end)
              (cons start end))))
      (error 
       (termint-org-src-debug-log 'debug "Function detection failed: %s" err)
       nil))))

(defun termint-org-src-get-paragraph-bounds ()
  "Get the bounds of the current paragraph.
Returns a cons cell (START . END)."
  (termint-org-src-debug-log 'debug "Calculating paragraph bounds")
  (save-excursion
    ;; In org-src buffers, just use the whole buffer if it's small
    (if (and (bound-and-true-p org-src-mode)
             (< (buffer-size) 1000))  ; Small buffer, likely single code block
        (progn
          (termint-org-src-debug-log 'debug "Using whole buffer as paragraph: %d-%d" (point-min) (point-max))
          (cons (point-min) (point-max)))
      ;; Otherwise use normal paragraph detection
      (let ((start (progn (backward-paragraph) 
                         (skip-chars-forward " \t\n")
                         (point)))
            (end (progn (forward-paragraph)
                       (skip-chars-backward " \t\n")
                       (point))))
        (termint-org-src-debug-log 'debug "Found paragraph bounds: %d-%d" start end)
        (cons start end)))))

(defun termint-org-src-next-code-line ()
  "Move to the next line containing code (skip comments and blank lines)."
  (termint-org-src-debug-log 'debug "Moving to next code line")
  (forward-line 1)
  (while (and (not (eobp))
              (or (looking-at "^[[:space:]]*$")  ; blank line
                  (looking-at "^[[:space:]]*#")   ; Python/R comment
                  (looking-at "^[[:space:]]*\\*")  ; Stata comment
                  (looking-at "^[[:space:]]*//")   ; C-style comment
                  ))
    (forward-line 1))
  (beginning-of-line))

;;; Console Management Functions

(defun termint-org-src-smart-python-start ()
  "Start Python jupyter console with smart direnv command."
  (interactive)
  (termint-org-src-debug-log 'info "=== SMART PYTHON CONSOLE START DEBUG ===")
  
  ;; Check direnv status first
  (let ((project-dir "/Users/vwh7mb/projects/wander2"))
    (termint-org-src-debug-log 'info "Checking direnv status for: %s" project-dir)
    
    ;; Test direnv status manually
    (let ((default-directory project-dir))
      (with-temp-buffer
        (let ((exit-code (call-process "direnv" nil t nil "status")))
          (termint-org-src-debug-log 'info "Direnv status exit code: %d" exit-code)
          (termint-org-src-debug-log 'info "Direnv status output: %s" (buffer-string)))))
    
    ;; Test if direnv exec works
    (termint-org-src-debug-log 'info "Testing direnv exec command...")
    (let ((test-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . echo DIRENV_WORKS'"))
      (termint-org-src-debug-log 'info "Test command: %s" test-cmd)
      (with-temp-buffer
        (let ((exit-code (call-process "sh" nil t nil "-c" 
                                      "cd /Users/vwh7mb/projects/wander2 && direnv exec . echo DIRENV_WORKS")))
          (termint-org-src-debug-log 'info "Test command exit code: %d" exit-code)
          (termint-org-src-debug-log 'info "Test command output: %s" (buffer-string)))))
    
    ;; Kill any existing hung buffer first
    (when (get-buffer "*jupyter-python*")
      (termint-org-src-debug-log 'info "Killing existing hung *jupyter-python* buffer")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer "*jupyter-python*")))
    
    ;; Define and start with smart command
    (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel python3'"))
      (termint-org-src-debug-log 'info "Defining termint with command: %s" smart-cmd)
      (termint-define "jupyter-python" smart-cmd 
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
      (termint-org-src-debug-log 'info "Calling termint-jupyter-python-start...")
      (termint-jupyter-python-start)
      (termint-org-src-debug-log 'info "=== SMART PYTHON CONSOLE START DEBUG END ==="))))

(defun termint-org-src-smart-r-start ()
  "Start R jupyter console with smart direnv command."
  (interactive)
  (termint-org-src-debug-log 'info "Starting smart R console")
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-r*")
    (termint-org-src-debug-log 'info "Killing existing hung *jupyter-r* buffer")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-r*")))
  ;; Define and start with smart command
  (termint-define "jupyter-r" 
                  "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel ir'"
                  :bracketed-paste-p t
                  :backend 'eat
                  :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
  (termint-jupyter-r-start))

(defun termint-org-src-smart-stata-start ()
  "Start Stata jupyter console with smart direnv command for org-src integration."
  (interactive)
  (termint-org-src-debug-log 'info "Starting smart Stata console")
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-stata*")
    (termint-org-src-debug-log 'info "Killing existing hung *jupyter-stata* buffer")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-stata*")))
  
  ;; Start monitoring before console starts
  ;; Stata graph monitoring is now handled by euporie-termint.el
  
  ;; Define and start with smart command
  (termint-define "jupyter-stata" 
                  "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel stata'"
                  :bracketed-paste-p t
                  :backend 'eat
                  :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
  (termint-jupyter-stata-start))

(defun termint-org-src-smart-sas-start ()
  "Start SAS euporie console with smart remote connection for org-src integration."
  (interactive)
  (termint-org-src-debug-log 'info "Starting smart SAS console")
  ;; Kill any existing hung buffer first
  (when (get-buffer "*euporie-sas*")
    (termint-org-src-debug-log 'info "Killing existing hung *euporie-sas* buffer")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*euporie-sas*")))
  
  ;; For SAS, check if we need remote or local execution
  (let* ((babel-info (bound-and-true-p org-src--babel-info))
         (dir (when babel-info (cdr (assoc :dir (nth 2 babel-info))))))
    
    (sas-workflow-debug-log 'debug "SAS smart start - babel-info: %s" babel-info)
    (sas-workflow-debug-log 'debug "SAS smart start - dir: %s" dir)
    (sas-workflow-debug-log 'debug "SAS smart start - is remote: %s" (when dir (file-remote-p dir)))
    
    (if (and dir (file-remote-p dir))
        (progn
          (sas-workflow-debug-log 'info "Starting remote SAS euporie console")
          (termint-org-src-debug-log 'info "Starting remote SAS euporie console")
          (when (fboundp 'euporie-sas-start-remote)
            (sas-workflow-debug-log 'debug "Calling euporie-sas-start-remote with dir: %s" dir)
            (euporie-sas-start-remote dir))
          (unless (fboundp 'euporie-sas-start-remote)
            (sas-workflow-debug-log 'error "Function euporie-sas-start-remote not available")))
      (progn
        (sas-workflow-debug-log 'info "Starting local SAS euporie console")
        (termint-org-src-debug-log 'info "Starting local SAS euporie console")
        ;; Define and start with local command
        (let ((local-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run euporie console --kernel-name=sas --graphics=sixel'"))
          (sas-workflow-debug-log 'debug "Local SAS command: %s" local-cmd)
          (termint-define "euporie-sas" local-cmd
                          :bracketed-paste-p t
                          :backend 'eat
                          :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor")))))))

(defun termint-org-src-get-console-buffer-name (kernel)
  "Get the expected console buffer name for KERNEL."
  (pcase kernel
    ("python" "*jupyter-python*")
    ("r" "*jupyter-r*")
    ("stata" "*jupyter-stata*")
    ("sas" "*euporie-sas*")  ; SAS uses euporie system
    (_ (error "Unknown kernel: %s" kernel))))

(defun termint-org-src-get-start-function (kernel)
  "Get the start function for KERNEL."
  (pcase kernel
    ("python" 'termint-org-src-smart-python-start)
    ("r" 'termint-org-src-smart-r-start)
    ("stata" 'termint-org-src-smart-stata-start)
    ("sas" 'termint-org-src-smart-sas-start)
    (_ (error "Unknown kernel: %s" kernel))))

(defun termint-org-src-ensure-console (kernel)
  "Ensure console for KERNEL is running. Start it if needed and display in right split."
  (let ((buffer-name (termint-org-src-get-console-buffer-name kernel))
        (start-func (termint-org-src-get-start-function kernel)))
    
    (sas-workflow-debug-log 'info "Ensuring console for kernel: %s, buffer: %s" kernel buffer-name)
    (termint-org-src-debug-log 'info "Checking for console buffer: %s" buffer-name)
    
    ;; Check if console buffer exists and has a live process
    (let ((console-buffer (get-buffer buffer-name)))
      (if (and console-buffer 
               (buffer-live-p console-buffer)
               ;; For vterm buffers, check if the process is alive
               (or (not (eq major-mode 'vterm-mode))
                   (and (get-buffer-process console-buffer)
                        (process-live-p (get-buffer-process console-buffer)))))
          (progn
            (termint-org-src-debug-log 'info "Console already running: %s" buffer-name)
            ;; Console exists, just display it in right split
            (termint-org-src-display-console-right console-buffer))
        
        ;; Console doesn't exist or is dead, start it
        (progn
          (sas-workflow-debug-log 'info "Starting new console for %s kernel" kernel)
          (termint-org-src-debug-log 'info "Starting new console for %s kernel" kernel)
          (message "Starting %s console..." kernel)
          
          ;; Call the start function
          (condition-case err
              (progn
                (sas-workflow-debug-log 'debug "Calling start function: %s" start-func)
                (funcall start-func)
                (termint-org-src-debug-log 'info "Called start function: %s" start-func)
                
                ;; Wait for buffer to be created
                (let ((max-wait 10) (wait-count 0))
                  (while (and (< wait-count max-wait)
                             (not (get-buffer buffer-name)))
                    (sleep-for 0.5)
                    (setq wait-count (1+ wait-count)))
                  
                  (let ((new-buffer (get-buffer buffer-name)))
                    (if new-buffer
                        (progn
                          (sas-workflow-debug-log 'info "Console buffer created: %s" buffer-name)
                          (termint-org-src-debug-log 'info "Console buffer created: %s" buffer-name)
                          (message "%s console ready!" kernel)
                          ;; Display in right split
                          (sas-workflow-debug-log 'debug "Displaying console in right split")
                          (termint-org-src-display-console-right new-buffer)
                          ;; Give it a moment to fully initialize
                          (sleep-for 1))
                      (progn
                        (sas-workflow-debug-log 'error "Console buffer not created after %d seconds" max-wait)
                        (termint-org-src-debug-log 'error "Console buffer not created after %d seconds" max-wait)
                        (error "Failed to create %s console buffer" kernel))))))
            (error
             (sas-workflow-debug-log 'error "Failed to start console: %s" err)
             (termint-org-src-debug-log 'error "Failed to start console: %s" err)
             (error "Failed to start %s console: %s" kernel err))))))

(defun termint-org-src-display-console-right (buffer)
  "Display console BUFFER in a right split window, preserving current window focus."
  (termint-org-src-debug-log 'info "=== WINDOW MANAGEMENT DEBUG START ===")
  
  ;; Detailed logging of initial state
  (let ((initial-window (selected-window))
        (initial-buffer (current-buffer))
        (initial-buffer-name (buffer-name)))
    
    (termint-org-src-debug-log 'info "Before display-buffer:")
    (termint-org-src-debug-log 'info "  Selected window: %s" initial-window)
    (termint-org-src-debug-log 'info "  Current buffer: %s" initial-buffer-name)
    
    ;; Check if console buffer is already visible
    (let ((console-window (get-buffer-window buffer)))
      (if console-window
          ;; Console already visible, just ensure it's focused properly
          (progn
            (termint-org-src-debug-log 'info "Console already visible in window: %s" console-window)
            (with-selected-window console-window
              (goto-char (point-max)))
            ;; Keep focus on original window
            (select-window initial-window))
        
        ;; Console not visible, create split layout
        (progn
          (termint-org-src-debug-log 'info "Creating split window layout")
          
          ;; If only one window, split horizontally
          (when (= 1 (length (window-list)))
            (split-window-right)
            (termint-org-src-debug-log 'info "Split window created"))
          
          ;; Move to right window and display console buffer
          (other-window 1)
          (switch-to-buffer buffer)
          (setq console-window (selected-window))
          (goto-char (point-max))
          (termint-org-src-debug-log 'info "Console buffer displayed in right window")
          
          ;; Return to original window  
          (select-window initial-window)
          
          ;; Ensure original buffer is still displayed in left window
          (when (buffer-live-p initial-buffer)
            (set-window-buffer initial-window initial-buffer))
          
          (termint-org-src-debug-log 'info "Returned focus to original window"))))
    
    ;; Final state logging
    (let ((final-selected (selected-window))
          (final-buffer (buffer-name)))
      
      (termint-org-src-debug-log 'info "Final state:")
      (termint-org-src-debug-log 'info "  Selected window: %s" final-selected)
      (termint-org-src-debug-log 'info "  Current buffer: %s" final-buffer)
      
      (if (string= final-buffer initial-buffer-name)
          (termint-org-src-debug-log 'info "✓ Successfully preserved original buffer")
        (termint-org-src-debug-log 'error "✗ Failed to preserve original buffer: %s -> %s" initial-buffer-name final-buffer)))
    
    (termint-org-src-debug-log 'info "=== WINDOW MANAGEMENT DEBUG END ===")))

;;; Core Send Function

(defun termint-org-src-send-region (start end &optional kernel)
  "Send region from START to END to termint console.
KERNEL specifies which termint console to use (python, r, stata, sas).
Automatically starts console if it doesn't exist."
  (let* ((code (buffer-substring-no-properties start end))
         (trimmed-code (string-trim code))
         (kernel (or kernel (termint-org-src-detect-kernel))))
    
    (termint-org-src-debug-log 'info "Sending region [%d-%d] to %s kernel" start end kernel)
    (termint-org-src-debug-log 'debug "Code to send: %s" (if (> (length trimmed-code) 100) 
                                                           (concat (substring trimmed-code 0 100) "...")
                                                           trimmed-code))
    
    ;; Don't send empty code
    (when (and trimmed-code (not (string-empty-p trimmed-code)))
      ;; Ensure console is running first - this also creates the split window
      (termint-org-src-ensure-console kernel)
      
      ;; Now send the code
      (condition-case err
          (progn
            (termint-org-src-debug-log 'info "Console ready, sending code...")
            (pcase kernel
              ("python" 
               (termint-org-src-debug-log 'debug "Calling termint-jupyter-python-send-string")
               (termint-jupyter-python-send-string trimmed-code))
              ("r" 
               (termint-org-src-debug-log 'debug "Calling termint-jupyter-r-send-string")
               (termint-jupyter-r-send-string trimmed-code))
              ("stata" 
               (termint-org-src-debug-log 'debug "Calling termint-jupyter-stata-send-string")
               (termint-jupyter-stata-send-string trimmed-code))
              ("sas" 
               (sas-workflow-debug-log 'info "SAS code send requested - kernel: %s" kernel)
               (termint-org-src-debug-log 'debug "Using SAS euporie for SAS code")
               ;; For SAS, use the euporie system like other kernels
               (when (fboundp 'termint-euporie-sas-send-string)
                 (sas-workflow-debug-log 'debug "Calling termint-euporie-sas-send-string with code")
                 (termint-euporie-sas-send-string trimmed-code))
               (unless (fboundp 'termint-euporie-sas-send-string)
                 (sas-workflow-debug-log 'error "Function termint-euporie-sas-send-string not available")))
              (_ (error "Unknown kernel: %s" kernel)))
            (termint-org-src-debug-log 'info "Code sent successfully")
            ;; Verify console contents after sending
            (run-with-timer 1 nil (lambda () (termint-org-src-verify-console-contents kernel))))
        (error 
         (termint-org-src-debug-log 'error "Failed to send region: %s" err)
         (message "Error sending code to %s: %s" kernel err))))
    
    ;; Return the code that was sent for debugging
    trimmed-code))

;;; Main Interactive Function

(defun termint-org-src-send-line-or-paragraph-or-fun-or-region ()
  "Send region, function, paragraph, or current line to termint console.
Priority order: active region > function > paragraph > current line.
After sending, advance to the next code line."
  (interactive)
  (termint-org-src-debug-log 'info "Starting termint-org-src-send-line-or-paragraph-or-fun-or-region")
  
  ;; Validate we're in an appropriate context
  (unless (or (bound-and-true-p org-src-mode)
              (derived-mode-p 'python-mode 'python-ts-mode 'ess-r-mode 'ess-mode 'stata-mode 'SAS-mode 'ess-sas-mode))
    (termint-org-src-debug-log 'error "Not in a supported mode: %s" major-mode)
    (error "Not in org-src-mode or supported language mode"))
  
  (let ((kernel (condition-case err
                    (termint-org-src-detect-kernel)
                  (error 
                   (termint-org-src-debug-log 'error "Kernel detection failed: %s" err)
                   (error "Cannot detect language/kernel: %s" err))))
        (start-pos (point))
        (sent-something nil)
        (in-org-src-buffer (bound-and-true-p org-src-mode)))
    
    (termint-org-src-debug-log 'info "Using kernel: %s" kernel)
    
    (cond
     ;; Region is active - highest priority
     ((use-region-p)
      (termint-org-src-debug-log 'info "Using active region: %d-%d" (region-beginning) (region-end))
      (termint-org-src-send-region (region-beginning) (region-end) kernel)
      (goto-char (region-end))
      (setq sent-something t))
     
     ;; Try function bounds - second priority
     ((termint-org-src-get-function-bounds)
      (let ((bounds (termint-org-src-get-function-bounds)))
        (termint-org-src-debug-log 'info "Using function bounds: %d-%d" (car bounds) (cdr bounds))
        (termint-org-src-send-region (car bounds) (cdr bounds) kernel)
        (goto-char (cdr bounds))
        (setq sent-something t)))
     
     ;; Fall back to paragraph - third priority
     (t
      (let ((bounds (termint-org-src-get-paragraph-bounds)))
        ;; If paragraph is too small (like single line), use current line instead
        (if (< (- (cdr bounds) (car bounds)) 20)
            (progn
              (termint-org-src-debug-log 'info "Using current line: %d-%d" (line-beginning-position) (line-end-position))
              (termint-org-src-send-region (line-beginning-position) (line-end-position) kernel)
              (goto-char (line-end-position)))
          (progn
            (termint-org-src-debug-log 'info "Using paragraph bounds: %d-%d" (car bounds) (cdr bounds))
            (termint-org-src-send-region (car bounds) (cdr bounds) kernel)
            (goto-char (cdr bounds))))
        (setq sent-something t))))
    
    ;; Move to next code line if we sent something
    (when sent-something
      ;; If we're in an org-src edit buffer, ensure split window layout
      (when in-org-src-buffer
        (termint-org-src-debug-log 'info "Creating split window layout for org-src buffer")
        (let ((console-buffer (get-buffer (termint-org-src-get-console-buffer-name kernel))))
          (when console-buffer
            ;; Ensure console is displayed in right split
            (termint-org-src-display-console-right console-buffer))))
      
      (termint-org-src-next-code-line)
      (message "Code sent to %s console" kernel)))))

;;; Keybinding Setup

(defun termint-org-src-setup-keybindings ()
  "Set up C-RET keybinding to override Doom defaults in org-src mode."
  (termint-org-src-debug-log 'info "Setting up keybindings for org-src mode")
  
  ;; Set up the hook to be called when entering org-src-mode
  (add-hook 'org-src-mode-hook 'termint-org-src-setup-buffer-keybindings)
  
  ;; Also set up evil-mode overrides if available
  (when (fboundp 'evil-define-key)
    (with-eval-after-load 'evil
      (add-hook 'org-src-mode-hook 'termint-org-src-setup-evil-keybindings))))

(defun termint-org-src-setup-buffer-keybindings ()
  "Set up keybindings for the current org-src buffer."
  (termint-org-src-debug-log 'info "Setting up buffer-local keybindings")
  
  ;; Override C-RET to use our function instead of --INSERT-- or sas-console
  (local-set-key (kbd "C-<return>") #'termint-org-src-send-line-or-paragraph-or-fun-or-region)
  
  ;; Log what we overrode
  (termint-org-src-debug-log 'info "Overrode C-<return> in org-src buffer (language: %s)" 
                             (if (boundp 'org-src--babel-type) org-src--babel-type "unknown")))

(defun termint-org-src-setup-evil-keybindings ()
  "Set up evil-mode keybindings for org-src mode."
  (when (and (fboundp 'evil-local-set-key) (boundp 'evil-mode) evil-mode)
    (termint-org-src-debug-log 'info "Setting up evil keybindings")
    
    ;; Override in all evil states to ensure it takes precedence
    (evil-local-set-key 'normal (kbd "C-<return>") #'termint-org-src-send-line-or-paragraph-or-fun-or-region)
    (evil-local-set-key 'insert (kbd "C-<return>") #'termint-org-src-send-line-or-paragraph-or-fun-or-region)
    (evil-local-set-key 'visual (kbd "C-<return>") #'termint-org-src-send-line-or-paragraph-or-fun-or-region)
    (evil-local-set-key 'emacs (kbd "C-<return>") #'termint-org-src-send-line-or-paragraph-or-fun-or-region)
    
    ;; This should override the --INSERT-- behavior
    (termint-org-src-debug-log 'info "Evil keybindings set up to override --INSERT-- behavior")))

;;; Console Verification Functions

(defun termint-org-src-verify-console-contents (kernel)
  "Verify the contents of the console buffer for KERNEL and display recent output."
  (let ((buffer-name (termint-org-src-get-console-buffer-name kernel)))
    (termint-org-src-debug-log 'info "Verifying console contents for %s" kernel)
    
    (let ((console-buffer (get-buffer buffer-name)))
      (if console-buffer
          (with-current-buffer console-buffer
            ;; Get the last 500 characters of the buffer to see recent output
            (let* ((buffer-end (point-max))
                   (buffer-start (max 1 (- buffer-end 500)))
                   (recent-output (buffer-substring-no-properties buffer-start buffer-end)))
              (termint-org-src-debug-log 'info "Console buffer exists, size: %d chars" (buffer-size))
              (termint-org-src-debug-log 'info "Recent output (last 500 chars): %s" recent-output)
              (message "✓ Console %s active, recent output logged" kernel)
              recent-output))
        (progn
          (termint-org-src-debug-log 'error "Console buffer %s not found" buffer-name)
          (message "✗ Console buffer %s not found" buffer-name)
          nil)))))

;;; Initialization and Testing Functions

(defun termint-org-src-interactive-test ()
  "Interactive test function to debug the real issues the user sees."
  (interactive)
  (termint-org-src-debug-log 'info "=== INTERACTIVE TEST STARTED ===")
  (message "Starting interactive test - check debug log for details...")
  
  ;; Clear existing console
  (when (get-buffer "*jupyter-python*")
    (termint-org-src-debug-log 'info "Killing existing console for clean test")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-python*")))
  
  ;; Test console startup with detailed logging
  (termint-org-src-debug-log 'info "Testing console startup...")
  (termint-org-src-smart-python-start)
  
  ;; Wait for startup
  (sleep-for 3)
  
  ;; Check if console is ready
  (let ((console-buffer (get-buffer "*jupyter-python*")))
    (if console-buffer
        (progn
          (termint-org-src-debug-log 'info "Console buffer created successfully")
          (with-current-buffer console-buffer
            (termint-org-src-debug-log 'info "Console buffer size: %d chars" (buffer-size))
            (if (> (buffer-size) 100)
                (termint-org-src-debug-log 'info "✓ Console appears to be running")
              (termint-org-src-debug-log 'error "✗ Console buffer empty - likely permission prompt"))))
      (termint-org-src-debug-log 'error "✗ No console buffer created")))
  
  ;; Test window management
  (termint-org-src-debug-log 'info "Testing window management...")
  (let ((org-src-buffers (seq-filter (lambda (buf) (string-match-p "org-src" (buffer-name buf))) (buffer-list))))
    (if org-src-buffers
        (progn
          (switch-to-buffer (car org-src-buffers))
          (termint-org-src-debug-log 'info "Switched to org-src buffer: %s" (buffer-name))
          
          ;; Test display-console-right
          (let ((console-buffer (get-buffer "*jupyter-python*")))
            (when console-buffer
              (termint-org-src-debug-log 'info "Testing display-console-right...")
              (termint-org-src-display-console-right console-buffer))))
      (termint-org-src-debug-log 'error "No org-src buffer found for testing")))
  
  (message "Interactive test completed - check debug log and current window state")
  (termint-org-src-debug-log 'info "=== INTERACTIVE TEST COMPLETED ==="))

(defun termint-org-src-test-setup ()
  "Test function to verify termint org-src setup is working."
  (interactive)
  (termint-org-src-debug-log 'info "=== TESTING TERMINT ORG-SRC SETUP ===")
  
  ;; Check if we're in org-src-mode
  (let ((in-org-src (bound-and-true-p org-src-mode))
        (lang (if (boundp 'org-src--babel-type) org-src--babel-type "none"))
        (current-mode major-mode))
    
    (message "=== Termint Org-Src Test Results ===")
    (message "In org-src-mode: %s" in-org-src)
    (message "Language: %s" lang)
    (message "Major mode: %s" current-mode)
    
    ;; Test kernel detection
    (condition-case err
        (let ((kernel (termint-org-src-detect-kernel)))
          (message "Detected kernel: %s" kernel)
          (termint-org-src-debug-log 'info "Kernel detection test passed: %s" kernel))
      (error 
       (message "Kernel detection failed: %s" err)
       (termint-org-src-debug-log 'error "Kernel detection test failed: %s" err)))
    
    ;; Test keybinding
    (let ((c-ret-binding (local-key-binding (kbd "C-<return>"))))
      (message "C-<return> bound to: %s" c-ret-binding)
      (termint-org-src-debug-log 'info "C-<return> binding: %s" c-ret-binding))
    
    ;; Test termint function availability
    (let ((python-send (fboundp 'termint-jupyter-python-send-region))
          (r-send (fboundp 'termint-jupyter-r-send-region))
          (stata-send (fboundp 'termint-jupyter-stata-send-region)))
      (message "Termint functions available:")
      (message "  Python: %s" python-send)
      (message "  R: %s" r-send) 
      (message "  Stata: %s" stata-send)
      (termint-org-src-debug-log 'info "Function availability - Python: %s, R: %s, Stata: %s" python-send r-send stata-send))
    
    (message "=== End Test ===")
    (termint-org-src-debug-log 'info "=== TEST COMPLETED ===")))))

;; Initialize logging - disabled to avoid loading issues
;; (when (fboundp 'termint-org-src-clear-log)
;;   (termint-org-src-clear-log))
;; (when (fboundp 'termint-org-src-debug-log) 
;;   (termint-org-src-debug-log 'info "termint-org-src.el loaded"))

;; Set up keybindings - disabled to avoid loading issues
;; (when (fboundp 'termint-org-src-setup-keybindings)
;;   (termint-org-src-setup-keybindings))

(provide 'termint-org-src)
;;; termint-org-src.el ends here