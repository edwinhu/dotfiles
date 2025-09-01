;;; euporie-termint.el --- Unified Euporie console integration -*- lexical-binding: t; -*-

;;; Commentary:
;; STREAMLINED ARCHITECTURE: Unified euporie console integration using termint.el with eat backend.
;; 
;; UNIVERSAL FUNCTIONS supporting Python, R, Stata, and SAS:
;;   - euporie-termint-send-code(kernel, code, &optional dir)
;;   - euporie-termint-get-or-create-buffer(kernel, &optional dir)
;;   - euporie-termint-display-console-right(buffer)
;;   - euporie-termint-detect-kernel()
;;   - euporie-termint-send-region-or-line()
;;
;; FEATURES:
;;   - IDENTICAL workflows for all languages
;;   - Local/Remote routing: Automatic via (file-remote-p dir) for SAS
;;   - Graphics: PRESERVED native euporie sixel graphics for Python/R/Stata
;;   - Keybinding: Single setup for ALL languages
;;   - Logging: Comprehensive file-based debug logging
;;
;; CRITICAL: This preserves existing native euporie sixel graphics for Python/R/Stata.
;; DO NOT break working functionality while streamlining architecture.

;;; Code:

(require 'termint nil t)
(load (expand-file-name "tramp-qrsh.el" (or (bound-and-true-p doom-user-dir) "~/.doom.d/")))
(require 'org)
(require 'ob)

(defgroup euporie-termint nil
  "Euporie console integration using termint and eat."
  :group 'org-babel)

(defcustom euporie-termint-graphics-protocol "sixel"
  "Graphics protocol for euporie console.
Supported protocols: sixel, kitty, kitty-unicode, iterm."
  :type '(choice (const "sixel")
                 (const "kitty") 
                 (const "kitty-unicode")
                 (const "iterm"))
  :group 'euporie-termint)

(defcustom euporie-termint-stata-graphics-protocol "kitty"
  "Graphics protocol specifically for Stata euporie console.
Some protocols may work better with stata_kernel than others."
  :type '(choice (const "sixel")
                 (const "kitty") 
                 (const "kitty-unicode")
                 (const "iterm"))
  :group 'euporie-termint)

(defcustom euporie-termint-project-dir "/Users/vwh7mb/projects/emacs-euporie"
  "Default project directory containing pixi environment."
  :type 'directory
  :group 'euporie-termint)

(defvar euporie-termint-debug-log-file (expand-file-name "euporie-debug.log" "~/")
  "Log file for euporie debugging information.")
(defvar sas-workflow-debug-log-file (expand-file-name "sas-workflow-debug.log" "~/")
  "Log file for SAS workflow debugging information.")

(defun sas-workflow-debug-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to SAS workflow debug file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) sas-workflow-debug-log-file))))

(defun euporie-termint-debug-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to debug file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-termint-debug-log-file))))

;;; Environment Detection

(defun euporie-termint--check-direnv-allowed (directory)
  "Check if direnv is already allowed for DIRECTORY."
  (when (and (boundp 'envrc-direnv-executable) envrc-direnv-executable)
    (let* ((default-directory directory)
           (status-output (with-temp-buffer
                           (when (zerop (call-process envrc-direnv-executable nil t nil "status"))
                             (buffer-string)))))
      (and status-output
           (string-match-p "Found RC allowPath" status-output)))))

(defun euporie-termint--build-euporie-command (kernel project-dir)
  "Build euporie-console command for KERNEL in PROJECT-DIR."
  (let* ((graphics-protocol (if (string= kernel "stata") 
                               euporie-termint-stata-graphics-protocol
                             euporie-termint-graphics-protocol))
         (base-cmd (format "pixi run euporie-console --graphics=%s --kernel-name=%s" 
                          graphics-protocol kernel))
         (is-remote (file-remote-p project-dir)))
    
    (if is-remote
        ;; For remote execution, don't wrap with shell cd - TRAMP handles directory context
        base-cmd
      ;; For local execution, use existing logic
      (if (euporie-termint--check-direnv-allowed project-dir)
          (format "sh -c 'cd %s && direnv exec . %s'" project-dir base-cmd)
        (format "sh -c 'cd %s && %s'" project-dir base-cmd)))))

;;; Kernel Management Functions

(defun euporie-python-start ()
  "Start Python euporie console with direnv handling."
  (interactive)
  (let* ((buffer-name "*euporie-python*")
         (smart-cmd (euporie-termint--build-euporie-command "python3" euporie-termint-project-dir)))
    
    (euporie-termint-debug-log 'info "Starting Python euporie console...")
    (euporie-termint-debug-log 'info "Python command: %s" smart-cmd)
    
    ;; Kill any existing buffer first
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))
    
    ;; Define termint with proper environment for graphics
    (termint-define "euporie-python" smart-cmd
                    :bracketed-paste-p t
                    :backend 'eat
                    :env '(("TERM" . "eat-truecolor") 
                           ("COLORTERM" . "truecolor")
                           ("EUPORIE_GRAPHICS" . "sixel")))
    ;; Start the termint session with error handling
    (condition-case err
        (termint-euporie-python-start)
      (error 
       (euporie-termint-debug-log 'error "Failed to start Python termint: %s" err)
       (message "Warning: Failed to start Python euporie console: %s" err)))
    
    ;; Then wait a short time for kernel to initialize (simplified approach)
    (when (get-buffer buffer-name)
      (sleep-for 2)  ; Simple 2-second wait instead of complex detection
      (euporie-termint-debug-log 'info "Kernel initialization wait complete"))))

(defun euporie-r-start ()
  "Start R euporie console with direnv handling."
  (interactive)
  (let* ((buffer-name "*euporie-r*")
         (smart-cmd (euporie-termint--build-euporie-command "ir" euporie-termint-project-dir)))
    
    (euporie-termint-debug-log 'info "Starting R euporie console...")
    (euporie-termint-debug-log 'info "R command: %s" smart-cmd)
    
    ;; Kill any existing buffer first
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))
    
    ;; Define termint with proper environment for graphics
    (termint-define "euporie-r" smart-cmd
                    :bracketed-paste-p t
                    :backend 'eat
                    :env '(("TERM" . "eat-truecolor") 
                           ("COLORTERM" . "truecolor")
                           ("EUPORIE_GRAPHICS" . "sixel")))
    ;; Start the termint session with error handling
    (condition-case err
        (termint-euporie-r-start)
      (error 
       (euporie-termint-debug-log 'error "Failed to start R termint: %s" err)
       (message "Warning: Failed to start R euporie console: %s" err)))
    
    ;; Then wait a short time for kernel to initialize (simplified approach)
    (when (get-buffer buffer-name)
      (sleep-for 2)  ; Simple 2-second wait instead of complex detection
      (euporie-termint-debug-log 'info "Kernel initialization wait complete"))))

(defun euporie-stata-start ()
  "Start Stata euporie console with direnv handling and Stata-specific optimizations."
  (interactive)
  (let* ((buffer-name "*euporie-stata*")
         (smart-cmd (euporie-termint--build-euporie-command "stata" euporie-termint-project-dir)))
    
    (euporie-termint-debug-log 'info "Starting Stata euporie console with optimizations...")
    (euporie-termint-debug-log 'info "Stata command: %s" smart-cmd)
    
    ;; Kill any existing buffer first
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))
    
    ;; Define termint with Stata-specific environment optimizations
    (termint-define "euporie-stata" smart-cmd
                    :bracketed-paste-p t
                    :backend 'eat
                    :env '(("TERM" . "eat-truecolor")           ; Use eat's native term with graphics support
                           ("COLORTERM" . "truecolor")
                           ("EUPORIE_GRAPHICS" . "sixel")       ; Use sixel protocol for consistency
                           ("LANG" . "en_US.UTF-8")            ; Explicit locale for Unicode
                           ("LC_ALL" . "en_US.UTF-8")))        ; Full locale support
    ;; Start the termint session with error handling
    (condition-case err
        (termint-euporie-stata-start)
      (error 
       (euporie-termint-debug-log 'error "Failed to start Stata termint: %s" err)
       (message "Warning: Failed to start Stata euporie console: %s" err)))
    
    ;; Set up output monitoring for automatic graphics
    ;; No output monitoring needed - euporie handles graphics natively
    
    ;; Also add a timer-based file watcher as backup
    (run-with-timer 2 1 'euporie-termint-check-for-new-stata-graphs)
    
    ;; Then wait a short time for kernel to initialize (simplified approach)
    (when (get-buffer buffer-name)
      (sleep-for 2)  ; Simple 2-second wait instead of complex detection
      (euporie-termint-debug-log 'info "Kernel initialization wait complete"))))

(defun euporie-sas-start (&optional dir)
  "Start SAS euporie console with direnv handling.
If DIR is provided and is a TRAMP path, start remote SAS session.
Otherwise start local SAS session."
  (interactive)
  (let* ((is-remote (and dir (file-remote-p dir)))
         (buffer-name "*euporie-sas*"))
    
    (sas-workflow-debug-log 'info "=== euporie-sas-start called with dir: %s ===" (or dir "nil"))
    (sas-workflow-debug-log 'debug "SAS start - is-remote: %s, buffer-name: %s" is-remote buffer-name)
    (euporie-termint-debug-log 'info "Starting %s SAS euporie console in directory: %s" 
                               (if is-remote "remote" "local") (or dir default-directory))
    
    ;; Kill any existing buffer first
    (when (get-buffer buffer-name)
      (sas-workflow-debug-log 'info "Killing existing buffer: %s" buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))
    
    (if is-remote
        (progn
          (sas-workflow-debug-log 'info "Calling euporie-sas-start-remote with dir: %s" dir)
          (euporie-sas-start-remote dir))
      (progn
        (sas-workflow-debug-log 'info "Calling euporie-sas-start-local")
        (euporie-sas-start-local)))
    
    ;; Return buffer for further use
    (let ((final-buffer (get-buffer buffer-name)))
      (sas-workflow-debug-log 'info "SAS startup complete - buffer: %s" 
                             (if final-buffer (buffer-name final-buffer) "nil"))
      (euporie-termint-debug-log 'info "SAS startup complete - buffer: %s" 
                                 (if final-buffer (buffer-name final-buffer) "nil"))
      final-buffer)))

(defun euporie-sas-start-local ()
  "Start local SAS euporie console using termint."
  (let ((smart-cmd (euporie-termint--build-euporie-command "sas" euporie-termint-project-dir)))
    
    (euporie-termint-debug-log 'info "Local SAS command: %s" smart-cmd)
    
    ;; Define termint for local execution
    (termint-define "euporie-sas" smart-cmd
                    :bracketed-paste-p t
                    :backend 'eat
                    :env '(("TERM" . "eat-truecolor") 
                           ("COLORTERM" . "truecolor")
                           ("EUPORIE_GRAPHICS" . "sixel")))
    
    ;; Start the termint session
    (condition-case err
        (progn 
          (termint-euporie-sas-start)
          ;; Display in split window using proper console display function
          (when-let ((buffer (get-buffer "*euporie-sas*")))
            (euporie-termint-display-console-right buffer)))
      (error 
       (euporie-termint-debug-log 'error "Failed to start local SAS termint: %s" err)
       (message "Warning: Failed to start local SAS euporie console: %s" err)))))

(defun euporie-sas-start-remote (remote-dir)
  "Start remote SAS euporie console by using working tramp-wrds-termint and then sending euporie command."
  (let* ((localname (if (file-remote-p remote-dir)
                        (file-remote-p remote-dir 'localname)
                      remote-dir)))
    
    (sas-workflow-debug-log 'info "=== euporie-sas-start-remote called with dir: %s ===" remote-dir)
    (sas-workflow-debug-log 'debug "Remote SAS start - localname extracted: %s" localname)
    (euporie-termint-debug-log 'info "Remote SAS start - using tramp-wrds-termint + euporie command for: %s" remote-dir)
    
    ;; Use the working tramp-wrds-termint to get to compute node
    (sas-workflow-debug-log 'info "Calling tramp-wrds-termint to establish remote connection")
    (let ((wrds-buffer (tramp-wrds-termint)))
      
      (if wrds-buffer
          (progn
            (sas-workflow-debug-log 'info "tramp-wrds-termint returned buffer: %s" (buffer-name wrds-buffer))
            ;; Keep original buffer name - termint expects this for send functions
            (sas-workflow-debug-log 'debug "Using original buffer name: %s" (buffer-name wrds-buffer)))
        (sas-workflow-debug-log 'error "tramp-wrds-termint returned nil - no remote connection established"))
      
      (when wrds-buffer
        
        ;; Send euporie command to the compute node shell with suppressed output
        (let ((euporie-cmd (format "cd %s 2>/dev/null && export PATH=/home/nyu/eddyhu/env/bin:$PATH >/dev/null 2>&1 && clear && exec euporie console --kernel-name=sas --graphics=sixel" localname)))
          (sas-workflow-debug-log 'info "Preparing euporie command: %s" euporie-cmd)
          (with-current-buffer wrds-buffer
            ;; Use termint send function (not comint)
            (sas-workflow-debug-log 'debug "Sending euporie command to compute node via termint")
            (if (fboundp 'termint-wrds-qrsh-send-string)
                (termint-wrds-qrsh-send-string euporie-cmd)
              (error "termint-wrds-qrsh-send-string not available"))
            (sas-workflow-debug-log 'info "Successfully sent euporie command to compute node")
            (euporie-termint-debug-log 'info "Sent euporie command to compute node: %s" euporie-cmd)))
        
        ;; Display in split window using proper console display function
        (sas-workflow-debug-log 'debug "Creating split window layout for remote SAS console")
        (euporie-termint-display-console-right wrds-buffer)
        
        (sas-workflow-debug-log 'info "Remote SAS setup complete - buffer: %s" (buffer-name wrds-buffer))
        (euporie-termint-debug-log 'info "Remote SAS setup complete - buffer: %s" (buffer-name wrds-buffer))
        wrds-buffer))))


;;; Buffer Management

(defun euporie-termint-get-or-create-buffer (kernel &optional dir)
  "Get or create euporie termint buffer for KERNEL.
DIR parameter is used for SAS to determine local vs remote execution."
  (let* ((is-remote-sas (and (string= kernel "sas") dir (file-remote-p dir)))
         (buffer-name (format "*euporie-%s*" kernel))
         (start-func (cond
                     ((string= kernel "python") #'euporie-python-start)
                     ((string= kernel "r") #'euporie-r-start)
                     ((string= kernel "stata") #'euporie-stata-start)
                     ((string= kernel "sas") (lambda () (euporie-sas-start dir)))
                     (t (error "Unsupported kernel: %s" kernel))))
         (buffer (get-buffer buffer-name)))
    
    ;; Check if buffer exists and has live process
    (if (and buffer 
             (buffer-live-p buffer)
             (get-buffer-process buffer)
             (process-live-p (get-buffer-process buffer)))
        buffer
      ;; Need to start new console
      (progn
        (funcall start-func)
        ;; For remote SAS, use special synchronous handling
        (if is-remote-sas
            (progn
              (euporie-termint-debug-log 'info "Remote SAS detected - using synchronous startup")
              ;; The remote function handles all timing internally
              (get-buffer buffer-name))
          ;; For local/non-SAS, use standard async pattern
          (progn
            ;; Wait for buffer creation and kernel readiness  
            (let ((max-wait-buffer 20) (max-wait-kernel 8) (wait-count 0) (buffer nil))
              ;; First, wait for buffer creation (up to 10 seconds)
              (while (and (< wait-count max-wait-buffer)
                         (not (setq buffer (get-buffer buffer-name))))
                (sleep-for 0.5)
                (setq wait-count (1+ wait-count)))
              
              ;; Then wait a short time for kernel to initialize (simplified approach)
              (when buffer
                (sleep-for 2)  ; Simple 2-second wait instead of complex detection
                (euporie-termint-debug-log 'info "Kernel initialization wait complete")))
            (get-buffer buffer-name))))))

;;; Language Detection

(defun euporie-termint-detect-kernel ()
  "Detect kernel from org-src buffer language or org-mode code block."
  (let ((lang-from-buffer-name (when (string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" (buffer-name))
                                 (match-string 1 (buffer-name))))
        (lang-from-variable (bound-and-true-p org-src--lang))
        (lang-from-org-element (when (eq major-mode 'org-mode)
                                 (org-element-property :language (org-element-at-point)))))
    
    (let ((detected-lang (or lang-from-variable lang-from-buffer-name lang-from-org-element)))
      (cond
       ((or (and detected-lang (or (string-equal detected-lang "r") (string-equal detected-lang "R"))) 
            (eq major-mode 'ess-r-mode)) 
        "r")
       ((or (and detected-lang (string-equal detected-lang "python")) 
            (eq major-mode 'python-mode) 
            (eq major-mode 'python-ts-mode)) 
        "python")
       ((or (and detected-lang (string-equal detected-lang "stata")) 
            (eq major-mode 'stata-mode)) 
        "stata")
       ((or (and detected-lang (or (string-equal detected-lang "sas") (string-equal detected-lang "SAS"))) 
            (eq major-mode 'SAS-mode)) 
        "sas")
       (t "python")))))

;;; Console Display Management

(defun euporie-termint-display-console-right (buffer &optional original-buffer original-window)
  "Display console BUFFER in a right split window."
  (let ((initial-window (or original-window (selected-window)))
        (initial-buffer (or original-buffer (current-buffer))))
    
    ;; Display buffer in right side window
    (let ((console-window (display-buffer buffer
                                          '((display-buffer-reuse-window
                                             display-buffer-in-side-window)
                                            (side . right)
                                            (window-width . 0.5)
                                            (inhibit-same-window . t)))))
      
      (when console-window
        ;; Scroll to bottom in console
        (with-selected-window console-window
          (goto-char (point-max)))
        
        ;; Restore focus to original window
        (when (window-live-p initial-window)
          (select-window initial-window))
        
        (when (buffer-live-p initial-buffer)
          (set-window-buffer (selected-window) initial-buffer))))))

;;; Graphics File Monitor for Stata

(defvar euporie-termint-stata-file-watcher nil
  "File watcher process for Stata graphics cache directory.")

(defvar euporie-termint-last-stata-graph-count 0
  "Track the last graph count to detect new graphs.")

(defun euporie-termint-check-for-new-stata-graphs ()
  "Timer-based function to check for new Stata graph files."
  (when (and (get-buffer "*euporie-stata*")
             (get-buffer-process "*euporie-stata*"))
    (let* ((cache-dir "~/.stata_kernel_cache")
           (graph-files (directory-files cache-dir nil "graph[0-9]+\\.png$"))
           (current-count (length graph-files)))
      
      (when (> current-count euporie-termint-last-stata-graph-count)
        (euporie-termint-debug-log 'info "New graph file detected - count: %d (was %d)" 
                                   current-count euporie-termint-last-stata-graph-count)
        
        ;; Find the newest graph file
        (let* ((newest-file (car (sort graph-files 
                                      (lambda (a b)
                                        (file-newer-than-file-p 
                                         (expand-file-name a cache-dir)
                                         (expand-file-name b cache-dir))))))
               (full-path (expand-file-name newest-file cache-dir)))
          
          ;; Keep file detection for MIME system coordination
          (when (and newest-file (file-exists-p full-path))
            (euporie-termint-debug-log 'info "Timer detected new graph: %s" full-path)))
        
        (setq euporie-termint-last-stata-graph-count current-count)))))

(defvar euporie-termint-stata-last-graph-file nil
  "Last processed graph file to avoid duplicates.")

(defun euporie-termint-start-stata-file-monitor ()
  "Start monitoring Stata cache directory for new PNG files."
  (when euporie-termint-stata-file-watcher
    (delete-process euporie-termint-stata-file-watcher))
  
  (let ((cache-dir (expand-file-name "~/.stata_kernel_cache")))
    (when (file-directory-p cache-dir)
      (euporie-termint-debug-log 'info "Starting file monitor for Stata graphics in: %s" cache-dir)
      
      ;; Use fswatch to monitor for new files (if available) or fallback to polling
      (if (executable-find "fswatch")
          (progn
            (setq euporie-termint-stata-file-watcher
                  (start-process "stata-file-monitor" nil "fswatch" "-o" cache-dir))
            (set-process-filter euporie-termint-stata-file-watcher #'euporie-termint-stata-file-event-handler)
            (euporie-termint-debug-log 'info "File monitor started with fswatch"))
        ;; Fallback: use a timer for polling
        (progn
          (setq euporie-termint-stata-file-watcher
                (run-with-timer 1 1 #'euporie-termint-check-new-stata-graphs))
          (euporie-termint-debug-log 'info "File monitor started with timer polling (fswatch not available)"))))))

(defun euporie-termint-stata-file-event-handler (process output)
  "Handle file system events for Stata graphics directory."
  (when (and output (> (length (string-trim output)) 0))
    (euporie-termint-check-new-stata-graphs)))

(defun euporie-termint-check-new-stata-graphs ()
  "Check for new graph files in Stata cache directory and display them."
  (let* ((cache-dir (expand-file-name "~/.stata_kernel_cache"))
         (png-files (when (file-directory-p cache-dir)
                     (directory-files cache-dir t "\\.png$")))
         (newest-file (when png-files
                       (car (sort png-files (lambda (a b)
                                             (time-less-p (nth 5 (file-attributes b))
                                                         (nth 5 (file-attributes a)))))))))
    
    (when (and newest-file
               (not (string= newest-file euporie-termint-stata-last-graph-file))
               (get-buffer "*euporie-stata*"))  ; Only if Stata buffer is active
      
      (setq euporie-termint-stata-last-graph-file newest-file)
      (euporie-termint-debug-log 'info "New Stata graph detected: %s" newest-file)
      ;; Graphics handled by euporie natively - no manual display needed
      )))

;;; Code Execution

(defun euporie-termint-send-code (kernel code &optional dir)
  "Send CODE to euporie console for KERNEL.
DIR parameter is used for SAS to determine local vs remote execution."
  (let* ((is-remote-sas (and (string= kernel "sas") dir (file-remote-p dir)))
         (buffer (euporie-termint-get-or-create-buffer kernel dir))
         (send-func (cond
                    ((string= kernel "python") #'termint-euporie-python-send-string)
                    ((string= kernel "r") #'termint-euporie-r-send-string)
                    ((string= kernel "stata") #'termint-euporie-stata-send-string)
                    ((and (string= kernel "sas") is-remote-sas) 
                     #'termint-wrds-qrsh-send-string)  ; Use WRDS-specific send function
                    ((string= kernel "sas") #'termint-euporie-sas-send-string)
                    (t (error "Unsupported kernel: %s" kernel)))))
    
    (sas-workflow-debug-log 'info "=== euporie-termint-send-code called ===")
    (sas-workflow-debug-log 'debug "Send code - kernel: %s, dir: %s, is-remote-sas: %s" kernel (or dir "nil") is-remote-sas)
    (sas-workflow-debug-log 'debug "Send code - buffer: %s, send-func: %s" 
                           (if buffer (buffer-name buffer) "nil") send-func)
    
    (when buffer
      (sas-workflow-debug-log 'info "Sending %s code: %s" kernel (substring code 0 (min 50 (length code))))
      (euporie-termint-debug-log 'info "Sending %s code: %s" kernel (substring code 0 (min 50 (length code))))
      
      ;; Display console in right window
      (euporie-termint-display-console-right buffer)
      
      ;; Send code to console with proper targeting
      (if is-remote-sas
          ;; For remote SAS, send directly to the *euporie-sas* buffer
          (progn
            (sas-workflow-debug-log 'debug "Using remote SAS send - targeting buffer directly")
            (with-current-buffer buffer
              (sas-workflow-debug-log 'debug "Calling send function: %s with code" send-func)
              (funcall send-func code)))
        ;; For other kernels, use standard approach
        (progn
          (when (string= kernel "sas")
            (sas-workflow-debug-log 'debug "Using local SAS send - standard approach"))
          (funcall send-func code)))
      
      ;; Start file monitoring for Stata graphics if not already running
      (when (string= kernel "stata")
        (unless (or (and euporie-termint-stata-file-watcher 
                        (or (process-live-p euporie-termint-stata-file-watcher)
                            (timerp euporie-termint-stata-file-watcher))))
          (run-with-timer 0.5 nil #'euporie-termint-start-stata-file-monitor)))
      
      ;; Return buffer for further processing if needed
      buffer)))

;;; Main C-RET Integration Function

(defun euporie-termint-send-region-or-line ()
  "Send current region or line to euporie console with automatic kernel detection."
  (interactive)
  
  (let* ((kernel (euporie-termint-detect-kernel))
         ;; Detect TRAMP path for remote execution (SAS-specific for now)
         (current-dir (or (and (string= kernel "sas") 
                              (file-remote-p default-directory))
                         default-directory))
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
    
    (when code
      (euporie-termint-debug-log 'info "Executing %s code via euporie in dir: %s" kernel current-dir)
      (if (and (string= kernel "sas") (file-remote-p current-dir))
          (euporie-termint-send-code kernel code current-dir)
        (euporie-termint-send-code kernel code)))))

;;; Org-babel Integration (Optional)

(defun org-babel-execute:python (body params)
  "Execute Python BODY with PARAMS using euporie."
  (let ((buffer (euporie-termint-get-or-create-buffer "python")))
    (when buffer
      (euporie-termint-send-code "python" body)
      ;; For org-babel, we don't return output as euporie handles display
      "")))

(defun org-babel-execute:R (body params)
  "Execute R BODY with PARAMS using euporie."
  (let ((buffer (euporie-termint-get-or-create-buffer "r")))
    (when buffer
      (euporie-termint-send-code "r" body)
      ;; For org-babel, we don't return output as euporie handles display
      "")))

(defun org-babel-execute:stata (body params)
  "Execute Stata BODY with PARAMS using euporie."
  (let ((buffer (euporie-termint-get-or-create-buffer "stata")))
    (when buffer
      (euporie-termint-send-code "stata" body)
      ;; For org-babel, we don't return output as euporie handles display
      "")))

(defun org-babel-execute:sas (body params)
  "Execute SAS BODY with PARAMS using euporie.
Supports remote execution via :dir parameter."
  (let ((dir (cdr (assoc :dir params))))
    (euporie-termint-debug-log 'info "SAS execution requested with dir: %s" dir)
    (euporie-termint-send-code "sas" body dir)
    ;; For org-babel, we don't return output as euporie handles display
    ""))

;;; Buffer keybinding setup helper function

(defun euporie-termint-setup-buffer-keybinding ()
  "Set up C-RET keybinding for the current org-src buffer."
  (when (string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" (buffer-name))
    
    ;; Unbind in all evil states for this buffer
    (evil-local-set-key 'insert (kbd "C-<return>") nil)
    (evil-local-set-key 'normal (kbd "C-<return>") nil)
    (evil-local-set-key 'visual (kbd "C-<return>") nil)
    
    ;; Bind our function
    (evil-local-set-key 'insert (kbd "C-<return>") #'euporie-termint-send-region-or-line)
    (evil-local-set-key 'normal (kbd "C-<return>") #'euporie-termint-send-region-or-line)
    (evil-local-set-key 'visual (kbd "C-<return>") #'euporie-termint-send-region-or-line)
    (local-set-key (kbd "C-<return>") #'euporie-termint-send-region-or-line)))

;;; Keybinding Setup

(defun euporie-termint-setup-keybindings ()
  "Setup C-RET keybinding for euporie integration."
  
  ;; Unbind Doom default C-RET keybinding globally
  (map! "C-<return>" nil)
  
  ;; Set up hooks for all supported languages
  (add-hook 'python-mode-hook #'euporie-termint-setup-buffer-keybinding)
  (add-hook 'ess-r-mode-hook #'euporie-termint-setup-buffer-keybinding)
  (add-hook 'stata-mode-hook #'euporie-termint-setup-buffer-keybinding)
  (add-hook 'ess-stata-mode-hook #'euporie-termint-setup-buffer-keybinding)
  (add-hook 'SAS-mode-hook #'euporie-termint-setup-buffer-keybinding)
  
  ;; Set up for org-src-mode
  (add-hook 'org-src-mode-hook #'euporie-termint-setup-buffer-keybinding))

;;; Setup and Initialization

(defun euporie-termint-setup ()
  "Set up euporie termint definitions."
  (message "euporie-termint: Setting up unified euporie console integration")
  
  ;; Configure termint backend - use eat for graphics support
  (setq termint-backend 'eat)
  
  ;; Verify euporie is available
  (let ((euporie-path (or (executable-find "euporie-console")
                         (when (file-executable-p (expand-file-name "euporie-console" euporie-termint-project-dir))
                           (expand-file-name "euporie-console" euporie-termint-project-dir)))))
    
    (if euporie-path
        (progn
          (message "euporie-termint: Found euporie at: %s" euporie-path)
          (euporie-termint-debug-log 'info "Unified euporie setup completed with path: %s" euporie-path))
      (progn
        (message "euporie-termint: WARNING - No euporie-console executable found")
        (euporie-termint-debug-log 'warn "No euporie-console executable found in PATH or project directory"))))))

;; Initialize keybindings when loaded
(when (featurep 'evil)
  (euporie-termint-setup-keybindings))

(provide 'euporie-termint)
;;; euporie-termint.el ends here