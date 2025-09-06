;;; euporie-termint.el --- Euporie console integration using termint and eat -*- lexical-binding: t; -*-

;;; Commentary:
;; Euporie console integration using termint.el with eat backend for native graphics support
;; EUPORIE HANDLES GRAPHICS NATIVELY - no manual conversion needed
;; Uses euporie console with --graphics=sixel for inline display

;;; Code:

(require 'termint nil t)
(require 'tramp-qrsh nil t)
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

;; ESS-style customizable window splitting thresholds
(defcustom euporie-termint-width-threshold nil
  "Minimum width for splitting windows sensibly to display console on right.
See `split-width-threshold' for a detailed description.
If nil, the value of `split-width-threshold' is used."
  :group 'euporie-termint
  :type '(choice (const nil) (integer)))

(defcustom euporie-termint-height-threshold nil
  "Minimum height for splitting windows sensibly to display console below.
See `split-height-threshold' for a detailed description.
If nil, the value of `split-height-threshold' is used."
  :group 'euporie-termint
  :type '(choice (const nil) (integer)))

(defvar euporie-termint-debug-log-file (expand-file-name "euporie-termint-debug.log" "~/")
  "Log file for euporie debugging information.")
(defvar sas-workflow-debug-log-file (expand-file-name "sas-workflow-debug.log" "~/")
  "Log file for SAS workflow debugging information.")


(defun euporie-termint-debug-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to debug file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-termint-debug-log-file))))

(defun sas-workflow-debug-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to SAS workflow debug file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) sas-workflow-debug-log-file))))

;;; Environment Detection


(defun euporie-termint--build-euporie-command (kernel project-dir)
  "Build euporie console command for KERNEL in PROJECT-DIR."
  (let ((graphics-protocol (if (string= kernel "stata") 
                               euporie-termint-stata-graphics-protocol
                             euporie-termint-graphics-protocol)))
    ;; Use euporie directly (no pixi run) for both local and remote to avoid network calls
    (format "euporie console --graphics=%s --kernel-name=%s" graphics-protocol kernel)))

;;; Kernel Management Functions

(defun euporie-python-start (&optional dir)
  "Start Python euporie console with direnv handling.
If DIR is provided and is a TRAMP path, start remote Python session.
Otherwise start local Python session."
  (interactive)
  (let* ((is-remote (and dir (file-remote-p dir)))
         (buffer-name "*euporie-python*")
         (smart-cmd (euporie-termint--build-euporie-command "python3" euporie-termint-project-dir)))
    
    (euporie-termint-debug-log 'info "Starting %s Python euporie console in directory: %s" 
                               (if is-remote "remote" "local") (or dir default-directory))
    
    ;; Kill any existing buffer first
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))
    
    (if is-remote
        (progn
          (euporie-termint-debug-log 'info "Calling euporie-python-start-remote with dir: %s" dir)
          (euporie-python-start-remote dir))
      (progn
        (euporie-termint-debug-log 'info "Calling euporie-python-start-local")
        (euporie-python-start-local)))))

(defun euporie-python-start-local ()
  "Start local Python euporie console using termint."
  (let* ((buffer-name "*euporie-python*")
         (smart-cmd (euporie-termint--build-euporie-command "python3" euporie-termint-project-dir)))
    
    (euporie-termint-debug-log 'info "Starting local Python euporie console...")
    (euporie-termint-debug-log 'info "Local Python command: %s" smart-cmd)
    
    ;; Define termint with proper environment for graphics and working directory
    (let ((default-directory euporie-termint-project-dir))
      (termint-define "euporie-python" smart-cmd
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "eat-truecolor") 
                             ("COLORTERM" . "truecolor")
                             ("EUPORIE_GRAPHICS" . "sixel"))))
    ;; Start the termint session with error handling
    (condition-case err
        (progn
          (let ((default-directory euporie-termint-project-dir))
            (termint-euporie-python-start))
          ;; Display in split window using proper console display function
          (when-let ((buffer (get-buffer "*euporie-python*")))
            (euporie-termint-display-console-right buffer)))
      (error 
       (euporie-termint-debug-log 'error "Failed to start Python termint: %s" err)
       (message "Warning: Failed to start Python euporie console: %s" err)))
    
    ;; Then wait a short time for kernel to initialize (simplified approach)
    (when (get-buffer buffer-name)
      (sleep-for 2)  ; Simple 2-second wait instead of complex detection
      (euporie-termint-debug-log 'info "Kernel initialization wait complete"))))

(defun euporie-r-start (&optional dir)
  "Start R euporie console with direnv handling.
If DIR is provided and is a TRAMP path, start remote R session.
Otherwise start local R session."
  (interactive)
  (let* ((is-remote (and dir (file-remote-p dir)))
         (buffer-name "*euporie-r*")
         (smart-cmd (euporie-termint--build-euporie-command "ir" euporie-termint-project-dir)))
    
    (euporie-termint-debug-log 'info "Starting %s R euporie console in directory: %s" 
                               (if is-remote "remote" "local") (or dir default-directory))
    
    ;; Kill any existing buffer first
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))
    
    (if is-remote
        (progn
          (euporie-termint-debug-log 'info "Calling euporie-r-start-remote with dir: %s" dir)
          (euporie-r-start-remote dir))
      (progn
        (euporie-termint-debug-log 'info "Calling euporie-r-start-local")
        (euporie-r-start-local)))))

(defun euporie-r-start-local ()
  "Start local R euporie console using termint."
  (let* ((buffer-name "*euporie-r*")
         (smart-cmd (euporie-termint--build-euporie-command "ir" euporie-termint-project-dir)))
    
    (euporie-termint-debug-log 'info "Starting local R euporie console...")
    (euporie-termint-debug-log 'info "Local R command: %s" smart-cmd)
    
    ;; Define termint with proper environment for graphics and working directory
    (let ((default-directory euporie-termint-project-dir))
      (termint-define "euporie-r" smart-cmd
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "eat-truecolor") 
                             ("COLORTERM" . "truecolor")
                             ("EUPORIE_GRAPHICS" . "sixel"))))
    ;; Start the termint session with error handling
    (condition-case err
        (progn
          (let ((default-directory euporie-termint-project-dir))
            (termint-euporie-r-start))
          ;; Display in split window using proper console display function
          (when-let ((buffer (get-buffer "*euporie-r*")))
            (euporie-termint-display-console-right buffer)))
      (error 
       (euporie-termint-debug-log 'error "Failed to start R termint: %s" err)
       (message "Warning: Failed to start R euporie console: %s" err)))
    
    ;; Then wait a short time for kernel to initialize (simplified approach)
    (when (get-buffer buffer-name)
      (sleep-for 2)  ; Simple 2-second wait instead of complex detection
      (euporie-termint-debug-log 'info "Kernel initialization wait complete"))))

(defun euporie-stata-start (&optional dir)
  "Start Stata euporie console with direnv handling and Stata-specific optimizations.
If DIR is provided and is a TRAMP path, start remote Stata session.
Otherwise start local Stata session."
  (interactive)
  (let* ((is-remote (and dir (file-remote-p dir)))
         (buffer-name "*euporie-stata*")
         (smart-cmd (euporie-termint--build-euporie-command "stata" euporie-termint-project-dir)))
    
    (euporie-termint-debug-log 'info "Starting %s Stata euporie console with optimizations in directory: %s" 
                               (if is-remote "remote" "local") (or dir default-directory))
    
    ;; Kill any existing buffer first
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))
    
    (if is-remote
        (progn
          (euporie-termint-debug-log 'info "Calling euporie-stata-start-remote with dir: %s" dir)
          (euporie-stata-start-remote dir))
      (progn
        (euporie-termint-debug-log 'info "Calling euporie-stata-start-local")
        (euporie-stata-start-local)))))

(defun euporie-stata-start-local ()
  "Start local Stata euporie console using termint."
  (let* ((buffer-name "*euporie-stata*")
         (smart-cmd (euporie-termint--build-euporie-command "stata" euporie-termint-project-dir)))
    
    (euporie-termint-debug-log 'info "Starting local Stata euporie console with optimizations...")
    (euporie-termint-debug-log 'info "Local Stata command: %s" smart-cmd)
    
    ;; Define termint with Stata-specific environment optimizations and working directory
    (let ((default-directory euporie-termint-project-dir))
      (termint-define "euporie-stata" smart-cmd
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "eat-truecolor")           ; Use eat's native term with graphics support
                             ("COLORTERM" . "truecolor")
                             ("EUPORIE_GRAPHICS" . "sixel")       ; Use sixel protocol for consistency
                             ("LANG" . "en_US.UTF-8")            ; Explicit locale for Unicode
                             ("LC_ALL" . "en_US.UTF-8"))))       ; Full locale support
    ;; Start the termint session with error handling
    (condition-case err
        (progn
          (let ((default-directory euporie-termint-project-dir))
            (termint-euporie-stata-start))
          ;; Display in split window using proper console display function
          (when-let ((buffer (get-buffer "*euporie-stata*")))
            (euporie-termint-display-console-right buffer)))
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
    
    ;; Define termint for local execution with working directory
    (let ((default-directory euporie-termint-project-dir))
      (termint-define "euporie-sas" smart-cmd
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "eat-truecolor") 
                             ("COLORTERM" . "truecolor")
                             ("EUPORIE_GRAPHICS" . "sixel"))))
    
    ;; Start the termint session
    (condition-case err
        (progn 
          (let ((default-directory euporie-termint-project-dir))
            (termint-euporie-sas-start))
          ;; Display in split window using proper console display function
          (when-let ((buffer (get-buffer "*euporie-sas*")))
            (euporie-termint-display-console-right buffer)))
      (error 
       (euporie-termint-debug-log 'error "Failed to start local SAS termint: %s" err)
       (message "Warning: Failed to start local SAS euporie console: %s" err)))))

(defun euporie-sas-start-remote (remote-dir)
  "Start remote SAS euporie console by using working tramp-qrsh-with-custom-buffer and then sending euporie command."
  (let* ((localname (if (file-remote-p remote-dir)
                        (file-remote-p remote-dir 'localname)
                      remote-dir)))
    
    (sas-workflow-debug-log 'info "=== euporie-sas-start-remote called with dir: %s ===" remote-dir)
    (sas-workflow-debug-log 'debug "Remote SAS start - localname extracted: %s" localname)
    (euporie-termint-debug-log 'info "Remote SAS start - using tramp-qrsh-with-custom-buffer + euporie command for: %s" remote-dir)
    
    ;; Use the working tramp-qrsh-with-custom-buffer to get to compute node
    (sas-workflow-debug-log 'info "Calling tramp-qrsh-with-custom-buffer to establish remote connection with buffer name: *euporie-sas*")
    (let ((wrds-buffer (tramp-qrsh-with-custom-buffer nil "*euporie-sas*")))
      
      (if wrds-buffer
          (progn
            (sas-workflow-debug-log 'info "tramp-qrsh-with-custom-buffer returned buffer: %s" (buffer-name wrds-buffer))
            ;; Keep original buffer name - termint expects this for send functions
            (sas-workflow-debug-log 'debug "Using original buffer name: %s" (buffer-name wrds-buffer)))
        (sas-workflow-debug-log 'error "tramp-qrsh-with-custom-buffer returned nil - no remote connection established"))
      
      (when wrds-buffer
        
        ;; Send euporie command to the compute node shell with suppressed output
        (let ((euporie-cmd (format "cd %s 2>/dev/null && exec %s" localname 
                                  (euporie-termint--build-euporie-command "sas" remote-dir))))
          (sas-workflow-debug-log 'info "Preparing euporie command: %s" euporie-cmd)
          (with-current-buffer wrds-buffer
            ;; Use termint send function (not comint)
            (sas-workflow-debug-log 'debug "Sending euporie command to compute node via termint")
            (if (fboundp 'termint-euporie-sas-send-string)
                (termint-euporie-sas-send-string euporie-cmd)
              (error "termint-euporie-sas-send-string not available"))
            (sas-workflow-debug-log 'info "Successfully sent euporie command to compute node")
            (euporie-termint-debug-log 'info "Sent euporie command to compute node: %s" euporie-cmd)
            
            ;; Allow time for euporie console to start
            (sleep-for 3)))  ; Brief delay for console startup
        
        ;; Display in split window using proper console display function
        (sas-workflow-debug-log 'debug "Creating split window layout for remote SAS console")
        (euporie-termint-display-console-right wrds-buffer)
        
        ;; STEP 1: Wait for SAS kernel to be ready (empty circle)
        (sas-workflow-debug-log 'info "STEP 1: Waiting for SAS kernel ready indicator (empty circle)...")
        (let ((wait-count 0)
              (max-wait 60)  ; 60 * 0.25 = 15 seconds max
              (kernel-ready nil))
          (while (and (< wait-count max-wait) (not kernel-ready))
            (sleep-for 0.25)  ; Poll every 250ms for SAS kernel
            (setq wait-count (1+ wait-count))
            (sas-workflow-debug-log 'debug "Polling for SAS kernel ready - attempt %d/%d (%.1f seconds)" wait-count max-wait (* wait-count 0.25))
            ;; Check buffer content for SAS readiness indicators
            (when (and (>= wait-count 8)  ; Minimum 2 seconds before checking
                       (buffer-live-p wrds-buffer))
              (with-current-buffer wrds-buffer
                (let ((buffer-content (buffer-substring-no-properties (max 1 (- (point-max) 500)) (point-max))))
                  (sas-workflow-debug-log 'debug "Checking buffer content for SAS empty circle indicator")
                  ;; Look for SAS indicator with EMPTY circle (kernel ready to receive input)
                  (when (string-match-p "▌SAS▐▌○▐" buffer-content)
                    (sas-workflow-debug-log 'info "Found SAS kernel ready indicator (empty circle) - ready for initialization")
                    (setq kernel-ready t))))))
          (if kernel-ready
              (sas-workflow-debug-log 'info "STEP 1 COMPLETE: SAS kernel ready after %d polls (%.1f seconds)" wait-count (* wait-count 0.25))
            (sas-workflow-debug-log 'warn "STEP 1 TIMEOUT: No SAS ready indicator after %.1f seconds" (* wait-count 0.25))))
        
        ;; STEP 2: Mode switching approach for euporie console initialization
        (sas-workflow-debug-log 'info "STEP 2: Initializing euporie console with mode switching...")
        (when (buffer-live-p wrds-buffer)
          (with-current-buffer wrds-buffer
            ;; Ensure euporie console is in normal mode using evil-mode function
            (sas-workflow-debug-log 'debug "Step 2a: Ensuring normal mode (evil-force-normal-state)")
            (when (fboundp 'evil-force-normal-state)
              (evil-force-normal-state))
            (sleep-for 0.2)  ; Brief pause for mode switch
            
            ;; Send C-e to initialize kernel (works in normal mode)
            (sas-workflow-debug-log 'debug "Step 2b: Sending C-e to initialize kernel")
            (termint-euporie-sas-send-string "\C-e")  ; Kernel initialization
            (sleep-for 0.3)  ; Brief pause for initialization
            
            ;; Switch to insert mode using proper evil-mode function without sending literal characters
            (sas-workflow-debug-log 'debug "Step 2c: Switching to insert mode using evil-insert-state")
            (when (fboundp 'evil-insert-state)
              (condition-case err
                  (evil-insert-state)
                (error 
                 (sas-workflow-debug-log 'warn "evil-insert-state failed: %s, trying evil-insert" err)
                 (when (fboundp 'evil-insert)
                   (condition-case err2
                       (evil-insert 1)
                     (error 
                      (sas-workflow-debug-log 'error "Both evil-insert-state and evil-insert failed: %s" err2)))))))
            (sleep-for 0.2)  ; Brief pause for mode switch
            
            (sas-workflow-debug-log 'info "Mode switching initialization complete - console ready for code input"))))
        (sas-workflow-debug-log 'info "Remote SAS kernel initialization wait complete")
        (euporie-termint-debug-log 'info "Remote SAS kernel initialization wait complete")
        
        (sas-workflow-debug-log 'info "Remote SAS setup complete - buffer: %s" (buffer-name wrds-buffer))
        (euporie-termint-debug-log 'info "Remote SAS setup complete - buffer: %s" (buffer-name wrds-buffer))
        wrds-buffer)))


;;; Send Functions

;; Global send function for SAS (works for both local and remote)
(defun termint-euporie-sas-send-string (code)
  "Send code to SAS euporie console (works for both local and remote)."
  (when (get-buffer "*euporie-sas*")
    (with-current-buffer "*euporie-sas*"
      (let ((proc (get-buffer-process "*euporie-sas*")))
        (when proc
          (process-send-string proc code)
          ;; Send second newline to ensure execution in euporie console
          (process-send-string proc "\n"))))))

;; Global send functions for Python, R, and Stata (works for both local and remote)
(defun termint-euporie-python-send-string (code)
  "Send code to Python euporie console (works for both local and remote)."
  (when (get-buffer "*euporie-python*")
    (with-current-buffer "*euporie-python*"
      (let ((proc (get-buffer-process "*euporie-python*")))
        (when proc
          (process-send-string proc code)
          ;; Send second newline to ensure execution in euporie console
          (process-send-string proc "\n"))))))

(defun termint-euporie-r-send-string (code)
  "Send code to R euporie console (works for both local and remote)."
  (when (get-buffer "*euporie-r*")
    (with-current-buffer "*euporie-r*"
      (let ((proc (get-buffer-process "*euporie-r*")))
        (when proc
          (process-send-string proc code)
          ;; Send second newline to ensure execution in euporie console
          (process-send-string proc "\n"))))))

(defun termint-euporie-stata-send-string (code)
  "Send code to Stata euporie console (works for both local and remote)."
  (when (get-buffer "*euporie-stata*")
    (with-current-buffer "*euporie-stata*"
      (let ((proc (get-buffer-process "*euporie-stata*")))
        (when proc
          (process-send-string proc code)
          ;; Send second newline to ensure execution in euporie console
          (process-send-string proc "\n"))))))

;;; Remote Execution Functions

;; Remote execution functions for Python, R, and Stata (following SAS pattern)
(defun euporie-python-start-remote (remote-dir)
  "Start remote Python euporie console using tramp-qrsh-with-custom-buffer."
  (let* ((localname (if (file-remote-p remote-dir)
                        (file-remote-p remote-dir 'localname)
                      remote-dir)))
    
    (euporie-termint-debug-log 'info "=== euporie-python-start-remote called with dir: %s ===" remote-dir)
    (euporie-termint-debug-log 'debug "Remote Python start - localname extracted: %s" localname)
    
    ;; Use tramp-qrsh-with-custom-buffer to get to compute node
    (euporie-termint-debug-log 'info "Calling tramp-qrsh-with-custom-buffer to establish remote connection with buffer name: *euporie-python*")
    (let ((wrds-buffer (tramp-qrsh-with-custom-buffer nil "*euporie-python*")))
      
      (if wrds-buffer
          (progn
            (euporie-termint-debug-log 'info "tramp-qrsh-with-custom-buffer returned buffer: %s" (buffer-name wrds-buffer))
            (euporie-termint-debug-log 'debug "Using original buffer name: %s" (buffer-name wrds-buffer)))
        (euporie-termint-debug-log 'error "tramp-qrsh-with-custom-buffer returned nil - no remote connection established"))
      
      (when wrds-buffer
        ;; Send euporie command to the compute node shell with suppressed output
        (let ((euporie-cmd (format "cd %s 2>/dev/null && exec %s" localname 
                                  (euporie-termint--build-euporie-command "python" remote-dir))))
          (euporie-termint-debug-log 'info "Preparing euporie command: %s" euporie-cmd)
          (with-current-buffer wrds-buffer
            ;; Use termint send function (not comint)
            (euporie-termint-debug-log 'debug "Sending euporie command to compute node via termint")
            (if (fboundp 'termint-euporie-python-send-string)
                (termint-euporie-python-send-string euporie-cmd)
              (error "termint-euporie-python-send-string not available"))
            (euporie-termint-debug-log 'info "Successfully sent euporie command to compute node")
            
            ;; Allow time for euporie console to start
            (sleep-for 3)))  ; Brief delay for console startup
        
        ;; Display in split window using proper console display function
        (euporie-termint-debug-log 'debug "Creating split window layout for remote Python console")
        (euporie-termint-display-console-right wrds-buffer)
        
        wrds-buffer))))

(defun euporie-r-start-remote (remote-dir)
  "Start remote R euporie console using tramp-qrsh-with-custom-buffer."
  (let* ((localname (if (file-remote-p remote-dir)
                        (file-remote-p remote-dir 'localname)
                      remote-dir)))
    
    (euporie-termint-debug-log 'info "=== euporie-r-start-remote called with dir: %s ===" remote-dir)
    (euporie-termint-debug-log 'debug "Remote R start - localname extracted: %s" localname)
    
    ;; Use tramp-qrsh-with-custom-buffer to get to compute node
    (euporie-termint-debug-log 'info "Calling tramp-qrsh-with-custom-buffer to establish remote connection with buffer name: *euporie-r*")
    (let ((wrds-buffer (tramp-qrsh-with-custom-buffer nil "*euporie-r*")))
      
      (if wrds-buffer
          (progn
            (euporie-termint-debug-log 'info "tramp-qrsh-with-custom-buffer returned buffer: %s" (buffer-name wrds-buffer))
            (euporie-termint-debug-log 'debug "Using original buffer name: %s" (buffer-name wrds-buffer)))
        (euporie-termint-debug-log 'error "tramp-qrsh-with-custom-buffer returned nil - no remote connection established"))
      
      (when wrds-buffer
        ;; Send euporie command to the compute node shell with suppressed output
        (let ((euporie-cmd (format "cd %s 2>/dev/null && exec %s" localname 
                                  (euporie-termint--build-euporie-command "r" remote-dir))))
          (euporie-termint-debug-log 'info "Preparing euporie command: %s" euporie-cmd)
          (with-current-buffer wrds-buffer
            ;; Use termint send function (not comint)
            (euporie-termint-debug-log 'debug "Sending euporie command to compute node via termint")
            (if (fboundp 'termint-euporie-r-send-string)
                (termint-euporie-r-send-string euporie-cmd)
              (error "termint-euporie-r-send-string not available"))
            (euporie-termint-debug-log 'info "Successfully sent euporie command to compute node")
            
            ;; Allow time for euporie console to start
            (sleep-for 3)))  ; Brief delay for console startup
        
        ;; Display in split window using proper console display function
        (euporie-termint-debug-log 'debug "Creating split window layout for remote R console")
        (euporie-termint-display-console-right wrds-buffer)
        
        wrds-buffer))))

(defun euporie-stata-start-remote (remote-dir)
  "Start remote Stata euporie console using tramp-qrsh-with-custom-buffer."
  (let* ((localname (if (file-remote-p remote-dir)
                        (file-remote-p remote-dir 'localname)
                      remote-dir)))
    
    (euporie-termint-debug-log 'info "=== euporie-stata-start-remote called with dir: %s ===" remote-dir)
    (euporie-termint-debug-log 'debug "Remote Stata start - localname extracted: %s" localname)
    
    ;; Use tramp-qrsh-with-custom-buffer to get to compute node
    (euporie-termint-debug-log 'info "Calling tramp-qrsh-with-custom-buffer to establish remote connection with buffer name: *euporie-stata*")
    (let ((wrds-buffer (tramp-qrsh-with-custom-buffer nil "*euporie-stata*")))
      
      (if wrds-buffer
          (progn
            (euporie-termint-debug-log 'info "tramp-qrsh-with-custom-buffer returned buffer: %s" (buffer-name wrds-buffer))
            (euporie-termint-debug-log 'debug "Using original buffer name: %s" (buffer-name wrds-buffer)))
        (euporie-termint-debug-log 'error "tramp-qrsh-with-custom-buffer returned nil - no remote connection established"))
      
      (when wrds-buffer
        ;; Send euporie command to the compute node shell with suppressed output
        (let ((euporie-cmd (format "cd %s 2>/dev/null && exec %s" localname 
                                  (euporie-termint--build-euporie-command "stata" remote-dir))))
          (euporie-termint-debug-log 'info "Preparing euporie command: %s" euporie-cmd)
          (with-current-buffer wrds-buffer
            ;; Use termint send function (not comint)
            (euporie-termint-debug-log 'debug "Sending euporie command to compute node via termint")
            (if (fboundp 'termint-euporie-stata-send-string)
                (termint-euporie-stata-send-string euporie-cmd)
              (error "termint-euporie-stata-send-string not available"))
            (euporie-termint-debug-log 'info "Successfully sent euporie command to compute node")
            
            ;; Allow time for euporie console to start
            (sleep-for 3)))  ; Brief delay for console startup
        
        ;; Display in split window using proper console display function
        (euporie-termint-debug-log 'debug "Creating split window layout for remote Stata console")
        (euporie-termint-display-console-right wrds-buffer)
        
        wrds-buffer))))

;;; Buffer Management


(defun euporie-termint-get-or-create-buffer (kernel &optional dir)
  "Get or create euporie termint buffer for KERNEL.
DIR parameter is used to determine local vs remote execution for all kernels."
  (let* ((is-remote (and dir (file-remote-p dir)))
         (buffer-name (format "*euporie-%s*" kernel))
         (start-func (cond
                     ((string= kernel "python") 
                      (progn
                        (euporie-termint-debug-log 'info "Creating Python start lambda with dir: %s" (or dir "nil"))
                        (lambda () 
                          (euporie-termint-debug-log 'info "Lambda called - about to call euporie-python-start with dir: %s" (or dir "nil"))
                          (euporie-python-start dir))))
                     ((string= kernel "r") 
                      (progn
                        (euporie-termint-debug-log 'info "Creating R start lambda with dir: %s" (or dir "nil"))
                        (lambda () 
                          (euporie-termint-debug-log 'info "Lambda called - about to call euporie-r-start with dir: %s" (or dir "nil"))
                          (euporie-r-start dir))))
                     ((string= kernel "stata") 
                      (progn
                        (euporie-termint-debug-log 'info "Creating Stata start lambda with dir: %s" (or dir "nil"))
                        (lambda () 
                          (euporie-termint-debug-log 'info "Lambda called - about to call euporie-stata-start with dir: %s" (or dir "nil"))
                          (euporie-stata-start dir))))
                     ((string= kernel "sas") 
                      (progn
                        (euporie-termint-debug-log 'info "Creating SAS start lambda with dir: %s" (or dir "nil"))
                        (lambda () 
                          (euporie-termint-debug-log 'info "Lambda called - about to call euporie-sas-start with dir: %s" (or dir "nil"))
                          (euporie-sas-start dir))))
                     (t (error "Unsupported kernel: %s" kernel))))
         (buffer (get-buffer buffer-name)))
    
    ;; Check if buffer exists and has live process, and is compatible with execution type
    (let ((buffer-compatible (and buffer 
                                  (buffer-live-p buffer)
                                  (get-buffer-process buffer)
                                  (process-live-p (get-buffer-process buffer))
                                  ;; Simple reuse: if buffer has live process, it's compatible
                                  ;; All kernels now support both local and remote execution
                                  t)))
      
      (when (and buffer (not buffer-compatible))
        (euporie-termint-debug-log 'info "Killing existing %s buffer - process may have died" kernel)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))
        (setq buffer nil))
      
      (if buffer-compatible
          buffer
        ;; Need to start new console
        (progn
        (funcall start-func)
        ;; For remote SAS, use special synchronous handling
        (if (and is-remote (string= kernel "sas"))
            (progn
              (euporie-termint-debug-log 'info "Remote SAS detected - using synchronous startup")
              ;; The remote function handles all timing internally
              (let ((created-buffer (get-buffer buffer-name)))
                ;; Auto-associate with current org-src buffer if we're in one
                (when (and created-buffer 
                           (string-match-p "\\*Org Src.*\\[.*\\]\\*" (buffer-name)))
                  (euporie-termint-associate-buffers (current-buffer) buffer-name)
                  (euporie-termint-debug-log 'info "Auto-associated org-src buffer %s with %s (remote)" 
                                           (buffer-name) buffer-name))
                created-buffer))
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
            (let ((created-buffer (get-buffer buffer-name)))
              ;; Auto-associate with current org-src buffer if we're in one
              (when (and created-buffer 
                         (string-match-p "\\*Org Src.*\\[.*\\]\\*" (buffer-name)))
                (euporie-termint-associate-buffers (current-buffer) buffer-name)
                (euporie-termint-debug-log 'info "Auto-associated org-src buffer %s with %s" 
                                         (buffer-name) buffer-name))
              created-buffer))))))))

;;; Language Detection

(defun euporie-termint-detect-kernel ()
  "Detect kernel from buffer name or org-src context."
  ;; Check if we're in a euporie buffer - extract kernel from buffer name
  (cond 
   ((string-match "\\*euporie-\\(.+\\)\\*" (buffer-name))
    (let ((kernel (match-string 1 (buffer-name))))
      (euporie-termint-debug-log 'debug "Detected kernel from euporie buffer name: %s" kernel)
      kernel))
   
   ;; Check org-src buffer context
   ((string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" (buffer-name))
    (let ((lang (match-string 1 (buffer-name))))
      (euporie-termint-debug-log 'debug "Detected kernel from org-src buffer: %s" lang)
      (cond
       ((or (string-equal lang "r") (string-equal lang "R")) "r")
       ((string-equal lang "python") "python") 
       ((string-equal lang "stata") "stata")
       ((or (string-equal lang "sas") (string-equal lang "SAS")) "sas")
       (t lang))))
   
   ;; Check org-src variable
   ((bound-and-true-p org-src--lang)
    (let ((lang org-src--lang))
      (euporie-termint-debug-log 'debug "Detected kernel from org-src variable: %s" lang)
      (cond
       ((or (string-equal lang "r") (string-equal lang "R")) "r")
       ((string-equal lang "python") "python")
       ((string-equal lang "stata") "stata") 
       ((or (string-equal lang "sas") (string-equal lang "SAS")) "sas")
       (t lang))))
   
   ;; Check org-mode element at point
   ((eq major-mode 'org-mode)
    (let ((lang (org-element-property :language (org-element-at-point))))
      (when lang
        (euporie-termint-debug-log 'debug "Detected kernel from org element: %s" lang)
        (cond
         ((or (string-equal lang "r") (string-equal lang "R")) "r")
         ((string-equal lang "python") "python")
         ((string-equal lang "stata") "stata")
         ((or (string-equal lang "sas") (string-equal lang "SAS")) "sas")
         (t lang)))))
   
   ;; Check major mode
   ((eq major-mode 'ess-r-mode) "r")
   ((or (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode)) "python")
   ((eq major-mode 'stata-mode) "stata")
   ((eq major-mode 'SAS-mode) "sas")
   
   ;; Default fallback
   (t 
    (euporie-termint-debug-log 'debug "No kernel detected, defaulting to python")
    "python")))

;;; Console Display Management

(defun euporie-termint-display-console-right (buffer &optional original-buffer original-window)
  "Display console BUFFER in a right split window using ESS-style window management with reuse.
This function uses ESS patterns but handles cases where original buffer may no longer exist."
  (condition-case err
      (let* ((initial-window (or original-window (selected-window)))
             (initial-buffer (or original-buffer (current-buffer)))
             (console-window (get-buffer-window buffer 'visible))
             ;; Check if initial buffer is still valid
             (initial-buffer-valid (and initial-buffer (buffer-live-p initial-buffer)))
             (initial-window-valid (and initial-window (window-live-p initial-window))))
        
        (euporie-termint-debug-log 'info "ESS-style window display: buffer=%s from=%s console-window-exists=%s" 
                                   (buffer-name buffer) 
                                   (if initial-buffer-valid (buffer-name initial-buffer) "<dead-buffer>")
                                   (if console-window "yes" "no"))
        
        ;; Save selected window to restore focus later (ESS pattern)
        (save-selected-window
          (cond
           ;; Case 1: Console window already visible - just switch to it and scroll
           (console-window
            (euporie-termint-debug-log 'info "Reusing existing console window")
            (with-selected-window console-window
              (goto-char (point-max))))
           
           ;; Case 2: Need to create window - use ESS split-window-sensibly approach
           (t
            (let* ((split-width-threshold (or euporie-termint-width-threshold
                                              split-width-threshold))
                   (split-height-threshold (or euporie-termint-height-threshold
                                               split-height-threshold))
                   (win (condition-case split-err
                            (split-window-sensibly (selected-window))
                          (error 
                           (euporie-termint-debug-log 'warn "split-window-sensibly failed: %s" split-err)
                           nil))))
              
              (if win
                  (progn
                    (euporie-termint-debug-log 'info "ESS-style split succeeded - using new window")
                    (set-window-buffer win buffer)
                    (with-selected-window win
                      (goto-char (point-max))))
                
                ;; Fallback: use pop-to-buffer with reuse-window priority (ESS pattern)
                (let ((console-window-fallback (pop-to-buffer buffer 
                                                               '((display-buffer-reuse-window
                                                                  display-buffer-in-side-window
                                                                  display-buffer-pop-up-window)
                                                                 (side . right)
                                                                 (window-width . 0.5)
                                                                 (inhibit-same-window . t)))))
                  (euporie-termint-debug-log 'info "ESS-style split failed - using pop-to-buffer fallback")
                  (when console-window-fallback
                    (with-selected-window console-window-fallback
                      (goto-char (point-max))))))))))
        
        ;; Enhanced fallback if pop-to-buffer also fails
        (unless (get-buffer-window buffer 'visible)
          (euporie-termint-debug-log 'warn "All window splitting methods failed, using switch-to-buffer-other-window")
          (condition-case switch-err
              (progn
                (switch-to-buffer-other-window buffer)
                (goto-char (point-max)))
            (error 
             (euporie-termint-debug-log 'error "switch-to-buffer-other-window failed: %s" switch-err)
             ;; Last resort - just display in current window
             (switch-to-buffer buffer)
             (goto-char (point-max)))))
        
        ;; Robust window/buffer restoration
        (cond
         ;; Best case: both window and buffer are still valid
         ((and initial-window-valid initial-buffer-valid)
          (euporie-termint-debug-log 'debug "Restoring to original window and buffer")
          (select-window initial-window)
          (unless (eq (current-buffer) initial-buffer)
            (set-buffer initial-buffer)))
         
         ;; Window valid but buffer gone - stay in window but don't set buffer
         (initial-window-valid
          (euporie-termint-debug-log 'debug "Restoring to original window (buffer no longer available)")
          (select-window initial-window))
         
         ;; Neither valid - find a reasonable window to focus
         (t
          (euporie-termint-debug-log 'debug "Original window/buffer gone - finding reasonable focus target")
          (let ((other-window (get-buffer-window buffer 'visible)))
            (if other-window
                (select-window other-window)
              ;; Fallback to any available window that's not the console
              (let ((available-windows (remove (get-buffer-window buffer 'visible) (window-list))))
                (when available-windows
                  (select-window (car available-windows))))))))
        
        (euporie-termint-debug-log 'info "ESS-style window display completed - focus handled robustly"))
    
    (error
     (euporie-termint-debug-log 'error "euporie-termint-display-console-right failed: %s" err)
     ;; Emergency fallback - just switch to the buffer
     (condition-case fallback-err
         (progn
           (switch-to-buffer buffer)
           (goto-char (point-max)))
       (error 
        (euporie-termint-debug-log 'error "Emergency fallback failed: %s" fallback-err))))))

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


;;; Code Execution

(defun euporie-termint-send-code (kernel code &optional dir force-execute)
  "Send CODE to euporie console for KERNEL with optional immediate execution.
DIR parameter is used for SAS to determine local vs remote execution.
If FORCE-EXECUTE is non-nil, send Ctrl+Enter to execute immediately."
  (let* ((is-remote-sas (and (string= kernel "sas") dir (file-remote-p dir)))
         ;; First try to get associated buffer, fall back to get-or-create
         (associated-buffer (euporie-termint-get-associated-buffer))
         (buffer (if associated-buffer
                     (progn 
                       (euporie-termint-debug-log 'info "Using associated buffer: %s" associated-buffer)
                       (get-buffer associated-buffer))
                   (progn
                     (euporie-termint-debug-log 'info "No association found, creating/getting buffer for kernel: %s" kernel)
                     (euporie-termint-get-or-create-buffer kernel dir))))
         (send-func (cond
                    ((string= kernel "python") #'termint-euporie-python-send-string)
                    ((string= kernel "r") #'termint-euporie-r-send-string)
                    ((string= kernel "stata") #'termint-euporie-stata-send-string)
                    ((string= kernel "sas") #'termint-euporie-sas-send-string)
                    (t (error "Unsupported kernel: %s" kernel)))))
    
    (sas-workflow-debug-log 'info "=== euporie-termint-send-code called ===")
    (sas-workflow-debug-log 'debug "Send code - kernel: %s, dir: %s, is-remote-sas: %s, force-execute: %s" 
                           kernel (or dir "nil") is-remote-sas force-execute)
    (sas-workflow-debug-log 'debug "Send code - buffer: %s, send-func: %s" 
                           (if buffer (buffer-name buffer) "nil") send-func)
    
    (when buffer
      (sas-workflow-debug-log 'info "Sending %s code: %s" kernel (substring code 0 (min 50 (length code))))
      (euporie-termint-debug-log 'info "Sending %s code: %s" kernel (substring code 0 (min 50 (length code))))
      
      ;; Display console in right window
      (euporie-termint-display-console-right buffer)
      
      ;; Send code to console with proper targeting
      (if is-remote-sas
          ;; For remote SAS, we need to wait for initialization to complete before sending code
          (progn
            (sas-workflow-debug-log 'debug "Using remote SAS send - checking if initialization is complete")
            ;; Wait for the initialization sequence to complete (it adds a short delay already)
            ;; We need an additional delay to ensure mode switching is complete
            (sleep-for 0.5)  ; Additional wait after mode switching completion
            (sas-workflow-debug-log 'debug "Mode switching delay complete - now sending code")
            (with-current-buffer buffer
              (sas-workflow-debug-log 'debug "Calling send function: %s with code" send-func)
              (funcall send-func code)
              ;; Force execution by sending additional newline if requested (euporie docs: two blank lines force execution)
              (when force-execute
                (sas-workflow-debug-log 'debug "Sending additional newline to force execution")
                ;; Send one additional newline (code already has one, this makes two blank lines total)
                (funcall send-func "\n"))))
        ;; For other kernels, use standard approach
        (progn
          (when (string= kernel "sas")
            (sas-workflow-debug-log 'debug "Using local SAS send - standard approach"))
          (funcall send-func code)
          ;; Force execution by sending additional newline if requested (euporie docs: two blank lines force execution)
          (when force-execute
            (euporie-termint-debug-log 'debug "Sending additional newline to force execution")
            ;; Send one additional newline (code already has one, this makes two blank lines total)
            (funcall send-func "\n"))))
      
      ;; Start file monitoring for Stata graphics if not already running
      (when (string= kernel "stata")
        (unless (or (and euporie-termint-stata-file-watcher 
                        (or (process-live-p euporie-termint-stata-file-watcher)
                            (timerp euporie-termint-stata-file-watcher))))
          (run-with-timer 0.5 nil #'euporie-termint-start-stata-file-monitor)))
      
      ;; Return buffer for further processing if needed
      buffer)))

;;; Helper Functions

(defun euporie-termint--region-function-or-paragraph ()
  "Extract region, function, or paragraph based on ESS logic.
If region is active, return it. Otherwise try to find function,
falling back to paragraph. Language-aware function detection."
  (cond 
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ;; For SAS, skip function detection entirely and use paragraph
   ;; This handles %MACRO, PROC, DATA, and other SAS constructs better
   ((or (eq major-mode 'SAS-mode) 
        (string-match-p "sas" (or (bound-and-true-p org-src--lang) "")))
    (thing-at-point 'paragraph t))
   ;; For R and other languages, use standard ESS approach
   (t 
    (condition-case nil
        (euporie-termint--standard-function-at-point)
      (error (thing-at-point 'paragraph t))))))

(defun euporie-termint--sas-function-at-point ()
  "Extract SAS procedure at point - looks for PROC/DATA statements."
  (save-excursion
    ;; Look backward for PROC or DATA statement
    (while (and (not (bobp)) 
                (not (looking-at "^\\s-*\\(proc\\|data\\)\\s-")))
      (forward-line -1))
    
    (if (looking-at "^\\s-*\\(proc\\|data\\)\\s-")
        ;; Found PROC/DATA, extract until RUN; or QUIT;
        (let ((start (point)))
          (while (and (not (eobp))
                      (not (looking-at "^\\s-*\\(run\\|quit\\)\\s-*;")))
            (forward-line 1))
          ;; Include the RUN/QUIT line
          (when (looking-at "^\\s-*\\(run\\|quit\\)\\s-*;")
            (end-of-line))
          (buffer-substring-no-properties start (point)))
      ;; No PROC/DATA found, signal error to fall back to paragraph
      (error "No SAS procedure found"))))

(defun euporie-termint--standard-function-at-point ()
  "Extract function using standard ESS approach with beginning/end-of-defun."
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (let ((start (point)))
      (end-of-defun)
      (buffer-substring-no-properties start (point)))))

;;; Buffer Association System

(defvar-local euporie-termint-associated-buffer nil
  "Buffer-local variable storing the associated euporie console buffer for this org-src buffer.")

(defun euporie-termint-associate-buffers (org-src-buffer euporie-buffer)
  "Associate an org-src buffer with its corresponding euporie buffer."
  (when (buffer-live-p org-src-buffer)
    (with-current-buffer org-src-buffer
      (setq-local euporie-termint-associated-buffer euporie-buffer)
      (euporie-termint-debug-log 'info "Associated %s with %s" 
                               (buffer-name org-src-buffer) euporie-buffer))))

(defun euporie-termint-get-associated-buffer ()
  "Get the associated euporie buffer for the current org-src buffer."
  (if (and (boundp 'euporie-termint-associated-buffer)
           euporie-termint-associated-buffer
           (get-buffer euporie-termint-associated-buffer))
      euporie-termint-associated-buffer
    ;; Fallback to kernel detection if no association exists
    (let* ((kernel (euporie-termint-detect-kernel))
           (buffer-name (format "*euporie-%s*" kernel)))
      (when (get-buffer buffer-name)
        buffer-name))))

;;; Buffer Reconnection Function

(defun euporie-termint-reconnect-buffers ()
  "Ensure termint buffer variables are connected to existing euporie buffers.
This fixes the issue where C-RET stops working after opening vterm or other buffer operations."
  (interactive)
  
  ;; List of kernel types and their corresponding buffer names
  (let ((kernel-buffers '(("python" . "*euporie-python*")
                         ("r" . "*euporie-r*")
                         ("stata" . "*euporie-stata*")
                         ("sas" . "*euporie-sas*"))))
    
    (dolist (kernel-buffer kernel-buffers)
      (let* ((kernel (car kernel-buffer))
             (buffer-name (cdr kernel-buffer))
             (buffer-var-name (intern (format "termint-euporie-%s-buffer" kernel)))
             (send-func-name (intern (format "termint-euporie-%s-send-string" kernel))))
        
        ;; If the buffer exists but the termint variable is nil, reconnect it
        (when (and (get-buffer buffer-name)
                   (or (not (boundp buffer-var-name))
                       (not (symbol-value buffer-var-name))))
          (set buffer-var-name buffer-name)
          (euporie-termint-debug-log 'info "Reconnected %s buffer variable to %s" 
                                   buffer-var-name buffer-name))))))

;;; Main C-RET Integration Function

(defun euporie-termint-send-region-or-line ()
  "Send current region or line to euporie console with automatic kernel detection.
Always forces execution when no region is selected to ensure code runs immediately."
  (interactive)
  
  (euporie-termint-debug-log 'info "=== C-RET pressed in buffer: %s ===" (buffer-name))
  
  ;; Ensure buffer variables are connected before proceeding
  (euporie-termint-reconnect-buffers)
  
  (let* ((kernel (euporie-termint-detect-kernel))
         ;; Extract :dir parameter from current context on-demand
         (extracted-dir (euporie-termint-extract-dir-from-current-context))
         (current-dir (or extracted-dir default-directory))
         (has-region (use-region-p))
         (code (cond
                (has-region
                 (buffer-substring-no-properties (region-beginning) (region-end)))
                ((eq major-mode 'org-mode)
                 ;; In org-mode, extract the current code block
                 (let ((element (org-element-at-point)))
                   (if (eq (org-element-type element) 'src-block)
                       (org-element-property :value element)
                     (thing-at-point 'line t))))
                ((string-match-p "\\*Org Src.*\\[" (buffer-name))
                 ;; In org-src edit buffer, use ESS-style region-function-paragraph logic
                 (euporie-termint--region-function-or-paragraph))
                (t
                 (thing-at-point 'line t))))
         ;; Force execution when no region is selected (single line/paragraph execution)
         (force-execution (not has-region)))
    
    (euporie-termint-debug-log 'debug "Code extraction result: %s" (if code (substring code 0 (min 100 (length code))) "nil"))
    (euporie-termint-debug-log 'debug "Force execution: %s (no region selected: %s)" force-execution (not has-region))
    
    (when code
      (euporie-termint-debug-log 'info "Executing %s code via euporie in dir: %s (force: %s)" kernel current-dir force-execution)
      ;; Pass dir parameter for any kernel with remote directory (not just SAS)
      (if (file-remote-p current-dir)
          (euporie-termint-send-code kernel code current-dir force-execution)
        (euporie-termint-send-code kernel code nil force-execution)))))

;;; Org-babel Integration (Optional)

(defun org-babel-execute:python (body params)
  "Execute Python BODY with PARAMS using euporie."
  (let ((dir (cdr (assoc :dir params))))
    (let ((buffer (euporie-termint-get-or-create-buffer "python")))
      (when buffer
        (euporie-termint-send-code "python" body dir t)  ; Always force execution for org-babel
        ;; For org-babel, we don't return output as euporie handles display
        ""))))

(defun org-babel-execute:R (body params)
  "Execute R BODY with PARAMS using euporie."
  (let ((dir (cdr (assoc :dir params))))
    (let ((buffer (euporie-termint-get-or-create-buffer "r")))
      (when buffer
        (euporie-termint-send-code "r" body dir t)  ; Always force execution for org-babel
        ;; For org-babel, we don't return output as euporie handles display
        ""))))

(defun org-babel-execute:stata (body params)
  "Execute Stata BODY with PARAMS using euporie."
  (let ((dir (cdr (assoc :dir params))))
    (let ((buffer (euporie-termint-get-or-create-buffer "stata")))
      (when buffer
        (euporie-termint-send-code "stata" body dir t)  ; Always force execution for org-babel
        ;; For org-babel, we don't return output as euporie handles display
        ""))))

(defun org-babel-execute:sas (body params)
  "Execute SAS BODY with PARAMS using euporie.
Supports remote execution via :dir parameter."
  (let ((dir (cdr (assoc :dir params))))
    (euporie-termint-debug-log 'debug "=== org-babel-execute:sas ENTRY ===")
    (euporie-termint-debug-log 'debug "  params: %s" params)
    (euporie-termint-debug-log 'debug "  extracted dir: %s" dir)
    (euporie-termint-debug-log 'debug "  dir type: %s" (type-of dir))
    (euporie-termint-debug-log 'info "SAS execution requested with dir: %s" dir)
    
    (euporie-termint-send-code "sas" body dir t)  ; Always force execution for org-babel
    ;; For org-babel, we don't return output as euporie handles display
    ""))

;;; Hook Functions

(defun euporie-termint-extract-dir-from-current-context ()
  "Extract :dir parameter from current org-src context using multiple strategies."
  (euporie-termint-debug-log 'debug "=== Extracting :dir from current context ===")
  (euporie-termint-debug-log 'debug "Buffer name: %s" (buffer-name))
  
  (let ((extracted-dir nil))
    
    ;; Strategy 1: Try org-src markers if available and valid
    (when (and (string-match "\\*Org Src.*\\[" (buffer-name))
               (boundp 'org-src--beg-marker) 
               org-src--beg-marker)
      (euporie-termint-debug-log 'debug "Strategy 1: Trying org-src markers")
      (euporie-termint-debug-log 'debug "  Marker: %s" org-src--beg-marker)
      (euporie-termint-debug-log 'debug "  Marker buffer: %s" (marker-buffer org-src--beg-marker))
      
      (when (and (markerp org-src--beg-marker)
                 (marker-buffer org-src--beg-marker)
                 (buffer-live-p (marker-buffer org-src--beg-marker))
                 (marker-position org-src--beg-marker))
        (condition-case err
            (with-current-buffer (marker-buffer org-src--beg-marker)
              (save-excursion
                (goto-char (marker-position org-src--beg-marker))
                (let* ((element (org-element-at-point))
                       (params (org-babel-parse-header-arguments
                               (or (org-element-property :parameters element) "")))
                       (dir (cdr (assoc :dir params))))
                  (euporie-termint-debug-log 'debug "  Strategy 1 - Element: %s" element)
                  (euporie-termint-debug-log 'debug "  Strategy 1 - Parsed params: %s" params)
                  (euporie-termint-debug-log 'debug "  Strategy 1 - Extracted dir: %s" dir)
                  (setq extracted-dir dir))))
          (error 
           (euporie-termint-debug-log 'debug "  Strategy 1 failed: %s" err)))))
    
    ;; Strategy 2: Search for org buffers with matching src block if Strategy 1 failed
    (when (and (not extracted-dir)
               (string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" (buffer-name)))
      (euporie-termint-debug-log 'debug "Strategy 2: Searching org buffers for matching src block")
      (let ((lang (match-string 1 (buffer-name)))
            (src-content (buffer-substring-no-properties (point-min) (min 100 (point-max)))))
        (euporie-termint-debug-log 'debug "  Looking for language: %s" lang)
        (euporie-termint-debug-log 'debug "  Content snippet: %s" (substring src-content 0 (min 50 (length src-content))))
        
        (dolist (buffer (buffer-list))
          (when (and (not extracted-dir)
                     (with-current-buffer buffer (eq major-mode 'org-mode)))
            (condition-case err
                (with-current-buffer buffer
                  (save-excursion
                    (goto-char (point-min))
                    ;; Search for src blocks with :dir parameter
                    (while (and (not extracted-dir)
                                (re-search-forward (format "^[ \t]*#\\+begin_src[ \t]+%s\\>.*:dir[ \t]+\\([^ \t\n]+\\)" lang) nil t))
                      (let ((found-dir (match-string 1)))
                        (euporie-termint-debug-log 'debug "  Strategy 2 - Found :dir in buffer %s: %s" (buffer-name buffer) found-dir)
                        (setq extracted-dir found-dir)))))
              (error 
               (euporie-termint-debug-log 'debug "  Strategy 2 - Error in buffer %s: %s" (buffer-name buffer) err)))))))
    
    (if extracted-dir
        (euporie-termint-debug-log 'info "✓ Successfully extracted :dir: %s" extracted-dir)
      (euporie-termint-debug-log 'debug "No :dir parameter found, using default directory"))
    
    extracted-dir))

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

(defun euporie-termint-setup-keybinding ()
  "Setup C-RET keybinding for euporie integration."
  
  ;; Unbind Doom default C-RET keybinding globally
  (map! "C-<return>" nil)
  
  ;; Set up hooks for all supported languages
  (add-hook 'python-mode-hook #'euporie-termint-setup-buffer-keybinding)
  (add-hook 'ess-r-mode-hook #'euporie-termint-setup-buffer-keybinding)
  (add-hook 'stata-mode-hook #'euporie-termint-setup-buffer-keybinding)
  (add-hook 'ess-stata-mode-hook #'euporie-termint-setup-buffer-keybinding)
  (add-hook 'SAS-mode-hook #'euporie-termint-setup-buffer-keybinding))

;;; Initialize termint definitions

(defun euporie-termint-setup ()
  "Set up euporie termint definitions."
  (message "euporie-termint: Setting up euporie console definitions")
  
  ;; Configure termint backend - use eat for graphics support
  (setq termint-backend 'eat)
  
  ;; Verify euporie is available
  (let ((euporie-path (or (executable-find "euporie")
                         (when (file-executable-p (expand-file-name "euporie" euporie-termint-project-dir))
                           (expand-file-name "euporie" euporie-termint-project-dir)))))
    
    (if euporie-path
        (progn
          (message "euporie-termint: Found euporie at: %s" euporie-path)
          (euporie-termint-debug-log 'info "Euporie setup completed with path: %s" euporie-path))
      (progn
        (message "euporie-termint: WARNING - No euporie executable found")
        (euporie-termint-debug-log 'warn "No euporie executable found in PATH or project directory")))))

;;; Initialization

;; Initialize after module is loaded
;; Note: Call euporie-termint-setup manually after loading this module

(provide 'euporie-termint)
;;; euporie-termint.el ends here
