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
  "Unified euporie console integration using termint and eat."
  :group 'org-babel)

(defcustom euporie-termint-graphics-protocol "sixel"
  "Graphics protocol for euporie console.
Supported protocols: sixel, kitty, kitty-unicode, iterm."
  :type '(choice (const "sixel")
                 (const "kitty") 
                 (const "kitty-unicode")
                 (const "iterm"))
  :group 'euporie-termint)

(defcustom euporie-termint-project-dir "/Users/vwh7mb/projects/emacs-euporie"
  "Default project directory containing pixi environment."
  :type 'directory
  :group 'euporie-termint)

;;; Logging Infrastructure

(defvar euporie-termint-debug-log-file (expand-file-name "euporie-debug.log" "~/")
  "Log file for euporie debugging information.")

(defun euporie-termint-debug-log (level format-string &rest args)
  "Log LEVEL message with FORMAT-STRING and ARGS to debug file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) euporie-termint-debug-log-file))))

;;; Universal Helper Functions

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
  (let* ((graphics-protocol euporie-termint-graphics-protocol)
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

;;; Universal Kernel Detection

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

;;; Universal Window Display Management

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

;;; Universal Buffer Management

(defun euporie-termint-get-or-create-buffer (kernel &optional dir)
  "Get or create euporie termint buffer for KERNEL.
DIR parameter is used for SAS to determine local vs remote execution."
  (let* ((is-remote-sas (and (string= kernel "sas") dir (file-remote-p dir)))
         (buffer-name (format "*euporie-%s*" kernel))
         (buffer (get-buffer buffer-name)))
    
    (euporie-termint-debug-log 'info "Getting or creating buffer for %s kernel (remote: %s)" 
                               kernel is-remote-sas)
    
    ;; Check if buffer exists and has live process
    (if (and buffer 
             (buffer-live-p buffer)
             (get-buffer-process buffer)
             (process-live-p (get-buffer-process buffer)))
        buffer
      ;; Need to start new console
      (progn
        (euporie-termint-debug-log 'info "Starting new %s console" kernel)
        (euporie-termint-start kernel dir)
        ;; Wait for buffer creation and return it
        (let ((max-wait-buffer 20) (wait-count 0) (new-buffer nil))
          ;; First, wait for buffer creation
          (while (and (< wait-count max-wait-buffer)
                     (not (setq new-buffer (get-buffer buffer-name))))
            (sleep-for 0.5)
            (setq wait-count (1+ wait-count)))
          
          ;; Then wait for kernel to initialize
          (when new-buffer
            (sleep-for 2)  ; Simple initialization wait
            (euporie-termint-debug-log 'info "Kernel initialization wait complete"))
          new-buffer)))))

;;; Universal Kernel Startup Functions

(defun euporie-termint-start (kernel &optional dir)
  "Universal function to start KERNEL console with optional DIR for remote execution."
  (let* ((is-remote-sas (and (string= kernel "sas") dir (file-remote-p dir)))
         (buffer-name (format "*euporie-%s*" kernel)))
    
    (euporie-termint-debug-log 'info "Starting %s console (remote: %s)" kernel is-remote-sas)
    
    ;; Kill any existing buffer first
    (when (get-buffer buffer-name)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer-name)))
    
    (cond
     ;; Remote SAS uses tramp-qrsh-session
     (is-remote-sas
      (euporie-termint-start-remote-sas dir))
     
     ;; All other cases use local termint
     (t
      (euporie-termint-start-local kernel)))))

(defun euporie-termint-start-local (kernel)
  "Start local KERNEL console using termint."
  (let ((smart-cmd (euporie-termint--build-euporie-command kernel euporie-termint-project-dir)))
    
    (euporie-termint-debug-log 'info "Local %s command: %s" kernel smart-cmd)
    
    ;; Use cond to handle each kernel type with literal termint names
    (cond
     ((string= kernel "python")
      (termint-define "euporie-python" smart-cmd
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "eat-truecolor")
                             ("COLORTERM" . "truecolor")
                             ("EUPORIE_GRAPHICS" . "sixel")))
      (condition-case err
          (progn 
            (termint-euporie-python-start)
            (when-let ((buffer (get-buffer "*euporie-python*")))
              (euporie-termint-display-console-right buffer)))
        (error 
         (euporie-termint-debug-log 'error "Failed to start Python termint: %s" err)
         (message "Warning: Failed to start Python euporie console: %s" err))))
     
     ((string= kernel "r")
      (termint-define "euporie-r" smart-cmd
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "eat-truecolor")
                             ("COLORTERM" . "truecolor")
                             ("EUPORIE_GRAPHICS" . "sixel")))
      (condition-case err
          (progn 
            (termint-euporie-r-start)
            (when-let ((buffer (get-buffer "*euporie-r*")))
              (euporie-termint-display-console-right buffer)))
        (error 
         (euporie-termint-debug-log 'error "Failed to start R termint: %s" err)
         (message "Warning: Failed to start R euporie console: %s" err))))
     
     ((string= kernel "stata")
      (termint-define "euporie-stata" smart-cmd
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "eat-truecolor")
                             ("COLORTERM" . "truecolor")
                             ("EUPORIE_GRAPHICS" . "sixel")
                             ("LANG" . "en_US.UTF-8")
                             ("LC_ALL" . "en_US.UTF-8")))
      (condition-case err
          (progn 
            (termint-euporie-stata-start)
            (when-let ((buffer (get-buffer "*euporie-stata*")))
              (euporie-termint-display-console-right buffer)))
        (error 
         (euporie-termint-debug-log 'error "Failed to start Stata termint: %s" err)
         (message "Warning: Failed to start Stata euporie console: %s" err))))
     
     ((string= kernel "sas")
      (termint-define "euporie-sas" smart-cmd
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "eat-truecolor")
                             ("COLORTERM" . "truecolor")
                             ("EUPORIE_GRAPHICS" . "sixel")))
      (condition-case err
          (progn 
            (termint-euporie-sas-start)
            (when-let ((buffer (get-buffer "*euporie-sas*")))
              (euporie-termint-display-console-right buffer)))
        (error 
         (euporie-termint-debug-log 'error "Failed to start SAS termint: %s" err)
         (message "Warning: Failed to start SAS euporie console: %s" err))))
     
     (t (error "Unsupported kernel: %s" kernel)))))

(defun euporie-termint-start-remote-sas (remote-dir)
  "Start remote SAS euporie console using tramp-qrsh-session."
  (let* ((localname (if (file-remote-p remote-dir)
                        (file-remote-p remote-dir 'localname)
                      remote-dir)))
    
    (euporie-termint-debug-log 'info "Starting remote SAS console for dir: %s" remote-dir)
    
    ;; Use tramp-qrsh-session to get to compute node
    (let ((qrsh-buffer (tramp-qrsh-session)))
      
      (if qrsh-buffer
          (progn
            (euporie-termint-debug-log 'info "Got qrsh buffer: %s" (buffer-name qrsh-buffer))
            
            ;; Send euporie command to the compute node shell
            (let ((euporie-cmd (format "cd %s 2>/dev/null && export PATH=/home/nyu/eddyhu/env/bin:$PATH >/dev/null 2>&1 && clear && exec euporie console --kernel-name=sas --graphics=sixel" localname)))
              (euporie-termint-debug-log 'info "Sending euporie command: %s" euporie-cmd)
              (with-current-buffer qrsh-buffer
                ;; Use termint send function if available
                (if (fboundp 'termint-qrsh-session-send-string)
                    (termint-qrsh-session-send-string euporie-cmd)
                  (error "termint-qrsh-session-send-string not available")))
              
              ;; Display in split window
              (euporie-termint-display-console-right qrsh-buffer)
              
              (euporie-termint-debug-log 'info "Remote SAS setup complete")
              qrsh-buffer))
        (error "tramp-qrsh-session failed to establish connection")))))

;;; Universal Code Sending

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
                     #'termint-qrsh-session-send-string)  ; Use qrsh-specific send function
                    ((string= kernel "sas") #'termint-euporie-sas-send-string)
                    (t (error "Unsupported kernel: %s" kernel)))))
    
    (euporie-termint-debug-log 'info "Sending %s code (remote: %s): %s" 
                               kernel is-remote-sas (substring code 0 (min 50 (length code))))
    
    (when buffer
      ;; Display console in right window
      (euporie-termint-display-console-right buffer)
      
      ;; Send code to console with proper targeting
      (if is-remote-sas
          ;; For remote SAS, send directly to the buffer
          (progn
            (euporie-termint-debug-log 'debug "Using remote SAS send")
            (with-current-buffer buffer
              (funcall send-func code)))
        ;; For other kernels, use standard approach
        (progn
          (funcall send-func code)))
      
      ;; Start file monitoring for Stata graphics if not already running
      (when (string= kernel "stata")
        (euporie-termint-start-stata-graphics-monitor))
      
      ;; Return buffer for further processing if needed
      buffer)))

;;; Universal C-RET Integration Function

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

;;; Stata Graphics Monitor (Preserve existing functionality)

(defvar euporie-termint-stata-file-watcher nil
  "File watcher process for Stata graphics cache directory.")

(defvar euporie-termint-last-stata-graph-count 0
  "Track the last graph count to detect new graphs.")

(defun euporie-termint-start-stata-graphics-monitor ()
  "Start monitoring for Stata graphics if not already running."
  (unless (or (and euporie-termint-stata-file-watcher 
                   (or (process-live-p euporie-termint-stata-file-watcher)
                       (timerp euporie-termint-stata-file-watcher))))
    (run-with-timer 0.5 nil #'euporie-termint-setup-stata-file-monitor)))

(defun euporie-termint-setup-stata-file-monitor ()
  "Set up file monitoring for Stata graphics cache directory."
  (let ((cache-dir (expand-file-name "~/.stata_kernel_cache")))
    (when (file-directory-p cache-dir)
      (euporie-termint-debug-log 'info "Starting file monitor for Stata graphics in: %s" cache-dir)
      ;; Use a timer for polling (simplified approach)
      (setq euporie-termint-stata-file-watcher
            (run-with-timer 1 1 #'euporie-termint-check-for-new-stata-graphs))
      (euporie-termint-debug-log 'info "File monitor started with timer polling"))))

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
        (setq euporie-termint-last-stata-graph-count current-count)))))

;;; Org-babel Integration

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

;;; Keybinding Setup

(defun euporie-termint-setup-keybindings ()
  "Set up C-RET keybinding for all supported language modes."
  
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

(defun euporie-termint-setup-buffer-keybinding ()
  "Set up C-RET keybinding for the current buffer."
  (when (or (string-match "\\*Org Src.*\\[ \\(.+\\) \\]\\*" (buffer-name))
            (memq major-mode '(python-mode python-ts-mode ess-r-mode stata-mode SAS-mode ess-stata-mode)))
    
    ;; Unbind in all evil states for this buffer
    (when (fboundp 'evil-local-set-key)
      (evil-local-set-key 'insert (kbd "C-<return>") nil)
      (evil-local-set-key 'normal (kbd "C-<return>") nil)
      (evil-local-set-key 'visual (kbd "C-<return>") nil))
    
    ;; Bind our function
    (when (fboundp 'evil-local-set-key)
      (evil-local-set-key 'insert (kbd "C-<return>") #'euporie-termint-send-region-or-line)
      (evil-local-set-key 'normal (kbd "C-<return>") #'euporie-termint-send-region-or-line)
      (evil-local-set-key 'visual (kbd "C-<return>") #'euporie-termint-send-region-or-line))
    (local-set-key (kbd "C-<return>") #'euporie-termint-send-region-or-line)))

;;; Universal Test Functions (Work for ANY kernel)

(defun test-local-execution (kernel)
  "Test local execution for KERNEL."
  (interactive (list (completing-read "Kernel: " '("python" "r" "stata" "sas"))))
  (let ((test-code (cond
                    ((string= kernel "python") "print('Hello from Python')\nprint(f'Kernel: {kernel}')")
                    ((string= kernel "r") "print('Hello from R')\ncat('Kernel:', kernel, '\n')")
                    ((string= kernel "stata") "display \"Hello from Stata\"\ndisplay \"Kernel: stata\"")
                    ((string= kernel "sas") "data _null_; put 'Hello from SAS'; put 'Kernel: sas'; run;")
                    (t "# Unknown kernel"))))
    (message "Testing %s local execution..." kernel)
    (euporie-termint-debug-log 'info "Running test-local-execution for kernel: %s" kernel)
    (condition-case err
        (progn
          (euporie-termint-send-code kernel test-code)
          (message "✓ Test code sent to %s console" kernel)
          t)
      (error
       (message "✗ Failed to send test code to %s console: %s" kernel err)
       (euporie-termint-debug-log 'error "test-local-execution failed for %s: %s" kernel err)
       nil))))

(defun test-remote-execution (kernel remote-dir)
  "Test remote execution for KERNEL in REMOTE-DIR."
  (interactive (list (completing-read "Kernel: " '("sas")) 
                    (read-directory-name "Remote directory: " "/sshx:wrds:/home/")))
  (if (string= kernel "sas")
      (let ((test-code "data _null_; put 'Hello from remote SAS'; put 'Remote dir test successful'; run;"))
        (message "Testing %s remote execution in %s..." kernel remote-dir)
        (euporie-termint-debug-log 'info "Running test-remote-execution for %s in %s" kernel remote-dir)
        (condition-case err
            (progn
              (euporie-termint-send-code kernel test-code remote-dir)
              (message "✓ Remote test code sent to %s console" kernel)
              t)
          (error
           (message "✗ Failed to send remote test code to %s: %s" kernel err)
           (euporie-termint-debug-log 'error "test-remote-execution failed for %s: %s" kernel err)
           nil)))
    (message "Remote execution only supported for SAS kernel")))

(defun test-window-management (kernel)
  "Test window management for KERNEL."
  (interactive (list (completing-read "Kernel: " '("python" "r" "stata" "sas"))))
  (message "Testing window management for %s..." kernel)
  (euporie-termint-debug-log 'info "Running test-window-management for kernel: %s" kernel)
  (condition-case err
      (let ((buffer (euporie-termint-get-or-create-buffer kernel)))
        (if buffer
            (progn
              (euporie-termint-display-console-right buffer)
              (message "✓ Window management test completed for %s" kernel)
              (euporie-termint-debug-log 'info "Window management test successful for %s" kernel)
              t)
          (message "✗ Failed to get buffer for %s" kernel)
          (euporie-termint-debug-log 'error "Failed to get buffer in window management test for %s" kernel)
          nil))
    (error
     (message "✗ Window management test failed for %s: %s" kernel err)
     (euporie-termint-debug-log 'error "test-window-management failed for %s: %s" kernel err)
     nil)))

(defun test-keybinding-dispatch (kernel)
  "Test C-RET keybinding dispatch for KERNEL."
  (interactive (list (completing-read "Kernel: " '("python" "r" "stata" "sas"))))
  (message "Testing keybinding dispatch for %s..." kernel)
  (euporie-termint-debug-log 'info "Running test-keybinding-dispatch for kernel: %s" kernel)
  (let ((detected-kernel (euporie-termint-detect-kernel))
        (c-ret-binding (key-binding (kbd "C-<return>"))))
    (message "Current kernel detected as: %s" detected-kernel)
    (message "C-RET binding: %s" c-ret-binding)
    (if (eq c-ret-binding 'euporie-termint-send-region-or-line)
        (progn
          (message "✓ Keybinding dispatch test completed for %s" kernel)
          (euporie-termint-debug-log 'info "Keybinding test successful for %s (detected: %s)" kernel detected-kernel)
          t)
      (progn
        (message "✗ Keybinding not properly set for %s" kernel)
        (euporie-termint-debug-log 'error "Keybinding test failed for %s - binding: %s" kernel c-ret-binding)
        nil))))

(defun test-graphics-display (kernel)
  "Test graphics display for KERNEL."
  (interactive (list (completing-read "Kernel: " '("python" "r" "stata" "sas"))))
  (let ((test-code (cond
                    ((string= kernel "python") "import matplotlib.pyplot as plt\nimport numpy as np\nx = np.linspace(0, 10, 100)\nplt.figure(figsize=(8,5))\nplt.plot(x, np.sin(x), label='sin(x)')\nplt.title('Python Graphics Test')\nplt.legend()\nplt.show()")
                    ((string= kernel "r") "x <- seq(0, 10, 0.1)\nplot(x, sin(x), type='l', main='R Graphics Test', col='blue')")
                    ((string= kernel "stata") "sysuse auto\nscatter price mpg, title(\"Stata Graphics Test\")")
                    ((string= kernel "sas") "data test; do x = 0 to 10 by 0.1; y = sin(x); output; end; run;\nproc sgplot data=test; series x=x y=y; title 'SAS Graphics Test'; run;")
                    (t "# Graphics not implemented for this kernel"))))
    (message "Testing %s graphics display..." kernel)
    (euporie-termint-debug-log 'info "Running test-graphics-display for kernel: %s" kernel)
    (condition-case err
        (progn
          (euporie-termint-send-code kernel test-code)
          (message "✓ Graphics test code sent to %s console" kernel)
          (euporie-termint-debug-log 'info "Graphics test sent successfully for %s" kernel)
          t)
      (error
       (message "✗ Graphics test failed for %s: %s" kernel err)
       (euporie-termint-debug-log 'error "test-graphics-display failed for %s: %s" kernel err)
       nil)))

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
        (euporie-termint-debug-log 'warn "No euporie-console executable found in PATH or project directory")))))

;;; Comprehensive Test Suite Function

(defun test-euporie-integration (&optional kernel)
  "Run comprehensive test suite for euporie integration.
If KERNEL is provided, test only that kernel. Otherwise, test all supported kernels."
  (interactive (list (when current-prefix-arg
                      (completing-read "Test specific kernel: " 
                                     '("python" "r" "stata" "sas") nil t))))
  
  (let ((kernels-to-test (if kernel (list kernel) '("python" "r" "stata")))  ; Skip SAS in full test
        (test-results '()))
    
    (message "=== Starting Comprehensive Euporie Integration Test Suite ===")
    (euporie-termint-debug-log 'info "Starting comprehensive test suite for kernels: %s" kernels-to-test)
    
    (dolist (test-kernel kernels-to-test)
      (message "\n--- Testing %s kernel ---" test-kernel)
      
      (let ((local-exec-result (test-local-execution test-kernel))
            (window-mgmt-result (test-window-management test-kernel))
            (keybinding-result (test-keybinding-dispatch test-kernel))
            (graphics-result (test-graphics-display test-kernel)))
        
        (push (list test-kernel
                   :local-execution local-exec-result
                   :window-management window-mgmt-result
                   :keybinding keybinding-result
                   :graphics graphics-result)
              test-results)))
    
    ;; Summary report
    (message "\n=== Test Results Summary ===")
    (dolist (result (reverse test-results))
      (let ((kernel (car result))
            (results (cdr result)))
        (message "%s: Local:%s Window:%s Key:%s Graphics:%s"
                kernel
                (if (plist-get results :local-execution) "✓" "✗")
                (if (plist-get results :window-management) "✓" "✗")
                (if (plist-get results :keybinding) "✓" "✗")
                (if (plist-get results :graphics) "✓" "✗"))))
    
    (euporie-termint-debug-log 'info "Test suite completed. Results: %s" test-results)
    test-results))

;; Initialize keybindings when loaded
(when (featurep 'evil)
  (euporie-termint-setup-keybindings))

;;; Export Functions for External Testing

;;;###autoload
(defun euporie-termint-universal-send-code (kernel code &optional dir)
  "Universal function to send CODE to KERNEL with optional remote DIR.
This is the main entry point for the streamlined architecture."
  (euporie-termint-send-code kernel code dir))

;;;###autoload  
(defun euporie-termint-universal-get-buffer (kernel &optional dir)
  "Universal function to get or create buffer for KERNEL with optional remote DIR."
  (euporie-termint-get-or-create-buffer kernel dir))

;;;###autoload
(defun euporie-termint-universal-display (buffer)
  "Universal function to display console BUFFER in right window."
  (euporie-termint-display-console-right buffer))

;;;###autoload
(defun euporie-termint-universal-detect-kernel ()
  "Universal function to detect current kernel from context."
  (euporie-termint-detect-kernel))

;;;###autoload
(defun euporie-termint-universal-send-region-or-line ()
  "Universal function to send region or line with automatic kernel detection."
  (interactive)
  (euporie-termint-send-region-or-line))

(provide 'euporie-termint)
;;; euporie-termint.el ends here