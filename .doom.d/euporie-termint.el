;;; euporie-termint.el --- Euporie console integration using termint and eat -*- lexical-binding: t; -*-

;;; Commentary:
;; Euporie console integration using termint.el with eat backend for native graphics support
;; EUPORIE HANDLES GRAPHICS NATIVELY - no manual conversion needed
;; Uses euporie-console with --graphics=sixel for inline display

;;; Code:

(require 'termint nil t)
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

(defcustom euporie-termint-project-dir "/Users/vwh7mb/projects/wander2"
  "Default project directory containing pixi environment."
  :type 'directory
  :group 'euporie-termint)

(defvar euporie-termint-debug-log-file (expand-file-name "euporie-debug.log" "~/")
  "Log file for euporie debugging information.")

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
                          graphics-protocol kernel)))
    (if (euporie-termint--check-direnv-allowed project-dir)
        (format "sh -c 'cd %s && direnv exec . %s'" project-dir base-cmd)
      (format "sh -c 'cd %s && %s'" project-dir base-cmd))))

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
                    :env '(("TERM" . "xterm-kitty") 
                           ("COLORTERM" . "truecolor")
                           ("EUPORIE_GRAPHICS" . "sixel")))
    (termint-euporie-python-start)))

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
                    :env '(("TERM" . "xterm-kitty") 
                           ("COLORTERM" . "truecolor")
                           ("EUPORIE_GRAPHICS" . "sixel")))
    (termint-euporie-r-start)))

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
                    :env '(("TERM" . "xterm-kitty")             ; Consistent TERM with Python/R
                           ("COLORTERM" . "truecolor")
                           ("EUPORIE_GRAPHICS" . "kitty")       ; Use kitty protocol for Stata
                           ("LANG" . "en_US.UTF-8")            ; Explicit locale for Unicode
                           ("LC_ALL" . "en_US.UTF-8")))        ; Full locale support
    (termint-euporie-stata-start)))

;;; Buffer Management

(defun euporie-termint-get-or-create-buffer (kernel)
  "Get or create euporie termint buffer for KERNEL."
  (let* ((buffer-name (format "*euporie-%s*" kernel))
         (start-func (cond
                     ((string= kernel "python") #'euporie-python-start)
                     ((string= kernel "r") #'euporie-r-start)
                     ((string= kernel "stata") #'euporie-stata-start)
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
        ;; Wait for buffer creation
        (let ((max-wait 15) (wait-count 0))
          (while (and (< wait-count max-wait)
                     (not (get-buffer buffer-name)))
            (sleep-for 0.5)
            (setq wait-count (1+ wait-count))))
        (get-buffer buffer-name)))))

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
            (set-process-filter euporie-termint-stata-file-watcher #'euporie-termint-stata-file-event-handler))
        ;; Fallback: use a timer for polling
        (run-with-timer 1 1 #'euporie-termint-check-new-stata-graphs)))))

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
      
      ;; Display the graph using chafa in the Stata buffer
      (euporie-termint-display-stata-graph newest-file))))

(defun euporie-termint-display-stata-graph (png-file)
  "Display PNG-FILE in the Stata euporie console using chafa."
  (when (and (file-exists-p png-file) (get-buffer "*euporie-stata*"))
    (let ((chafa-command (format "! chafa \"%s\"" png-file)))
      (euporie-termint-debug-log 'info "Displaying Stata graph: %s" chafa-command)
      (termint-euporie-stata-send-string chafa-command))))

;;; Code Execution

(defun euporie-termint-send-code (kernel code)
  "Send CODE to euporie console for KERNEL."
  (let* ((buffer (euporie-termint-get-or-create-buffer kernel))
         (send-func (cond
                    ((string= kernel "python") #'termint-euporie-python-send-string)
                    ((string= kernel "r") #'termint-euporie-r-send-string)
                    ((string= kernel "stata") #'termint-euporie-stata-send-string)
                    (t (error "Unsupported kernel: %s" kernel)))))
    
    (when buffer
      (euporie-termint-debug-log 'info "Sending %s code: %s" kernel (substring code 0 (min 50 (length code))))
      
      ;; Display console in right window
      (euporie-termint-display-console-right buffer)
      
      ;; Send code to console
      (funcall send-func code)
      
      ;; Start file monitoring for Stata graphics if not already running
      (when (and (string= kernel "stata") 
                 (not euporie-termint-stata-file-watcher))
        (run-with-timer 0.5 nil #'euporie-termint-start-stata-file-monitor))
      
      ;; Return buffer for further processing if needed
      buffer)))

;;; Main C-RET Integration Function

(defun euporie-termint-send-region-or-line ()
  "Send current region or line to euporie console with automatic kernel detection."
  (interactive)
  
  (let* ((kernel (euporie-termint-detect-kernel))
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
      (euporie-termint-debug-log 'info "Executing %s code via euporie" kernel)
      (euporie-termint-send-code kernel code))))

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

;;; Keybinding Setup

(defun euporie-termint-setup-keybinding ()
  "Setup C-RET keybinding for euporie integration."
  
  ;; Unbind Doom default C-RET keybinding globally
  (map! "C-<return>" nil)
  
  ;; Common function to set up keybindings in org-src buffers
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

  ;; Set up hooks for all supported languages
  (add-hook 'python-mode-hook #'euporie-termint-setup-buffer-keybinding)
  (add-hook 'ess-r-mode-hook #'euporie-termint-setup-buffer-keybinding)
  (add-hook 'stata-mode-hook #'euporie-termint-setup-buffer-keybinding)
  (add-hook 'ess-stata-mode-hook #'euporie-termint-setup-buffer-keybinding))

;;; Initialize termint definitions

(defun euporie-termint-setup ()
  "Set up euporie termint definitions."
  (message "euporie-termint: Setting up euporie console definitions")
  
  ;; Configure termint backend - use eat for graphics support
  (setq termint-backend 'eat)
  
  ;; Verify euporie is available
  (let ((euporie-path (or (executable-find "euporie-console")
                         (when (file-executable-p (expand-file-name "euporie-console" euporie-termint-project-dir))
                           (expand-file-name "euporie-console" euporie-termint-project-dir)))))
    
    (if euporie-path
        (progn
          (message "euporie-termint: Found euporie at: %s" euporie-path)
          (euporie-termint-debug-log 'info "Euporie setup completed with path: %s" euporie-path))
      (progn
        (message "euporie-termint: WARNING - No euporie-console executable found")
        (euporie-termint-debug-log 'warn "No euporie-console executable found in PATH or project directory")))))

;;; Initialization

(with-eval-after-load 'termint
  (euporie-termint-setup)
  (euporie-termint-setup-keybinding))

(provide 'euporie-termint)
;;; euporie-termint.el ends here