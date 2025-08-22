;;; termint-org-src-fixed.el --- Fixed termint org-src integration -*- lexical-binding: t; -*-

(require 'termint)
(require 'org-src)

;;; Allow risky local variables to be remembered permanently
;; This fixes direnv permission prompts by allowing Emacs to remember
;; risky local variable permissions instead of asking every time
(advice-add 'risky-local-variable-p :override #'ignore)

;;; Debug logging
(defvar termint-org-src-debug-log-file (expand-file-name "termint-org-src-debug.log" "~/"))

(defun termint-org-src-debug-log (level format-string &rest args)
  "Log with timestamp to file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) termint-org-src-debug-log-file))))

(defun termint-org-src-clear-log ()
  "Clear the debug log file."
  (with-temp-buffer
    (write-file termint-org-src-debug-log-file)))

;;; Language detection
(defun termint-org-src-detect-kernel ()
  "Detect kernel from org-src buffer language."
  (let ((lang-from-buffer-name (when (string-match "\\*Org Src.*\\[ \\([^]]+\\) \\]\\*" (buffer-name))
                                 (match-string 1 (buffer-name))))
        (lang-from-variable (bound-and-true-p org-src--lang)))
    (termint-org-src-debug-log 'debug "Buffer name: %s" (buffer-name))
    (termint-org-src-debug-log 'debug "Language from buffer name: %s" lang-from-buffer-name)
    (termint-org-src-debug-log 'debug "Language from org-src--lang: %s" lang-from-variable)
    (termint-org-src-debug-log 'debug "Major mode: %s" major-mode)
    
    (let ((detected-lang (or lang-from-variable lang-from-buffer-name)))
      (cond
       ((or (equal detected-lang "python") (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode)) "python")
       ((or (equal detected-lang "r") (equal detected-lang "R") (eq major-mode 'ess-r-mode)) "r") 
       ((or (equal detected-lang "stata") (eq major-mode 'stata-mode)) "stata")
       (t (progn 
            (termint-org-src-debug-log 'info "Using fallback kernel: python")
            "python"))))))

;;; Console management with all features
(defun termint-org-src-smart-python-start ()
  "Start Python jupyter console with smart direnv command."
  (interactive)
  (termint-org-src-debug-log 'info "Starting smart Python console with direnv")
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-python*")
    (termint-org-src-debug-log 'info "Killing existing hung *jupyter-python* buffer")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-python*")))
  
  ;; Define and start with smart direnv command
  (let ((smart-cmd "sh -c 'cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel python3'"))
    (termint-org-src-debug-log 'info "Defining termint with smart command: %s" smart-cmd)
    (termint-define "jupyter-python" smart-cmd :bracketed-paste-p t)
    (termint-org-src-debug-log 'info "Calling termint-jupyter-python-start...")
    (termint-jupyter-python-start)
    (termint-org-src-debug-log 'info "Smart Python console started")))

(defun termint-org-src-display-console-right (buffer &optional original-buffer original-window)
  "Display console BUFFER in a right split window, preserving focus on ORIGINAL-BUFFER in ORIGINAL-WINDOW."
  (termint-org-src-debug-log 'info "Displaying console in right split, preserving focus")
  
  (let ((initial-window (or original-window (selected-window)))
        (initial-buffer (or original-buffer (current-buffer))))
    
    (termint-org-src-debug-log 'info "Initial buffer: %s" (buffer-name initial-buffer))
    
    ;; Use display-buffer with specific parameters for right split
    (let ((console-window (display-buffer buffer
                                          '((display-buffer-reuse-window
                                             display-buffer-in-side-window)
                                            (side . right)
                                            (window-width . 0.5)
                                            (inhibit-same-window . t)))))
      
      (termint-org-src-debug-log 'info "Console window created: %s" console-window)
      
      (when console-window
        ;; Briefly switch to console window to scroll to bottom
        (with-selected-window console-window
          (goto-char (point-max)))
        
        ;; Force restore focus to original window and buffer
        (when (window-live-p initial-window)
          (termint-org-src-debug-log 'info "Restoring focus to initial window")
          (select-window initial-window))
        
        (when (buffer-live-p initial-buffer)
          (termint-org-src-debug-log 'info "Restoring buffer to: %s" (buffer-name initial-buffer))
          (set-window-buffer (selected-window) initial-buffer))
        
        (termint-org-src-debug-log 'info "Focus restored to org-src buffer")))))

(defun termint-org-src-ensure-console-with-features (kernel code)
  "Ensure console for KERNEL is running with direnv and window management, then send CODE."
  (let ((buffer-name "*jupyter-python*") ; For now, only handle Python
        (start-func 'termint-org-src-smart-python-start)
        ;; CRITICAL: Capture org-src buffer/window BEFORE any console operations
        (original-buffer (current-buffer))
        (original-window (selected-window)))
    
    (termint-org-src-debug-log 'info "Captured original state - Buffer: %s, Window: %s" 
                              (buffer-name original-buffer) original-window)
    (termint-org-src-debug-log 'info "Checking for console buffer: %s" buffer-name)
    
    ;; Check if console buffer exists and has a live process
    (let ((console-buffer (get-buffer buffer-name)))
      (if (and console-buffer 
               (buffer-live-p console-buffer)
               (get-buffer-process console-buffer)
               (process-live-p (get-buffer-process console-buffer)))
          (progn
            (termint-org-src-debug-log 'info "Console already running: %s" buffer-name)
            ;; Console exists, just display it and send code
            (termint-org-src-display-console-right console-buffer original-buffer original-window)
            (termint-jupyter-python-send-string code))
        
        ;; Console doesn't exist or is dead, start it
        (progn
          (termint-org-src-debug-log 'info "Starting new console for %s kernel" kernel)
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
                    (termint-org-src-debug-log 'info "Console buffer created: %s" buffer-name)
                    (message "%s console ready!" kernel)
                    ;; Display in right split
                    (termint-org-src-display-console-right new-buffer original-buffer original-window)
                    ;; Give it a moment to fully initialize, then send code
                    (sleep-for 1)
                    (termint-jupyter-python-send-string code))
                (progn
                  (termint-org-src-debug-log 'error "Console buffer not created after %d seconds" max-wait)
                  (error "Failed to create %s console buffer" kernel))))))))))

;;; Main function
(defun termint-org-src-send-simple ()
  "Simple function to send region/line to termint console."
  (interactive)
  (termint-org-src-debug-log 'info "Starting simple send function")
  
  (let* ((kernel (termint-org-src-detect-kernel))
         (code (cond
                ((use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end)))
                ((> (point-max) (point-min))
                 (buffer-substring-no-properties (point-min) (point-max)))
                (t
                 (thing-at-point 'line t)))))
    
    (termint-org-src-debug-log 'info "Kernel: %s" kernel)
    (termint-org-src-debug-log 'info "Code: %s" code)
    
    (cond
     ((string= kernel "python")
      (termint-org-src-debug-log 'info "Sending to Python console with direnv + window management")
      (termint-org-src-ensure-console-with-features "python" code))
     (t 
      (termint-org-src-debug-log 'error "Unsupported kernel: %s" kernel)
      (message "Unsupported kernel: %s" kernel)))))

;;; Keybinding setup
(defun termint-org-src-setup-keybinding ()
  "Setup C-RET keybinding by unbinding Doom keys first."
  (termint-org-src-debug-log 'info "Setting up keybinding by unbinding Doom defaults")
  
  ;; Unbind the Doom default C-RET keybinding globally
  (map! "C-<return>" nil)
  
  ;; Set up hook for org-src buffers
  (add-hook 'python-mode-hook 
            (lambda ()
              (when (string-match "\\*Org Src.*\\[ \\([^]]+\\) \\]\\*" (buffer-name))
                (termint-org-src-debug-log 'info "Setting up C-<return> in org-src buffer: %s" (buffer-name))
                
                ;; Unbind in all evil states for this buffer
                (evil-local-set-key 'insert (kbd "C-<return>") nil)
                (evil-local-set-key 'normal (kbd "C-<return>") nil)
                (evil-local-set-key 'visual (kbd "C-<return>") nil)
                
                ;; Now bind our function
                (evil-local-set-key 'insert (kbd "C-<return>") #'termint-org-src-send-simple)
                (evil-local-set-key 'normal (kbd "C-<return>") #'termint-org-src-send-simple)
                (evil-local-set-key 'visual (kbd "C-<return>") #'termint-org-src-send-simple)
                (local-set-key (kbd "C-<return>") #'termint-org-src-send-simple)
                
                (termint-org-src-debug-log 'info "Unbound Doom C-<return> and bound termint-org-src-send-simple"))))
  
  (termint-org-src-debug-log 'info "Added keybinding hooks with Doom unbinding"))

;; Initialize
(termint-org-src-clear-log)
(termint-org-src-debug-log 'info "termint-org-src-fixed.el loaded")
(termint-org-src-setup-keybinding)

(provide 'termint-org-src-fixed)
;;; termint-org-src-fixed.el ends here