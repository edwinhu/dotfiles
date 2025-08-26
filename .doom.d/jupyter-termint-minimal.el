;;; jupyter-termint-minimal.el --- Minimal Jupyter termint with vterm backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal version focused on testing vterm backend for sixel graphics

;;; Code:

(require 'termint nil t)
(require 'org)

(defvar jupyter-termint-debug-log-file 
  (expand-file-name "jupyter-minimal-debug.log" "~/")
  "Log file for debugging.")

(defun jupyter-termint-debug-log (level format-string &rest args)
  "Log with timestamp to file."
  (let ((message (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
      (append-to-file (point-min) (point-max) jupyter-termint-debug-log-file))))

(defun jupyter-termint-smart-stata-start ()
  "Start Stata jupyter console with vterm backend."
  (interactive)
  
  ;; Kill any existing hung buffer first
  (when (get-buffer "*jupyter-stata*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*jupyter-stata*")))
  
  ;; Start with vterm backend for sixel support
  (let ((smart-cmd "sh -c \"export TERM=xterm-kitty COLORTERM=truecolor JUPYTER_CONSOLE=1 && cd /Users/vwh7mb/projects/wander2 && direnv exec . pixi run jupyter console --kernel stata\""))
    (jupyter-termint-debug-log 'info "Starting Stata console with command: %s" smart-cmd)
    (termint-define "jupyter-stata" smart-cmd 
                    :bracketed-paste-p t
                    :backend 'vterm
                    :env '(("TERM" . "xterm-kitty") ("COLORTERM" . "truecolor") ("JUPYTER_CONSOLE" . "1")))
    (termint-jupyter-stata-start)))

(defun jupyter-termint-send-stata ()
  "Send current region/line to Stata console."
  (interactive)
  
  (let* ((code (cond
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
    
    ;; Ensure console is running
    (let ((buffer-name "*jupyter-stata*"))
      (let ((console-buffer (get-buffer buffer-name)))
        (if (and console-buffer 
                 (buffer-live-p console-buffer)
                 (get-buffer-process console-buffer)
                 (process-live-p (get-buffer-process console-buffer)))
            (progn
              ;; Console exists, just send code
              (termint-jupyter-stata-send-string code))
          
          ;; Console doesn't exist, start it
          (progn
            (message "Starting Stata console with vterm backend...")
            (jupyter-termint-smart-stata-start)
            
            ;; Wait for buffer to be created
            (let ((max-wait 10) (wait-count 0))
              (while (and (< wait-count max-wait)
                         (not (get-buffer buffer-name)))
                (sleep-for 0.5)
                (setq wait-count (1+ wait-count)))
              
              (let ((new-buffer (get-buffer buffer-name)))
                (if new-buffer
                    (progn
                      (message "Stata console ready!")
                      (sleep-for 2)
                      (termint-jupyter-stata-send-string code))
                  (error "Failed to create Stata console buffer"))))))))))

(defun jupyter-termint-create-split-layout (buffer-name)
  "Create split window layout with jupyter buffer on the right."
  (when (get-buffer buffer-name)
    ;; Save current window configuration
    (let ((original-window (selected-window))
          (jupyter-window nil))
      
      ;; Check if jupyter buffer is already visible
      (setq jupyter-window (get-buffer-window buffer-name))
      
      (unless jupyter-window
        ;; Split window if not already done
        (when (= 1 (length (window-list)))
          (split-window-right))
        
        ;; Display jupyter buffer in right window
        (other-window 1)
        (switch-to-buffer buffer-name)
        (setq jupyter-window (selected-window))
        
        ;; Return to original window
        (select-window original-window))
      
      jupyter-window)))

(defun jupyter-termint-send-stata-with-split ()
  "Send Stata code and create/show split layout."
  (interactive)
  (jupyter-termint-send-stata)
  (jupyter-termint-create-split-layout "*jupyter-stata*"))

(provide 'jupyter-termint-minimal)
;;; jupyter-termint-minimal.el ends here