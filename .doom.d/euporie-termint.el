;;; euporie-termint.el --- Euporie console integration using termint and eat -*- lexical-binding: t; -*-

;;; Commentary:
;; Euporie console integration using termint.el with eat backend.
;; Euporie natively handles inline graphics (sixel/kitty/iterm) so we don't need
;; manual img2sixel conversion. This replaces the complex jupyter-termint approach
;; with euporie's built-in terminal graphics support.

;;; Code:

(require 'termint nil t)
(require 'org)
(require 'ob)
(require 'org-src)
(require 'cl-lib)

(defgroup euporie-termint nil
  "Euporie console integration using termint and eat."
  :group 'org-babel)

(defcustom euporie-graphics-protocol "kitty"
  "Graphics protocol to use with euporie.
Valid options: none, sixel, kitty, kitty-unicode, iterm"
  :type '(choice (const "none")
                 (const "sixel") 
                 (const "kitty")
                 (const "kitty-unicode")
                 (const "iterm"))
  :group 'euporie-termint)

(defvar euporie-termint-debug-log-file 
  (expand-file-name "euporie-debug.log" "~/")
  "Log file for euporie integration debugging.")

;;; Logging Functions

(defun euporie-termint-debug-log (level format-string &rest args)
  "Log euporie messages to file with timestamp (disabled for clean output)."
  ;; Debug logging disabled to prevent verbose terminal output
  nil)

;;; Core Console Functions

;;;###autoload
(defun euporie-python ()
  "Start or switch to Python euporie console."
  (interactive)
  (euporie-termint-debug-log 'info "Starting Python euporie console...")
  
  ;; Kill any existing buffer first
  (when (get-buffer "*euporie-python*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*euporie-python*")))
  
  ;; Define the euporie console with nuclear matplotlib suppression
  (let ((euporie-cmd (format "/tmp/euporie-silent.sh %s python3" euporie-graphics-protocol)))
    (euporie-termint-debug-log 'info "Python command: %s" euporie-cmd)
    (termint-define "euporie-python" euporie-cmd
                    :bracketed-paste-p t
                    :backend 'eat)
    (termint-euporie-python-start)))

;;;###autoload
(defun euporie-r ()
  "Start or switch to R euporie console."
  (interactive)
  (euporie-termint-debug-log 'info "Starting R euporie console...")
  
  ;; Kill any existing buffer first
  (when (get-buffer "*euporie-r*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*euporie-r*")))
  
  ;; Define the euporie console with silent launcher
  (let ((euporie-cmd (format "/tmp/euporie-silent.sh %s ir" euporie-graphics-protocol)))
    (euporie-termint-debug-log 'info "R command: %s" euporie-cmd)
    (termint-define "euporie-r" euporie-cmd
                    :bracketed-paste-p t
                    :backend 'eat)
    (termint-euporie-r-start)))

;;;###autoload
(defun euporie-stata ()
  "Start or switch to Stata euporie console."
  (interactive)
  (euporie-termint-debug-log 'info "Starting Stata euporie console...")
  
  ;; Kill any existing buffer first
  (when (get-buffer "*euporie-stata*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*euporie-stata*")))
  
  ;; Define the euporie console with silent launcher
  (let ((euporie-cmd (format "/tmp/euporie-silent.sh %s stata" euporie-graphics-protocol)))
    (euporie-termint-debug-log 'info "Stata command: %s" euporie-cmd)
    (termint-define "euporie-stata" euporie-cmd
                    :bracketed-paste-p t
                    :backend 'eat)
    (termint-euporie-stata-start)))

;;; Org-Babel Integration

;;;###autoload  
(defun euporie-send-region (start end buffer-name)
  "Send region to euporie console buffer."
  (when (get-buffer buffer-name)
    (let ((code (buffer-substring-no-properties start end)))
      (with-current-buffer buffer-name
        (let ((proc (get-buffer-process (current-buffer))))
          (when proc
            (process-send-string proc (concat code "\n"))
            (euporie-termint-debug-log 'info "Sent code to %s: %s" buffer-name (string-trim code))))))))

;;;###autoload
(defun euporie-python-send-region (start end)
  "Send region to Python euporie console."
  (interactive "r")
  (euporie-send-region start end "*euporie-python*"))

;;;###autoload  
(defun euporie-r-send-region (start end)
  "Send region to R euporie console."
  (interactive "r")
  (euporie-send-region start end "*euporie-r*"))

;;;###autoload
(defun euporie-stata-send-region (start end)
  "Send region to Stata euporie console."
  (interactive "r")
  (euporie-send-region start end "*euporie-stata*"))

;;; Debug Functions

;;;###autoload
(defun euporie-debug-info ()
  "Show euporie integration debug information."
  (interactive)
  (message "=== EUPORIE DEBUG INFO ===")
  (message "euporie-console path: %s" (executable-find "euporie-console"))
  (message "Graphics protocol: %s" euporie-graphics-protocol)
  (message "Available buffers:")
  (dolist (buf '("*euporie-python*" "*euporie-r*" "*euporie-stata*"))
    (message "  %s: %s" buf (if (get-buffer buf) "EXISTS" "NOT FOUND")))
  (message "Termint functions available:")
  (message "  termint-euporie-python-start: %s" (fboundp 'termint-euporie-python-start))
  (message "  termint-euporie-r-start: %s" (fboundp 'termint-euporie-r-start))
  (message "  termint-euporie-stata-start: %s" (fboundp 'termint-euporie-stata-start)))

(provide 'euporie-termint)

;;; euporie-termint.el ends here