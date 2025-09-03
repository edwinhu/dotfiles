;;; test-euporie-simplified.el --- Simplified version to test loading

;;; Commentary:
;; Simplified version to identify the loading issue

;;; Code:

(require 'termint nil t)
(require 'tramp-qrsh nil t)
(require 'org)
(require 'ob)

(message "Test: Basic requires loaded")

(defun test-euporie-termint-send-region-or-line ()
  "Send current region or line to euporie console with automatic kernel detection."
  (interactive)
  (message "Function works!"))

(message "Test: Main function defined")

(defun test-euporie-termint-setup-keybinding ()
  "Setup C-RET keybinding for euporie integration."
  (message "Setup function works!"))

(message "Test: Setup function defined")

(defun test-euporie-termint-setup ()
  "Set up euporie termint definitions."
  (message "euporie-termint: Setting up euporie console definitions"))

(message "Test: All functions defined")

(provide 'test-euporie-simplified)
;;; test-euporie-simplified.el ends here