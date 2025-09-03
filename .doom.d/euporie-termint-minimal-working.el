;;; euporie-termint-minimal.el --- Minimal working euporie termint integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal working version for testing function availability

;;; Code:

(require 'org)

(defun euporie-termint-send-region-or-line ()
  "Send current region or line to euporie console with automatic kernel detection."
  (interactive)
  (message "euporie-termint-send-region-or-line called"))

(defun euporie-termint-setup-keybinding ()
  "Setup C-RET keybinding for euporie integration."
  (interactive)
  (message "euporie-termint-setup-keybinding called"))

(defun euporie-termint-setup ()
  "Set up euporie termint definitions."
  (interactive)
  (message "euporie-termint-setup called"))

(provide 'euporie-termint)
;;; euporie-termint-minimal.el ends here