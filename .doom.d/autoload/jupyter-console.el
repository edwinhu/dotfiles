;;; ~/.doom.d/autoload/jupyter-console.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jupyter-console/override-babel-python ()
  "Override org-babel-execute:python to use jupyter-console."
  (require 'jupyter-console)
  (defun org-babel-execute:python (body params)
    "Execute Python BODY with PARAMS using jupyter console."
    (jupyter-console-log 'info "Executing Python code block (autoload override)")
    (let* ((file (buffer-file-name))
           (buffer (jupyter-console-get-or-create "python3" file))
           (result (jupyter-console-send-string buffer body)))
      (jupyter-console-log 'info "Python execution completed")
      result)))

;;;###autoload
(defun +jupyter-console/override-babel-stata ()
  "Override org-babel-execute:stata to use jupyter-console."
  (require 'jupyter-console)
  (defun org-babel-execute:stata (body params)
    "Execute Stata BODY with PARAMS using jupyter console."
    (jupyter-console-log 'info "Executing Stata code block (autoload override)")
    (let* ((file (buffer-file-name))
           (buffer (jupyter-console-get-or-create "stata" file))
           (result (jupyter-console-send-string buffer body)))
      (jupyter-console-log 'info "Stata execution completed")
      result)))

;;;###autoload
(defun +jupyter-console/override-babel-r ()
  "Override org-babel-execute:R to use jupyter-console."
  (require 'jupyter-console)
  (defun org-babel-execute:R (body params)
    "Execute R BODY with PARAMS using jupyter console."
    (jupyter-console-log 'info "Executing R code block (autoload override)")
    (let* ((file (buffer-file-name))
           (buffer (jupyter-console-get-or-create "ir" file))
           (result (jupyter-console-send-string buffer body)))
      (jupyter-console-log 'info "R execution completed via jupyter console")
      result)))

;;;###autoload
(defun +jupyter-console/init ()
  "Initialize jupyter-console overrides."
  (require 'jupyter-console)
  (+jupyter-console/override-babel-python)
  (+jupyter-console/override-babel-stata)
  (+jupyter-console/override-babel-r))