;;; test-jupyter-doom.el --- Test jupyter-termint with full Doom environment

(defun test-jupyter-with-doom ()
  "Test jupyter-termint functions with full Doom environment loaded."
  (message "=== TESTING JUPYTER WITH FULL DOOM ENVIRONMENT ===")
  
  ;; Check if Doom is properly loaded
  (message "Doom loaded: %s" (boundp 'doom-version))
  (message "Termint available: %s" (featurep 'termint))
  (message "Jupyter-termint available: %s" (featurep 'jupyter-termint))
  
  ;; List initial buffers
  (message "Initial buffers: %s" (mapcar #'buffer-name (buffer-list)))
  
  ;; Test function availability
  (message "\nFunction availability:")
  (let ((functions '(termint-jupyter-python-start
                    termint-jupyter-r-start
                    termint-jupyter-stata-start
                    termint-jupyter-python-start-kernel
                    termint-jupyter-r-start-kernel
                    termint-jupyter-stata-start-kernel)))
    (dolist (func functions)
      (message "  %s: %s" func (if (fboundp func) "✓" "✗"))))
  
  ;; Test each function if available
  (message "\n--- Testing Python ---")
  (if (fboundp 'termint-jupyter-python-start)
      (condition-case err
          (progn
            (if noninteractive
                (message "Skipping buffer creation in batch mode")
              (progn
                (termint-jupyter-python-start)
                (message "✓ Python function called")))
            (message "✓ Python function exists and is callable"))
        (error (message "✗ Python error: %s" err)))
    (message "✗ Python function not available"))
  
  (message "\n--- Testing R ---")
  (if (fboundp 'termint-jupyter-r-start)
      (condition-case err
          (progn
            (if noninteractive
                (message "Skipping buffer creation in batch mode")
              (progn
                (termint-jupyter-r-start)
                (message "✓ R function called")))
            (message "✓ R function exists and is callable"))
        (error (message "✗ R error: %s" err)))
    (message "✗ R function not available"))
  
  (message "\n--- Testing Stata ---")
  (if (fboundp 'termint-jupyter-stata-start)
      (condition-case err
          (progn
            (if noninteractive
                (message "Skipping buffer creation in batch mode")
              (progn
                (termint-jupyter-stata-start)
                (message "✓ Stata function called")))
            (message "✓ Stata function exists and is callable"))
        (error (message "✗ Stata error: %s" err)))
    (message "✗ Stata function not available"))
  
  ;; Show final buffers
  (let ((final-buffers (mapcar #'buffer-name (buffer-list))))
    (message "\nFinal buffers: %s" final-buffers)
    (unless noninteractive
      (message "\nChecking for created jupyter buffers:")
      (dolist (buffer-name '("*jupyter-python*" "*jupyter-r*" "*jupyter-stata*"))
        (if (get-buffer buffer-name)
            (message "  ✓ %s: created" buffer-name)
          (message "  ✗ %s: not found" buffer-name)))))
  
  (message "=== END DOOM TEST ==="))

(provide 'test-jupyter-doom)