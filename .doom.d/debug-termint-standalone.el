;;; debug-termint-standalone.el --- Standalone termint debugging without Doom macros -*- lexical-binding: t; -*-

(require 'termint)
(require 'jupyter-termint)

(defun debug-termint-detailed ()
  "Detailed debug of termint-jupyter setup and execution paths."
  (message "=== DETAILED JUPYTER-TERMINT DEBUG ===")
  
  ;; Check jupyter executable paths
  (let ((jupyter-paths (list
                        (executable-find "jupyter")
                        ".pixi/envs/default/bin/jupyter"
                        "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter")))
    (message "Jupyter executable checks:")
    (dolist (path jupyter-paths)
      (if path
          (message "  %s: %s (exists: %s)" 
                   path 
                   (if (file-executable-p path) "✓ executable" "✗ not executable")
                   (file-exists-p path))
        (message "  nil path"))))
  
  ;; Test termint backend
  (message "\nTermint configuration:")
  (message "  termint-backend: %s" termint-backend)
  (message "  vterm available: %s" (featurep 'vterm))
  
  ;; Check if jupyter-termint functions exist
  (message "\nJupyter-termint function availability:")
  (let ((funcs '(termint-jupyter-python-start termint-jupyter-r-start termint-jupyter-stata-start)))
    (dolist (func funcs)
      (message "  %s: %s" func (if (fboundp func) "✓" "✗"))))
  
  ;; Try to manually create a termint buffer (only if not in batch mode)
  (if noninteractive
      (message "\nSkipping buffer creation test in batch mode (vterm requires display)")
    (progn
      (message "\nTesting manual termint buffer creation...")
      (condition-case err
          (let* ((jupyter-path (or (executable-find "jupyter") 
                                  ".pixi/envs/default/bin/jupyter"))
                 (test-buffer-name "*test-termint-jupyter*"))
            (message "Using jupyter path: %s" jupyter-path)
            (if (and jupyter-path (file-executable-p jupyter-path))
                (progn
                  (message "Calling termint-define for test...")
                  (termint-define "test-jupyter" jupyter-path
                                  :args '("console" "--kernel" "python3")
                                  :bracketed-paste-p t)
                  (sleep-for 1)
                  (if (fboundp 'termint-test-jupyter-start)
                      (progn
                        (message "✓ termint-test-jupyter-start function created")
                        (message "Attempting to start test jupyter console...")
                        (condition-case start-err
                            (progn
                              (call-interactively 'termint-test-jupyter-start)
                              (sleep-for 2)
                              (if (get-buffer "*test-jupyter*")
                                  (message "✓ Test buffer *test-jupyter* created successfully!")
                                (message "✗ Test buffer *test-jupyter* was not created")))
                          (error (message "✗ Error starting test jupyter: %s" start-err))))
                    (message "✗ termint-test-jupyter-start function not created")))
              (message "✗ No valid jupyter executable found")))
        (error (message "✗ Manual termint test failed: %s" err)))))
  
  ;; Check existing buffer list for any termint-related buffers
  (let ((all-buffers (mapcar #'buffer-name (buffer-list))))
    (message "\nAll current buffers: %s" all-buffers)
    (message "Termint-related buffers: %s" 
             (seq-filter (lambda (name) 
                          (or (string-match-p "jupyter" name)
                              (string-match-p "termint" name)
                              (string-match-p "vterm" name)))
                        all-buffers)))
  
  (message "=== END DETAILED DEBUG ==="))

(defun debug-test-basic-termint ()
  "Test basic termint functionality with a simple shell."
  (if noninteractive
      (message "Skipping basic termint test in batch mode (requires display)")
    (progn
      (message "Testing basic termint with shell...")
      (condition-case err
          (progn
            (termint-define "test-shell" "bash" :bracketed-paste-p nil)
            (sleep-for 1)
            (if (fboundp 'termint-test-shell-start)
                (progn
                  (message "✓ termint-test-shell-start function created")
                  (call-interactively 'termint-test-shell-start)
                  (sleep-for 2)
                  (if (get-buffer "*test-shell*")
                      (message "✓ Basic termint test successful - *test-shell* buffer created")
                    (message "✗ Basic termint test failed - no *test-shell* buffer")))
              (message "✗ termint-test-shell-start function not created")))
        (error (message "✗ Basic termint test error: %s" err))))))

;; Run the debug if this file is executed directly
(when (and (boundp 'load-file-name) load-file-name)
  (jupyter-termint-setup)
  (debug-termint-detailed)
  (debug-test-basic-termint))

(provide 'debug-termint-standalone)
;;; debug-termint-standalone.el ends here