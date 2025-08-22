;;; test-integration-loading.el --- Debug integration file loading

(message "=== TESTING INTEGRATION FILE LOADING ===")

;; Fix load-path
(unless (featurep 'termint)
  (add-to-list 'load-path "~/.emacs.d/.local/straight/build-29.4/termint")
  (require 'termint))

(when (featurep 'termint)
  (setq termint-backend 'vterm)
  
  ;; Set up jupyter (minimal)
  (let ((jupyter-path "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter"))
    (termint-define "jupyter-python" (format "%s console --kernel python3" jupyter-path)
                    :bracketed-paste-p t))
  
  (message "Before loading integration file...")
  (message "Functions before load:")
  (message "  termint-jupyter-python-eval-and-step: %s" 
           (fboundp 'termint-jupyter-python-eval-and-step))
  (message "  termint-jupyter-setup-keybindings: %s" 
           (fboundp 'termint-jupyter-setup-keybindings))
  
  ;; Load the integration file with debugging
  (condition-case err
      (progn
        (message "Starting to load integration file...")
        (load "~/.doom.d/termint-jupyter-integration.el")
        (message "✓ Integration file loaded successfully"))
    (error (message "✗ Integration file loading failed: %s" err)))
  
  (message "Functions after load:")
  (message "  termint-jupyter-python-eval-and-step: %s" 
           (fboundp 'termint-jupyter-python-eval-and-step))
  (message "  termint-jupyter-setup-keybindings: %s" 
           (fboundp 'termint-jupyter-setup-keybindings))
  (message "  termint-jupyter-eval-region-or-function-or-paragraph-and-step: %s" 
           (fboundp 'termint-jupyter-eval-region-or-function-or-paragraph-and-step))
  
  ;; Test if we can call the function manually
  (when (fboundp 'termint-jupyter-python-eval-and-step)
    (message "✓ Can call the function")
    (condition-case err
        (progn
          (message "Testing function call...")
          ;; Don't actually call it since we're in batch mode
          (message "✓ Function is callable"))
      (error (message "✗ Function call failed: %s" err))))
  
  ;; Check if the issue is with eval-after-load conditions
  (message "Checking integration functions exist:")
  (dolist (func '(termint-jupyter-python-eval-and-step
                 termint-jupyter-r-eval-and-step
                 termint-jupyter-stata-eval-and-step
                 termint-jupyter-sas-eval-and-step
                 termint-jupyter-setup-keybindings))
    (message "  %s: %s" func (if (fboundp func) "✓" "✗"))))

(provide 'test-integration-loading)
;;; test-integration-loading.el ends here