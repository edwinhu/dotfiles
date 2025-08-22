;;; test-jupyter-termint-only.el --- Test just the jupyter termint setup

(message "=== TESTING JUPYTER TERMINT SETUP ===")

;; Fix load-path
(unless (featurep 'termint)
  (add-to-list 'load-path "~/.emacs.d/.local/straight/build-29.4/termint")
  (require 'termint))

(when (featurep 'termint)
  (message "✓ termint loaded")
  (setq termint-backend 'vterm)
  (message "✓ termint-backend set to vterm")
  
  ;; Test the exact jupyter logic from config.el
  (let ((jupyter-path (or (executable-find "jupyter")
                          "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter")))
    (message "Checking jupyter-path: %s" jupyter-path)
    (message "file-executable-p: %s" (file-executable-p jupyter-path))
    
    (if (and jupyter-path (file-executable-p jupyter-path))
        (progn
          (message "✓ jupyter-path is valid, creating termint definitions...")
          
          ;; Python console
          (termint-define "jupyter-python" (format "%s console --kernel python3" jupyter-path)
                          :bracketed-paste-p t)
          (message "✓ Created jupyter-python definition")
          
          ;; Check if functions were created
          (if (fboundp 'termint-jupyter-python-start)
              (message "✓ termint-jupyter-python-start function created")
            (message "✗ termint-jupyter-python-start function NOT created"))
            
          ;; R console  
          (termint-define "jupyter-r" (format "%s console --kernel ir" jupyter-path)
                          :bracketed-paste-p t)
          (message "✓ Created jupyter-r definition")
          
          ;; Check if functions were created
          (if (fboundp 'termint-jupyter-r-start)
              (message "✓ termint-jupyter-r-start function created")
            (message "✗ termint-jupyter-r-start function NOT created"))
            
          ;; Stata console
          (termint-define "jupyter-stata" (format "%s console --kernel stata_kernel" jupyter-path)
                          :bracketed-paste-p t)
          (message "✓ Created jupyter-stata definition")
          
          ;; Now try to load our integration file
          (message "\nLoading integration file...")
          (condition-case err
              (progn
                (load "~/.doom.d/termint-jupyter-integration.el")
                (message "✓ Integration file loaded")
                
                ;; Check for wrapper functions
                (if (fboundp 'termint-jupyter-python-eval-and-step)
                    (message "✓ termint-jupyter-python-eval-and-step available")
                  (message "✗ termint-jupyter-python-eval-and-step NOT available"))
                  
                (if (fboundp 'termint-jupyter-setup-keybindings)
                    (message "✓ termint-jupyter-setup-keybindings available")
                  (message "✗ termint-jupyter-setup-keybindings NOT available")))
            (error (message "✗ Failed to load integration file: %s" err))))
      
      (message "✗ jupyter-path check failed"))))

(provide 'test-jupyter-termint-only)
;;; test-jupyter-termint-only.el ends here