;;; debug-termint-comprehensive.el --- Comprehensive termint debugging

(message "=== COMPREHENSIVE TERMINT DEBUG ===")
(message "")

;; Fix load-path issue first (same as in config.el)
(unless (featurep 'termint)
  (add-to-list 'load-path "~/.emacs.d/.local/straight/build-29.4/termint"))

;; Test 1: Check if termint package is available
(message "--- TEST 1: Package Availability ---")
(if (locate-library "termint")
    (message "✓ termint library found at: %s" (locate-library "termint"))
  (message "✗ termint library NOT found"))

;; Test 2: Try to load termint
(message "")
(message "--- TEST 2: Loading termint ---")
(condition-case err
    (progn
      (require 'termint)
      (message "✓ termint loaded successfully")
      (message "  termint-backend: %s" (if (boundp 'termint-backend) termint-backend "not set")))
  (error (message "✗ Failed to load termint: %s" err)))

(when (featurep 'termint)
  ;; Test 3: Check termint-define function
  (message "")
  (message "--- TEST 3: termint-define function ---")
  (if (fboundp 'termint-define)
      (message "✓ termint-define function available")
    (message "✗ termint-define function NOT available"))
  
  ;; Test 4: Try creating a test termint definition
  (message "")
  (message "--- TEST 4: Test termint definition ---")
  (condition-case err
      (progn
        (termint-define "test-repl" "echo 'test'")
        (message "✓ termint-define executed successfully")
        
        ;; Check if functions were created
        (if (fboundp 'termint-test-repl-start)
            (message "✓ termint-test-repl-start function created")
          (message "✗ termint-test-repl-start function NOT created"))
          
        (if (fboundp 'termint-test-repl-send-string)
            (message "✓ termint-test-repl-send-string function created")
          (message "✗ termint-test-repl-send-string function NOT created")))
    (error (message "✗ termint-define failed: %s" err)))
  
  ;; Test 5: Check jupyter availability and create jupyter functions
  (message "")
  (message "--- TEST 5: Jupyter availability and termint definitions ---")
  (let ((jupyter-path (executable-find "jupyter")))
    (if jupyter-path
        (progn
          (message "✓ jupyter found at: %s" jupyter-path)
          
          ;; Try to create jupyter-python termint definition
          (condition-case err
              (progn
                (termint-define "jupyter-python-debug" 
                               (format "%s console --kernel python3" jupyter-path)
                               :bracketed-paste-p t)
                (message "✓ jupyter-python-debug termint definition created")
                
                ;; Check if functions were created
                (if (fboundp 'termint-jupyter-python-debug-start)
                    (message "✓ termint-jupyter-python-debug-start function created")
                  (message "✗ termint-jupyter-python-debug-start function NOT created"))
                  
                (if (fboundp 'termint-jupyter-python-debug-send-string)
                    (message "✓ termint-jupyter-python-debug-send-string function created")
                  (message "✗ termint-jupyter-python-debug-send-string function NOT created")))
            (error (message "✗ jupyter-python-debug termint definition failed: %s" err))))
      (message "✗ jupyter executable NOT found")))
  
  ;; Test 6: Load our integration file and check custom functions
  (message "")
  (message "--- TEST 6: Integration file loading ---")
  (let ((integration-file (expand-file-name "termint-jupyter-integration.el" 
                                           (or (bound-and-true-p doom-user-dir)
                                               (expand-file-name "~/.doom.d/")))))
    (if (file-exists-p integration-file)
        (progn
          (message "✓ Integration file exists: %s" integration-file)
          (condition-case err
              (progn
                (load integration-file)
                (message "✓ Integration file loaded successfully")
                
                ;; Check for custom functions
                (dolist (func '(termint-jupyter-python-eval-and-step
                              termint-jupyter-r-eval-and-step
                              termint-jupyter-stata-eval-and-step
                              termint-jupyter-setup-keybindings
                              termint-jupyter-eval-region-or-function-or-paragraph-and-step))
                  (if (fboundp func)
                      (message "✓ %s function available" func)
                    (message "✗ %s function NOT available" func))))
            (error (message "✗ Failed to load integration file: %s" err))))
      (message "✗ Integration file NOT found: %s" integration-file)))
  
  ;; Test 7: Test keybinding setup function
  (message "")
  (message "--- TEST 7: Keybinding setup ---")
  (if (fboundp 'termint-jupyter-setup-keybindings)
      (progn
        (message "✓ termint-jupyter-setup-keybindings function available")
        (condition-case err
            (progn
              (termint-jupyter-setup-keybindings)
              (message "✓ termint-jupyter-setup-keybindings executed successfully"))
          (error (message "✗ termint-jupyter-setup-keybindings failed: %s" err))))
    (message "✗ termint-jupyter-setup-keybindings function NOT available"))
  
  ;; Test 8: Check actual keybindings in python-mode
  (message "")
  (message "--- TEST 8: Actual keybindings ---")
  (with-eval-after-load 'python
    (message "Python mode loaded, checking keybindings...")
    (if (boundp 'python-mode-map)
        (progn
          (message "✓ python-mode-map exists")
          (let ((binding (lookup-key python-mode-map (kbd "C-<return>"))))
            (if binding
                (message "✓ C-<return> is bound to: %s" binding)
              (message "✗ C-<return> is NOT bound in python-mode-map"))))
      (message "✗ python-mode-map NOT available")))
  
  ;; Test 9: Create a test buffer and check keybindings there
  (message "")
  (message "--- TEST 9: Test buffer keybinding check ---")
  (with-temp-buffer
    (python-mode)
    (let ((binding (key-binding (kbd "C-<return>"))))
      (if binding
          (message "✓ In python-mode buffer, C-<return> is bound to: %s" binding)
        (message "✗ In python-mode buffer, C-<return> is NOT bound"))))
  
  (message "")
  (message "=== DEBUG COMPLETE ===")
  (message "Check the results above to identify what's not working."))

(unless (featurep 'termint)
  (message "✗ CRITICAL: termint not loaded - cannot run further tests"))

(provide 'debug-termint-comprehensive)
;;; debug-termint-comprehensive.el ends here