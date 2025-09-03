;;; test-sas-direct.el --- Test SAS org-babel execution directly

;; Test to verify org-babel-execute:sas works with :dir parameter

(defun test-sas-direct ()
  "Test org-babel-execute:sas with :dir parameter directly."
  (interactive)
  
  ;; Clear debug log
  (with-temp-buffer
    (insert "")
    (write-file "/Users/vwh7mb/euporie-debug.log"))
  
  ;; Load required modules
  (add-to-list 'load-path "~/.doom.d/")
  (require 'tramp-qrsh)
  (require 'euporie-termint)
  
  ;; Test: Call org-babel-execute:sas directly with :dir parameter
  (message "=== TEST: Direct org-babel-execute:sas call ===")
  (let ((test-params '((:dir . "/sshx:wrds|qrsh::/home/nyu/eddyhu/projects/wander2"))))
    (condition-case err
        (progn
          (org-babel-execute:sas "proc print data=sashelp.cars(obs=3); run;" test-params)
          (message "✓ Function executed successfully"))
      (error (message "✗ Function execution failed: %s" err))))
  
  ;; Wait for logs to be written
  (sleep-for 2)
  
  ;; Read and display debug log
  (message "=== DEBUG LOG CONTENTS ===")
  (with-temp-buffer
    (insert-file-contents "/Users/vwh7mb/euporie-debug.log")
    (message "%s" (buffer-string)))
  
  (message "=== TEST COMPLETE ==="))

;; Run the test
(test-sas-direct)

(provide 'test-sas-direct)