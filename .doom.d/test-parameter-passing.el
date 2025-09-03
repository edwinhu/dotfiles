;;; test-parameter-passing.el --- Test SAS parameter passing

;; Test to verify :dir parameter is passed through org-babel chain

(defun test-sas-parameter-passing ()
  "Test that :dir parameter flows correctly through the SAS execution chain."
  (interactive)
  
  ;; Clear debug log
  (with-temp-buffer
    (insert "")
    (write-file "/Users/vwh7mb/sas-workflow-debug.log"))
  
  ;; Load required modules
  (add-to-list 'load-path "~/.doom.d/")
  (require 'ob-sas)
  (require 'euporie-termint)
  
  ;; Test 1: Call org-babel-execute:sas directly with :dir parameter
  (message "=== TEST 1: Direct org-babel-execute:sas call ===")
  (let ((test-params '((:dir . "/sshx:wrds|qrsh::/home/nyu/eddyhu/projects/wander2"))))
    (org-babel-execute:sas "proc print data=sashelp.cars(obs=3); run;" test-params))
  
  ;; Wait for logs to be written
  (sleep-for 2)
  
  ;; Read and display debug log
  (message "=== DEBUG LOG CONTENTS ===")
  (with-temp-buffer
    (insert-file-contents "/Users/vwh7mb/sas-workflow-debug.log")
    (message "%s" (buffer-string)))
  
  (message "=== TEST COMPLETE ==="))

;; Run the test
(test-sas-parameter-passing)

(provide 'test-parameter-passing)