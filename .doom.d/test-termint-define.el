;;; test-termint-define.el --- Test termint-define specifically

(require 'termint nil t)

(message "Test: About to call termint-define")

(condition-case err
    (progn
      (termint-define "test-session" "echo hello"
                      :bracketed-paste-p t
                      :backend 'eat
                      :env '(("TERM" . "xterm")))
      (message "Test: termint-define succeeded"))
  (error 
   (message "Test: termint-define failed with error: %s" err)))

(defun test-after-termint-define ()
  "Function defined after termint-define"
  (message "This function loaded after termint-define"))

(message "Test: Function defined after termint-define")

;;; test-termint-define.el ends here