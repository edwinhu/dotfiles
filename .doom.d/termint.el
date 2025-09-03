;;; termint.el --- Simple terminal interface wrapper using comint -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple wrapper providing termint interface using comint backend
;; Provides the basic functions needed by euporie-termint.el

;;; Code:

(require 'comint)
(require 'shell)

(defvar termint-backend 'comint
  "Backend to use for termint - using comint.")

(defvar termint--defined-sessions nil
  "List of defined termint sessions.")

(defun termint-define (name command &rest options)
  "Define a termint session NAME with COMMAND and OPTIONS.
This creates functions for starting and sending strings to the session."
  (let* ((buffer-name (format "*%s*" name))
         (start-func-name (intern (format "termint-%s-start" name)))
         (send-func-name (intern (format "termint-%s-send-string" name)))
         (bracketed-paste (plist-get options :bracketed-paste-p))
         (env-vars (plist-get options :env)))
    
    ;; Store session info
    (push (list name command options) termint--defined-sessions)
    
    ;; Define the start function
    (fset start-func-name
          `(lambda ()
             (interactive)
             (let ((buffer-name ,buffer-name)
                   (default-directory default-directory)
                   (process-environment (copy-sequence process-environment)))
               
               ;; Add environment variables
               ,@(when env-vars
                   `((dolist (env-var ',env-vars)
                       (setenv (car env-var) (cdr env-var)))))
               
               ;; Kill existing buffer
               (when (get-buffer buffer-name)
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer buffer-name)))
               
               ;; Parse command into program and args
               (let* ((cmd-parts (split-string ,command))
                      (program (car cmd-parts))
                      (args (cdr cmd-parts)))
                 ;; Start comint process
                 (apply #'make-comint ,name program nil args)
                 (get-buffer ,buffer-name)))))
    
    ;; Define the send-string function  
    (fset send-func-name
          `(lambda (string)
             (let ((buffer (get-buffer ,buffer-name)))
               (when (and buffer (get-buffer-process buffer))
                 (with-current-buffer buffer
                   (comint-send-string (get-buffer-process buffer) 
                                     (if (string-suffix-p "\n" string)
                                         string
                                       (concat string "\n"))))))))))

(provide 'termint)
;;; termint.el ends here