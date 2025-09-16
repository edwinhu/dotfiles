;;; ob-stata.el --- Org Babel Functions for Stata -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Custom Implementation
;; Keywords: languages, org, babel, stata

;;; Commentary:

;; This file provides Stata language support for Org Babel.
;; It defines stata-mode and implements org-babel-execute:stata
;; to enable proper syntax highlighting and code execution
;; for Stata source blocks in Org mode.

;;; Code:

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

;; Logging functionality
;; (defvar ob-stata-debug-log-file (expand-file-name "ob-stata-debug.log" "~/")
;;   "File for logging ob-stata debug information.")
;; 
;; (defvar ob-stata-debug-enabled t
;;   "Whether to enable debug logging for ob-stata.")
;; 
;; (defun ob-stata-log (level format-string &rest args)
;;   "Log a message to the ob-stata debug log.
;; LEVEL is the log level (info, warn, error, debug).
;; FORMAT-STRING and ARGS are passed to `format'."
;;   (when ob-stata-debug-enabled
;;     (let ((message (apply #'format format-string args))
;;           (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
;;       (with-temp-buffer
;;         (insert (format "[%s] [%s] %s\n" timestamp (upcase (symbol-name level)) message))
;;         (append-to-file (point-min) (point-max) ob-stata-debug-log-file)))))
;; 
;; (defun ob-stata-clear-log ()
;;   "Clear the ob-stata debug log file."
;;   (interactive)
;;   (when (file-exists-p ob-stata-debug-log-file)
;;     (delete-file ob-stata-debug-log-file))
;;   (ob-stata-log 'info "=== ob-stata debug log started ==="))
;; 
;; ;; Initialize log
;; (ob-stata-clear-log)

;; Define stata-mode as a derived mode from prog-mode
(define-derived-mode stata-mode prog-mode "Stata"
  "Major mode for editing Stata source code.
This is a minimal implementation to support org-babel editing."
  ;; (ob-stata-log 'info "Initializing stata-mode")
  
  ;; Set comment syntax
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  
  ;; Basic syntax highlighting
  (setq-local font-lock-defaults
              '(stata-font-lock-keywords nil nil nil nil))
  
  ;; Indentation settings
  (setq-local indent-line-function 'stata-indent-line)
  (setq-local tab-width 4)
  
  ;; (ob-stata-log 'info "stata-mode initialized successfully")
  )

;; Font lock keywords for syntax highlighting
(defconst stata-font-lock-keywords
  (list
   ;; Keywords
   '("\\<\\(foreach\\|forvalues\\|if\\|else\\|while\\|for\\|in\\|program\\|end\\|capture\\|quietly\\|noisily\\|preserve\\|restore\\|local\\|global\\|scalar\\|matrix\\|return\\|continue\\|break\\|exit\\)\\>" 
     . font-lock-keyword-face)
   ;; Commands
   '("\\<\\(generate\\|gen\\|replace\\|drop\\|keep\\|rename\\|label\\|encode\\|decode\\|sort\\|gsort\\|merge\\|append\\|collapse\\|reshape\\|by\\|bysort\\|egen\\|regress\\|reg\\|logit\\|probit\\|tobit\\|ivregress\\|xtreg\\|xtset\\|tsset\\|summarize\\|sum\\|tabulate\\|tab\\|correlate\\|corr\\|describe\\|des\\|list\\|display\\|di\\|clear\\|use\\|save\\|import\\|export\\|set\\|help\\|search\\|findit\\|which\\|ssc\\|net\\|ado\\|do\\|run\\|include\\|log\\|cmdlog\\)\\>"
     . font-lock-builtin-face)
   ;; Functions
   '("\\<\\(exp\\|log\\|ln\\|sqrt\\|abs\\|int\\|round\\|ceil\\|floor\\|mod\\|min\\|max\\|sum\\|mean\\|median\\|sd\\|var\\|cov\\|corr\\|uniform\\|normal\\|invnormal\\|substr\\|strlen\\|strpos\\|subinstr\\|trim\\|itrim\\|upper\\|lower\\|proper\\)\\>("
     . font-lock-function-name-face)
   ;; Variables (simplified pattern)
   '("\\<[a-zA-Z_][a-zA-Z0-9_]*\\>" . font-lock-variable-name-face)
   ;; Numbers
   '("\\<[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
   ;; Strings
   '("\"[^\"]*\"" . font-lock-string-face)
   ;; Comments
   '("//.*$" . font-lock-comment-face)
   '("/\\*.*?\\*/" . font-lock-comment-face))
  "Syntax highlighting for Stata mode.")

;; Simple indentation function
(defun stata-indent-line ()
  "Indent current line as Stata code."
  (interactive)
  (let ((indent-level 0)
        (line-start (line-beginning-position)))
    (save-excursion
      (beginning-of-line)
      ;; Simple indentation: indent if previous line ends with {
      (when (and (not (bobp))
                 (progn
                   (forward-line -1)
                   (looking-at ".*{\\s-*$")))
        (setq indent-level 4)))
    (indent-line-to indent-level)))

;; Register stata-mode for .do and .ado files
(add-to-list 'auto-mode-alist '("\\.do\\'" . stata-mode))
(add-to-list 'auto-mode-alist '("\\.ado\\'" . stata-mode))

;; Map 'stata' language to stata-mode for org-src blocks
(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("stata" . stata)))

;; Org babel functions
(defcustom org-babel-default-header-args:stata
  '((:results . "output") (:session . nil))
  "Default header arguments for Stata source blocks."
  :group 'org-babel
  :type '(alist :key-type symbol :value-type string))

(defcustom org-babel-stata-command "stata"
  "Command to invoke Stata."
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:stata (body params)
  "Execute a block of Stata code with org-babel.
BODY contains the code to execute.
PARAMS contains the header arguments."
  ;; (ob-stata-log 'info "Executing Stata code block")
  ;; (ob-stata-log 'debug "Body: %s" body)
  ;; (ob-stata-log 'debug "Params: %s" params)
  
  (let* ((session (cdr (assoc :session params)))
         (result-type (cdr (assoc :result-type params)))
         (result-params (cdr (assoc :result-params params)))
         (full-body (org-babel-expand-body:generic body params)))
    
    ;; (ob-stata-log 'info "Session: %s, Result type: %s" session result-type)
    
    (if session
        (progn
          ;; (ob-stata-log 'info "Using session execution")
          (org-babel-stata-evaluate-session session full-body result-type))
      (progn
        ;; (ob-stata-log 'info "Using external process execution")
        (org-babel-stata-evaluate-external-process full-body result-type)))))

(defun org-babel-stata-evaluate-external-process (body result-type)
  "Evaluate BODY in an external Stata process.
RESULT-TYPE specifies the type of results to return."
  ;; (ob-stata-log 'info "Evaluating in external Stata process")
  
  ;; For now, return a placeholder since actual Stata execution
  ;; would require the Stata binary to be installed
  (let ((result (format "# Stata code (external execution):\n%s\n# [Execution would require Stata binary]" body)))
    ;; (ob-stata-log 'info "External evaluation completed")
    result))

(defun org-babel-stata-evaluate-session (session body result-type)
  "Evaluate BODY in a Stata SESSION.
RESULT-TYPE specifies the type of results to return."
  ;; (ob-stata-log 'info "Evaluating in Stata session: %s" session)
  
  ;; For now, return a placeholder since actual session execution
  ;; would require integration with a Stata process
  (let ((result (format "# Stata code (session: %s):\n%s\n# [Session execution would require Stata integration]" 
                        session body)))
    ;; (ob-stata-log 'info "Session evaluation completed")
    result))

(defun org-babel-prep-session:stata (session params)
  "Prepare SESSION according to PARAMS."
  ;; (ob-stata-log 'info "Preparing Stata session: %s" session)
  (error "Stata sessions are not yet supported"))

(defun org-babel-stata-initiate-session (session)
  "Initialize a Stata SESSION."
  ;; (ob-stata-log 'info "Initiating Stata session: %s" session)
  (error "Stata sessions are not yet supported"))

;; Variable assignment function
(defun org-babel-variable-assignments:stata (params)
  "Return list of Stata variable assignments from PARAMS."
  ;; (ob-stata-log 'debug "Getting variable assignments from params: %s" params)
  (mapcar
   (lambda (pair)
     (format "local %s = %s"
             (car pair)
             (org-babel-stata-var-to-stata (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-stata-var-to-stata (var)
  "Convert an elisp VAR to a Stata variable."
  (cond
   ((numberp var) (number-to-string var))
   ((stringp var) (format "\"%s\"" var))
   ((listp var) (mapconcat #'org-babel-stata-var-to-stata var " "))
   (t (format "%s" var))))

;; Testing functions
;; (defun ob-stata-test-mode ()
;;   "Test that stata-mode is properly defined."
;;   (interactive)
;;   (ob-stata-log 'info "=== Running ob-stata tests ===")
;;   
;;   ;; Test 1: Check if stata-mode is defined
;;   (ob-stata-log 'info "Test 1: Checking if stata-mode is defined")
;;   (if (fboundp 'stata-mode)
;;       (ob-stata-log 'info "✓ stata-mode is defined")
;;     (ob-stata-log 'error "✗ stata-mode is not defined"))
;;   
;;   ;; Test 2: Check if org-babel-execute:stata is defined
;;   (ob-stata-log 'info "Test 2: Checking if org-babel-execute:stata is defined")
;;   (if (fboundp 'org-babel-execute:stata)
;;       (ob-stata-log 'info "✓ org-babel-execute:stata is defined")
;;     (ob-stata-log 'error "✗ org-babel-execute:stata is not defined"))
;;   
;;   ;; Test 3: Test mode activation
;;   (ob-stata-log 'info "Test 3: Testing stata-mode activation")
;;   (with-temp-buffer
;;     (stata-mode)
;;     (if (eq major-mode 'stata-mode)
;;         (ob-stata-log 'info "✓ stata-mode activates correctly")
;;       (ob-stata-log 'error "✗ stata-mode failed to activate")))
;;   
;;   ;; Test 4: Test syntax highlighting setup
;;   (ob-stata-log 'info "Test 4: Testing syntax highlighting")
;;   (with-temp-buffer
;;     (stata-mode)
;;     (insert "generate newvar = oldvar * 2")
;;     (font-lock-fontify-buffer)
;;     (ob-stata-log 'info "✓ Syntax highlighting configured"))
;;   
;;   ;; Test 5: Test org-src-lang-modes integration
;;   (ob-stata-log 'info "Test 5: Testing org-src-lang-modes")
;;   (let ((mode-mapping (assoc "stata" org-src-lang-modes)))
;;     (if mode-mapping
;;         (ob-stata-log 'info "✓ stata is in org-src-lang-modes: %s" mode-mapping)
;;       (ob-stata-log 'warn "⚠ stata not found in org-src-lang-modes")))
;;   
;;   ;; Test 6: Test jupyter-stata mapping
;;   (ob-stata-log 'info "Test 6: Testing jupyter-stata mapping")
;;   (let ((jupyter-mapping (assoc "jupyter-stata" org-src-lang-modes)))
;;     (if jupyter-mapping
;;         (ob-stata-log 'info "✓ jupyter-stata is in org-src-lang-modes: %s" jupyter-mapping)
;;       (ob-stata-log 'warn "⚠ jupyter-stata not found in org-src-lang-modes")))
;;   
;;   (ob-stata-log 'info "=== ob-stata tests completed ===")
;;   (message "ob-stata tests completed. Check %s for results." ob-stata-debug-log-file))

;; ;; Interactive test function for org-edit-src-code
;; (defun ob-stata-test-org-edit ()
;;   "Test org-edit-src-code functionality with Stata blocks."
;;   (interactive)
;;   (ob-stata-log 'info "=== Testing org-edit-src-code with Stata ===")
;;   
;;   ;; Create a test org buffer
;;   (let ((test-buffer (generate-new-buffer "*ob-stata-test*")))
;;     (with-current-buffer test-buffer
;;       (org-mode)
;;       (insert "* Test Stata Block\n\n")
;;       (insert "#+begin_src stata\n")
;;       (insert "generate newvar = oldvar * 2\n")
;;       (insert "regress y x1 x2\n")
;;       (insert "#+end_src\n\n")
;;       (insert "* Test Jupyter-Stata Block\n\n")
;;       (insert "#+begin_src jupyter-stata\n")
;;       (insert "summarize price mpg weight\n")
;;       (insert "correlate price mpg\n")
;;       (insert "#+end_src\n")
;;       (goto-char (point-min))
;;       (search-forward "generate")
;;       (ob-stata-log 'info "Created test buffer with Stata blocks"))
;;     
;;     (switch-to-buffer test-buffer)
;;     (ob-stata-log 'info "Test buffer ready. Try C-c ' on the Stata code blocks.")
;;     (message "Test buffer created. Position cursor in a src block and press C-c ' to test.")))

;; Provide the feature
(provide 'ob-stata)

;; (ob-stata-log 'info "ob-stata.el loaded successfully")
;; (ob-stata-log 'info "stata-mode defined: %s" (fboundp 'stata-mode))
;; (ob-stata-log 'info "org-babel-execute:stata defined: %s" (fboundp 'org-babel-execute:stata))

;;; ob-stata.el ends here
