;;; ob-sas.el --- org-babel functions for sas code evaluation

;; Copyright (C) 2015 Edwin Hu
;; Author: eddyhu@gmail.com
;;      Ista Zahn
;;      G. Jay Kerns
;;      Eric Schulte
;;      Dan Davison

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The file provides Org-Babel support for evaluating sas code.
;; It is basically result of find-and-replace "sas" for "julia"
;; in ob-julia.el by G. Jay Kerns. Only ":results output" works: the
;; header args must include ":results output" (this is the default).
;; Note that I'm not sure ':results value' makes sense or is useful 
;; but I have left all the value-processing stuff inherited from 
;; ob-julia and ob-R. ':results graphics' would be nice, but I have
;; not tried to implement it. 
;; --Ista, 07/30/2014

;;; Requirements:
;; Sas: http://sas.com
;; ESS: http://ess.r-project.org

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(declare-function orgtbl-to-csv "org-table" (table params))
(declare-function sas "ext:ess-sas" (&optional start-args))
(declare-function inferior-ess-send-input "ext:ess-inf" ())
(declare-function ess-make-buffer-current "ext:ess-inf" ())
(declare-function ess-eval-buffer "ext:ess-inf" (vis))
(declare-function org-number-sequence "org-compat" (from &optional to inc))
(declare-function org-remove-if-not "org" (predicate seq))

(defconst org-babel-header-args:sas
  '((width		 . :any)
    (horizontal		 . :any)
    (results             . ((file list vector table scalar verbatim)
			    (raw org html latex code pp wrap)
			    (replace silent append prepend)
                            ;; NOTE: not sure 'value' makes sense in sas
                            ;; we may want to remove it from the list
			    (output value graphics))))
  "sas-specific header arguments.")

(add-to-list 'org-babel-tangle-lang-exts '("sas" . "sas"))

;; only ':results output' currently works, so make that the default
(defvar org-babel-default-header-args:sas '((:results . "output")))

(defcustom org-babel-sas-command "sas -rsasuser -noovp -nosyntaxcheck -nonews -nonotes -stdio -nodate -nonumber -nocenter -linesize MAX"
  "Name of command to use for executing sas code."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.3")
  :type 'string)

(defvar ess-local-process-name) ; dynamically scoped
(defun org-babel-edit-prep:sas (info)
  (let ((session (cdr (assoc :session (nth 2 info)))))
    (when (and session (string-match "^\\*\\(.+?\\)\\*$" session))
      (save-match-data (org-babel-sas-initiate-session session nil)))))

(defun org-babel-expand-body:sas (body params &optional graphics-file)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((graphics-file
	 (or graphics-file (org-babel-sas-graphical-output-file params))))
    (mapconcat
     #'identity
     ((lambda (inside)
	(if graphics-file
            inside
	  inside))
      (append (org-babel-variable-assignments:sas params)
	      (list body))) "\n")))

(defun org-babel-execute:sas (body params)
  "Execute a block of sas code.
This function is called by `org-babel-execute-src-block'."
  (save-excursion
    (let* ((result-params (cdr (assoc :result-params params)))
	   (result-type (cdr (assoc :result-type params)))
           (session (org-babel-sas-initiate-session
		     (cdr (assoc :session params)) params))
	   (colnames-p (cdr (assoc :colnames params)))
	   (rownames-p (cdr (assoc :rownames params)))
	   (graphics-file (org-babel-sas-graphical-output-file params))
	   (full-body (org-babel-expand-body:sas body params graphics-file))
	   (result
	    (org-babel-sas-evaluate
	     session full-body result-type result-params
	     (or (equal "yes" colnames-p)
		 (org-babel-pick-name
		  (cdr (assoc :colname-names params)) colnames-p))
	     (or (equal "yes" rownames-p)
		 (org-babel-pick-name
		  (cdr (assoc :rowname-names params)) rownames-p)))))
      (if graphics-file nil result))))

(defun org-babel-prep-session:sas (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-sas-initiate-session session params))
	 (var-lines (org-babel-variable-assignments:sas params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-load-session:sas (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:sas session params)))
      (with-current-buffer buffer
        (let ((proc (get-buffer-process (current-buffer))))
          (if proc
              (goto-char (process-mark proc))
            (goto-char (point-max))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-variable-assignments:sas (params)
  "Return list of sas statements assigning the block's variables."
  (let ((vars (mapcar #'cdr (org-babel--get-vars params))))
    (mapcar
     (lambda (pair)
       (org-babel-sas-assign-elisp
	(car pair) (cdr pair)
	(equal "yes" (cdr (assoc :colnames params)))
	(equal "yes" (cdr (assoc :rownames params)))))
     (mapcar
      (lambda (i)
	(cons (car (nth i vars))
	      (org-babel-reassemble-table
	       (cdr (nth i vars))
	       (cdr (nth i (cdr (assoc :colname-names params))))
	       (cdr (nth i (cdr (assoc :rowname-names params)))))))
      (org-number-sequence 0 (1- (length vars)))))))

(defun org-babel-sas-quote-csv-field (s)
  "Quote field S for export to sas."
  (if (stringp s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    (format "%S" s)))

(defun org-babel-sas-assign-elisp (name value colnames-p rownames-p)
  "Construct sas code assigning the elisp VALUE to a variable named NAME."
  (if (listp value)
      (let ((max (apply #'max (mapcar #'length (org-remove-if-not
						#'sequencep value))))
	    (min (apply #'min (mapcar #'length (org-remove-if-not
						#'sequencep value))))
	    (transition-file (org-babel-temp-file "sas-import-")))
        ;; ensure VALUE has an orgtbl structure (depth of at least 2)
        (unless (listp (car value)) (setq value (list value)))
        (with-temp-file transition-file
          (insert
	   (orgtbl-to-csv value '(:fmt org-babel-sas-quote-csv-field))
	   "\n"))
	(let ((file (org-babel-process-file-name transition-file 'noquote))
	      (header (if (or (eq (nth 1 value) 'hline) colnames-p)
			  "TRUE" "FALSE"))
	      (row-names (if rownames-p "1" "NULL")))
	  (if (= max min)
	      (format "%s = insheet using \"%s\"" name file)
	    (format "%s = insheet using \"%s\""
		    name file))))
    (format "%s = %s" name (org-babel-sas-quote-csv-field value))))

(defvar ess-ask-for-ess-directory) ; dynamically scoped

(defun org-babel-sas-initiate-session (session params)
  "If there is not a current sas process then create one."
  (unless (string= session "none")
    (let ((session (or session "*iESS[SAS]*"))
	  (ess-ask-for-ess-directory
	   (and (and (boundp 'ess-ask-for-ess-directory) ess-ask-for-ess-directory)
		(not (cdr (assoc :dir params))))))
      (if (org-babel-comint-buffer-livep session)
	  session
	(save-window-excursion
	  (require 'ess) (ess-sas-interactive)
	  (rename-buffer
	   (if (bufferp session)
	       (buffer-name session)
	     (if (stringp session)
		 session
	       (buffer-name))))
	  (current-buffer))))))

(defun org-babel-sas-associate-session (session)
  "Associate sas code buffer with a sas session.
Make SESSION be the inferior ESS process associated with the
current code buffer."
  (let ((proc (get-buffer-process session)))
    (when proc
      (setq ess-local-process-name (process-name proc))))
  (ess-make-buffer-current))

(defun org-babel-sas-graphical-output-file (params)
  "Name of file to which sas should send graphical output."
  (and (member "graphics" (cdr (assq :result-params params)))
       (cdr (assq :file params))))

(defvar org-babel-sas-eoe-indicator "%PUT \"org_babel_sas_eoe\";")
(defvar org-babel-sas-eoe-output "org_babel_sas_eoe")

(defvar org-babel-sas-write-object-command "outsheet using \"%s\"")

(defun org-babel-sas-evaluate
  (session body result-type result-params column-names-p row-names-p)
  "Evaluate sas code in BODY."
  (if session
      (org-babel-sas-evaluate-session
       session body result-type result-params column-names-p row-names-p)
    (org-babel-sas-evaluate-external-process
     body result-type result-params column-names-p row-names-p)))

(defun org-babel-sas-evaluate-external-process
  (body result-type result-params column-names-p row-names-p)
  "Evaluate BODY in external sas process.
If RESULT-TYPE equals 'output then return standard output as a
string.  If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (case result-type
    (value
     (let ((tmp-file (org-babel-temp-file "sas-")))
       (org-babel-eval org-babel-sas-command
		       (format org-babel-sas-write-object-command
			       (org-babel-process-file-name tmp-file 'noquote)
			       (format "begin\n%s\nend" body)))
       (org-babel-sas-process-value-result
	(org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (buffer-string))
	  (org-babel-import-elisp-from-file tmp-file '(4)))
	column-names-p)))
    (output (org-babel-eval org-babel-sas-command body))))

(defun org-babel-sas-evaluate-session
  (session body result-type result-params column-names-p row-names-p)
  "Evaluate BODY in SESSION.
If RESULT-TYPE equals 'output then return standard output as a
string.  If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (case result-type
    (value
     (with-temp-buffer
       (insert (org-babel-chomp body))
       (let ((proc (get-buffer-process session))
             (ess-eval-visibly-p nil))
         (when proc
           (setq ess-local-process-name (process-name proc)))
	 (ess-eval-buffer nil)))
     (let ((tmp-file (org-babel-temp-file "sas-")))
       (org-babel-comint-eval-invisibly-and-wait-for-file
	session tmp-file
	(format org-babel-sas-write-object-command
		(org-babel-process-file-name tmp-file 'noquote) "ans"))
       (org-babel-sas-process-value-result
	(org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (buffer-string))
	  (org-babel-import-elisp-from-file tmp-file '(4)))
	column-names-p)))
    (output
     (mapconcat
      #'org-babel-chomp
      (butlast
       (delq nil
	     (mapcar
	      (lambda (line) (when (> (length line) 0) line))
	      (mapcar
	       (lambda (line) ;; cleanup extra prompts left in output
		 (if (string-match
		      "^\\([ ]*[>+\\.][ ]?\\)+\\([[0-9]+\\|[ ]\\)" line)
		     (substring line (match-end 1))
		   line))
	       (org-babel-comint-with-output (session org-babel-sas-eoe-output)
		 (insert (mapconcat #'org-babel-chomp
				    (list body org-babel-sas-eoe-indicator)
				    "\n"))
		 (inferior-ess-send-input)))))) "\n"))))

(defun org-babel-sas-process-value-result (result column-names-p)
  "sas-specific processing of return value.
Insert hline if column names in output have been requested."
  (if column-names-p
      (cons (car result) (cons 'hline (cdr result)))
    result))

;;; SAS-mode definition for org-src blocks

(define-derived-mode SAS-mode prog-mode "SAS"
  "Major mode for editing SAS source code.
This is a minimal implementation to support org-babel editing."
  
  ;; Set comment syntax
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")
  (setq-local comment-start-skip "/\\*+\\s *")
  
  ;; Also support // comments
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  
  ;; Basic syntax highlighting
  (setq-local font-lock-defaults
              '(sas-font-lock-keywords nil nil nil nil))
  
  ;; Indentation settings
  (setq-local indent-line-function 'sas-indent-line)
  (setq-local tab-width 4)
  
  ;; Add cleanup hook for org-src-mode
  (when (bound-and-true-p org-src-mode)
    (add-hook 'kill-buffer-hook #'sas-mode-cleanup nil t)))

(defun sas-mode-cleanup ()
  "Cleanup function for SAS-mode when used in org-src buffers."
  (when (bound-and-true-p org-src-mode)
    ;; Clear any persistent fontification
    (font-lock-mode -1)
    (font-lock-mode 1)))

;; Font lock keywords for SAS syntax highlighting
(defconst sas-font-lock-keywords
  (list
   ;; Keywords
   '("\\<\\(data\\|proc\\|run\\|quit\\|if\\|then\\|else\\|do\\|end\\|while\\|until\\|for\\|to\\|by\\|output\\|delete\\|stop\\|return\\|goto\\|link\\|select\\|when\\|otherwise\\|where\\|set\\|merge\\|update\\|modify\\|retain\\|drop\\|keep\\|rename\\|label\\|format\\|informat\\|length\\|array\\|call\\|put\\|input\\|file\\|infile\\|cards\\|datalines\\|options\\|title\\|footnote\\|libname\\|filename\\|macro\\|mend\\|global\\|local\\|let\\|sysfunc\\|symput\\|symget\\)\\>" 
     . font-lock-keyword-face)
   ;; Procedures
   '("\\<proc\\s-+\\(\\w+\\)\\>" 1 font-lock-builtin-face)
   ;; Functions
   '("\\<\\(sum\\|mean\\|min\\|max\\|std\\|var\\|n\\|nmiss\\|count\\|freq\\|substr\\|trim\\|upcase\\|lowcase\\|compress\\|scan\\|index\\|length\\|cats\\|catx\\|put\\|input\\|round\\|ceil\\|floor\\|abs\\|sqrt\\|log\\|exp\\|sin\\|cos\\|tan\\|rand\\|uniform\\|normal\\|today\\|date\\|datepart\\|timepart\\|year\\|month\\|day\\|weekday\\|hour\\|minute\\|second\\)\\>("
     . font-lock-function-name-face)
   ;; Variables and dataset names
   '("\\<[a-zA-Z_][a-zA-Z0-9_]*\\>" . font-lock-variable-name-face)
   ;; Numbers
   '("\\<[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
   ;; Strings
   '("\"[^\"]*\"\\|'[^']*'" . font-lock-string-face)
   ;; Comments (/* */ style)
   '("/\\*.*?\\*/" . font-lock-comment-face)
   ;; Comments (// style)  
   '("//.*$" . font-lock-comment-face)
   ;; Macro variables
   '("&[a-zA-Z_][a-zA-Z0-9_]*" . font-lock-variable-name-face))
  "Syntax highlighting for SAS mode.")

;; Simple indentation function
(defun sas-indent-line ()
  "Indent current line as SAS code."
  (interactive)
  (let ((indent-level 0))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (setq indent-level 0)
        ;; Simple indentation: increase after data/proc, decrease at run/quit
        (save-excursion
          (forward-line -1)
          (beginning-of-line)
          (cond
           ((looking-at "\\s-*\\(data\\|proc\\)\\>")
            (setq indent-level tab-width))
           ((looking-at "\\s-*\\(run\\|quit\\)\\s-*;")
            (setq indent-level 0))
           (t (setq indent-level 0))))))
    
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to indent-level)))

;; Register SAS-mode for .sas files
(add-to-list 'auto-mode-alist '("\\.sas\\'" . SAS-mode))

(provide 'ob-sas)

;;; ob-sas.el ends here
