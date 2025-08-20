;;; $DOOMDIR/config.el -*- lexical-binding: t; -*- Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; CRITICAL: ZMQ completely eliminated - no jupyter package loaded

;; Load jupyter-console module for org-babel integration
(load (expand-file-name "jupyter-console.el" 
                       (or (bound-and-true-p doom-user-dir)
                          (expand-file-name "~/.doom.d/"))))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "JetBrains Mono" :size 13.0 :dpi 96 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "CMU Serif" :size 13.0 :dpi 96))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'catppuccin)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Enable helm keyboard navigation
(customize-set-variable 'helm-ff-lynx-style-map t)

;; Mixed pitch fonts
(use-package mixed-pitch
  :hook
  (org-roam-mode . mixed-pitch-mode))

;; Get rid of titlebar
;; https://github.com/doomemacs/doomemacs/issues/7532
;; (add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))

;; eat terminal
(use-package eat
  :config
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)

  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  ;; Use standard terminal type
  (setq eat-term-name "xterm-256color")
  
  ;; Fix whitespace issues in eat terminal
  (add-hook 'eat-mode-hook
            (lambda ()
              ;; Disable whitespace visualization
              (when (fboundp 'whitespace-mode) (whitespace-mode -1))
              (setq-local show-trailing-whitespace nil)
              (setq-local nobreak-char-display nil)))

  ;; Optional keybindings
  :bind (("C-c t" . eat)
         ("C-c T" . eat-other-window))
)

;; vterm terminal configuration
(after! vterm
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Disable whitespace visualization
              (when (fboundp 'whitespace-mode) (whitespace-mode -1))
              (setq-local show-trailing-whitespace nil)
              (setq-local nobreak-char-display nil)))
  
  ;; Better integration with TRAMP and remote sessions
  (setq vterm-max-scrollback 10000)  ; Increase scrollback for long sessions
  (setq vterm-kill-buffer-on-exit t) ; Clean up buffers when sessions end
  )

;; Test vterm instead of eat for better Unicode handling
(setq +term-backend 'vterm)

;; Prevent global whitespace mode issues in terminals
(after! whitespace
  ;; Ensure whitespace mode doesn't interfere with terminals
  (setq whitespace-global-modes '(not vterm-mode eat-mode term-mode)))

;; Fix Unicode emoji substitution by overriding Doom's font configuration
(defun fix-unicode-emoji-substitution ()
  "Prevent technical Unicode symbols from rendering as color emoji.
This function forces JetBrains Mono for ranges of Unicode characters
that should be monospace but are often hijacked by Apple Color Emoji."
  (when (fboundp 'set-fontset-font)
    (let ((mono-font "JetBrains Mono"))
      ;; Technical Symbols (U+2300-U+23FF)
      ;; Includes: ⏸ ⏹ ⏺ ⏻ ⏼ ⏽ etc.
      (set-fontset-font t '(#x2300 . #x23ff) mono-font)
      
      ;; Miscellaneous Technical (U+2300-U+23FF already covered above)
      
      ;; Geometric Shapes (U+25A0-U+25FF)
      ;; Includes: ■ □ ▲ △ ▶ ▷ ◀ ◁ ● ○ etc.
      (set-fontset-font t '(#x25a0 . #x25ff) mono-font)
      
      ;; Box Drawing (U+2500-U+257F)
      ;; Includes: ─ │ ┌ ┐ └ ┘ ├ ┤ etc.
      (set-fontset-font t '(#x2500 . #x257f) mono-font)
      
      ;; Block Elements (U+2580-U+259F)
      ;; Includes: ▀ ▄ █ ▌ ▐ etc.
      (set-fontset-font t '(#x2580 . #x259f) mono-font)
      
      ;; Arrows (U+2190-U+21FF)
      ;; Includes: ← → ↑ ↓ ↔ ↕ etc.
      (set-fontset-font t '(#x2190 . #x21ff) mono-font)
      
      ;; Mathematical Operators (U+2200-U+22FF)
      ;; Includes: ∀ ∂ ∃ ∅ ∇ ∈ etc.
      (set-fontset-font t '(#x2200 . #x22ff) mono-font)
      
      ;; Miscellaneous Symbols (U+2600-U+26FF)
      ;; Includes: ☀ ☁ ☂ ☃ ★ ☆ ☎ ☏ ☐ ☑ ☒ ⚡ etc.
      ;; This range has many emoji variants, so be selective
      (set-fontset-font t '(#x2600 . #x26ff) mono-font)
      
      ;; Dingbats (U+2700-U+27BF) - often has emoji variants
      ;; Includes: ✓ ✗ ✦ ✧ ✳ etc.
      (set-fontset-font t '(#x2700 . #x27bf) mono-font)
      
      ;; Supplemental Arrows-A (U+27F0-U+27FF)
      (set-fontset-font t '(#x27f0 . #x27ff) mono-font)
      
      ;; Supplemental Arrows-B (U+2900-U+297F)
      (set-fontset-font t '(#x2900 . #x297f) mono-font)
      
      ;; Miscellaneous Symbols and Arrows (U+2B00-U+2BFF)
      ;; Includes: ⬅ ⬆ ⬇ ⭐ ⭕ etc.
      (set-fontset-font t '(#x2b00 . #x2bff) mono-font)
      
      ;; Additional specific problematic characters
      ;; These are common technical symbols that still might slip through
      (dolist (char '(?⏸ ?⏹ ?⏺ ?⏻ ?⏼ ?⏽  ; Media controls
                      ?✓ ?✗ ?✦ ?✧ ?✳      ; Check marks and asterisks
                      ?▶ ?◀ ?▲ ?▼ ?◆ ?◇    ; Triangles and diamonds
                      ?● ?○ ?■ ?□ ?▪ ?▫    ; Circles and squares
                      ?⚡ ?⚠ ?⚙ ?⚛         ; Warning and tech symbols
                      ?⬅ ?➡ ?⬆ ?⬇         ; Bold arrows
                      ?⭐ ?⭕))             ; Star and circle
        (set-fontset-font t char mono-font)))))

;; Run after Doom's font setup (this is the critical hook)
(add-hook! 'after-setting-font-hook :append #'fix-unicode-emoji-substitution)

;; Nerd Icons configuration
(use-package nerd-icons
  :config
  ;; Configure nerd-icons for better terminal support
  (when (not (display-graphic-p))
    (setq nerd-icons-font-family "JetBrainsMono Nerd Font"))
  ;; Set default scale for consistent sizing
  (setq nerd-icons-scale-factor 1.0))

;; Claude Code
(use-package claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-window-width 120)
  (setq claude-code-ide-terminal-backend 'vterm))

;; Include SAS support
(use-package! ob-sas
  :load-path "~/.doom.d/")

;; Include Stata support
(use-package! ob-stata
  :load-path "~/.doom.d/")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sas . t)
   (R . t)
   (stata . t)))

;; Use advice to ensure our functions are always called
(defun +jupyter-console-python-advice (body params)
  "Advice to use jupyter-console for Python."
  (require 'jupyter-console)
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "python3" file))
         (result (jupyter-console-send-string buffer body)))
    result))

(defun +jupyter-console-stata-advice (body params)
  "Advice to use jupyter-console for Stata."
  (require 'jupyter-console)
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "stata" file))
         (result (jupyter-console-send-string buffer body)))
    result))

(defun +jupyter-console-r-advice (body params)
  "Advice to use jupyter-console for R."
  (require 'jupyter-console)
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "ir" file))
         (result (jupyter-console-send-string buffer body)))
    result))

(defun +sas-console-execute (body params)
  "Execute SAS code using sas-console (interactive isas sessions)."
  (require 'sas-console)
  (require 'wrds-debug)
  
  (wrds-debug-log 'info "+sas-console-execute called (SUCCESS: advice working)")
  
  (let* ((session (cdr (assoc :session params)))
         (result-type (cdr (assoc :result-type params)))
         (full-body (org-babel-expand-body:generic body params))
         ;; Use same file detection logic as jupyter-console-send-region
         (file (or (buffer-file-name)
                   (and (boundp 'org-src-source-file-name)
                        org-src-source-file-name))))
    
    (wrds-debug-log 'info "Parameters: session=%s, file=%s" session file)
    (wrds-debug-log-sas "execute" full-body session)
    
    (if (and session (string-match "/sshx:" session))
        ;; Remote execution via sas-console
        (progn
          (wrds-debug-log 'info "Using sas-console for remote SAS execution")
          (let ((console-buffer (sas-console-get-or-create session file)))
            (if console-buffer
                (condition-case err
                    (let ((result (sas-console-send-string console-buffer full-body)))
                      (wrds-debug-log-sas "result" result session)
                      result)
                  (error
                   (wrds-debug-log 'error "SAS console execution failed: %s" err)
                   (format "ERROR: %s" err)))
              (progn
                (wrds-debug-log 'error "Failed to create SAS console")
                "ERROR: Failed to create SAS console"))))
      
      ;; Local execution - try sas-console first, then fall back
      (condition-case err
          (progn
            (wrds-debug-log 'info "Attempting local sas-console execution")
            (let ((console-buffer (sas-console-get-or-create nil file)))
              (if console-buffer
                  (sas-console-send-string console-buffer full-body)
                (error "Local SAS console not available"))))
        (error
         (wrds-debug-log 'warn "Local sas-console failed, falling back to ob-sas: %s" err)
         ;; Provide better error message for remote users
         (if (string-match "Local SAS not available" (error-message-string err))
             (format "ERROR: %s\nHINT: Add ':session /sshx:wrds|qrsh::/' to your SAS source block for remote execution" 
                     (error-message-string err))
           ;; Fall back to standard ob-sas for other errors
           (org-babel-execute:sas body params)))))))

;; Add advice after org loads - and ensure it runs after ESS loads too
(with-eval-after-load 'org
  (with-eval-after-load 'ob
    (require 'jupyter-console)
    ;; Use advice-add with override to ensure our functions run
    (advice-add 'org-babel-execute:python :override #'+jupyter-console-python-advice)
    (advice-add 'org-babel-execute:stata :override #'+jupyter-console-stata-advice)
    (advice-add 'org-babel-execute:R :override #'+jupyter-console-r-advice)))

;; Install SAS advice after ESS loads to ensure we override ESS's version
(with-eval-after-load 'ess
  (with-eval-after-load 'ob-sas
    (advice-add 'org-babel-execute:sas :override #'+sas-console-execute)
    (message "SAS console advice installed (overriding ESS)")
    ;; Log after wrds-debug is available
    (when (fboundp 'wrds-debug-log)
      (wrds-debug-log 'info "SAS console advice installed (overriding ESS)"))))
;; Language mode mappings for syntax highlighting in org source blocks
(add-to-list 'org-src-lang-modes '("sas" . SAS))
;; Jupyter language mappings removed to prevent loading jupyter package

;; Edit in same window
;; Doom now treats these buffers as pop-ups
;; which breaks the default behavior unless you tell it to ignore
(after! org
  (set-popup-rule! "^\\*Org Src" :ignore t)
  (set-popup-rule! "^\\*SAS Console" :ignore t))
(after! vterm
  (set-popup-rule! "^\\*vterm " :ignore t))
(setq org-src-window-setup 'current-window)

;; Auto-open SAS console for SAS org-src blocks
;; Disabled auto-opening SAS console on C-' - user prefers it to start only on C-RET
;; (after! sas-console
;;   (defun +sas-console-org-src-hook ()
;;     "Hook to auto-open SAS console when editing SAS source blocks."
;;     (when (and (bound-and-true-p org-src-mode)
;;                (eq (org-src-get-lang-mode "sas") major-mode))
;;       ;; Delay slightly to let org-src-mode fully initialize
;;       (run-with-timer 0.1 nil #'sas-console-open-for-org-src)))
;;   
;;   (add-hook 'org-src-mode-hook #'+sas-console-org-src-hook))

;; use tectonic for pdf processing as it is faster
(setq org-latex-pdf-process
  '("tectonic %f"))

;; enable org shift select
(setq org-support-shift-select 'always)

;; org hide markers
(setq org-hide-emphasis-markers t)

;; unfill
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; need to have exec-path-from-shell in order to get PATH
(exec-path-from-shell-initialize)

;; ;; File-based logging for debugging
;; (defvar jupyter-debug-log-file (expand-file-name "jupyter-debug.log" "~/"))
;; 
;; (defun log-to-file (message)
;;   "Log MESSAGE to debug file with timestamp"
;;   (with-temp-buffer
;;     (insert (format "[%s] %s\n" 
;;                     (format-time-string "%Y-%m-%d %H:%M:%S") 
;;                     message))
;;     (append-to-file (point-min) (point-max) jupyter-debug-log-file)))
;; 
;; (defun clear-jupyter-log ()
;;   "Clear the jupyter debug log file"
;;   (interactive)
;;   (when (file-exists-p jupyter-debug-log-file)
;;     (delete-file jupyter-debug-log-file))
;;   (log-to-file "=== Jupyter Debug Log Started ==="))
;; 
;; ;; Initialize log
;; (clear-jupyter-log)

;; Direnv integration for environment management
(use-package! envrc
  :config
  (envrc-global-mode)
  ;; Ensure envrc updates exec-path and PATH
  ;; (setq envrc-debug t)  ; Enable debug mode for troubleshooting
  
  ;; Smart direnv allow checking - only prompt for genuinely new .envrc files
  (defcustom envrc-smart-allow-checking t
    "When non-nil, check direnv status before prompting for allow.
This prevents repeated prompts for .envrc files that were previously allowed."
    :type 'boolean
    :group 'envrc)

  (defun envrc--check-direnv-status (env-dir)
    "Check if ENV-DIR's .envrc is already allowed by parsing direnv status.
Returns 'allowed if the .envrc is loaded, 'blocked if it exists but not loaded,
or 'none if no .envrc exists."
    (let* ((default-directory env-dir)
           (status-output (with-temp-buffer
                           (when (zerop (call-process envrc-direnv-executable nil t nil "status"))
                             (buffer-string)))))
      (cond
       ;; No .envrc file at all
       ((and status-output
             (string-match-p "No .envrc.*found" status-output))
        'none)
       
       ;; .envrc is allowed (has allowPath, regardless of current loaded state)
       ((and status-output 
             (string-match-p "Found RC allowPath" status-output))
        'allowed)
        
       ;; .envrc exists but is denied (Found RC allowed 2)
       ((and status-output
             (string-match-p "Found RC allowed 2" status-output))
        'denied)
       
       ;; .envrc exists but not loaded (has "Found RC path" but "No .envrc loaded")
       ((and (envrc--env-dir-p env-dir)
             status-output
             (string-match-p "Found RC path" status-output)
             (string-match-p "No .envrc.*loaded" status-output))
        'blocked)
       
       ;; Default fallback
       (t 'none))))

  (defun envrc--smart-export (env-dir)
    "Smart version of envrc--export that checks allow status first.
Only prompts for permission if the .envrc file is genuinely new/blocked."
    (if (not envrc-smart-allow-checking)
        ;; Fall back to original behavior if disabled
        (envrc--export-original env-dir)
      
      ;; Check status first
      (let ((status (envrc--check-direnv-status env-dir)))
        (cond
         ;; Already allowed - proceed silently
         ((eq status 'allowed)
          (envrc--export-original env-dir))
         
         ;; Blocked or denied - offer to allow
         ((or (eq status 'blocked) (eq status 'denied))
          (let ((prompt-msg (if (eq status 'denied)
                               (format "Directory %s has .envrc but is denied. Allow it? " env-dir)
                               (format "Directory %s has .envrc but is not allowed. Allow it? " env-dir))))
            (if (y-or-n-p prompt-msg)
                (progn
                  (let* ((default-directory env-dir)
                         (exit-code (envrc--call-process-with-global-env 
                                    envrc-direnv-executable nil 
                                    (get-buffer-create "*envrc-allow*") 
                                    nil "allow")))
                    (if (zerop exit-code)
                        (progn
                          (message "Allowed .envrc in %s" env-dir)
                          (envrc--export-original env-dir))
                      (display-buffer "*envrc-allow*")
                      (user-error "Error running direnv allow"))))
              'error)))
         
         ;; No .envrc file
         (t 'none)))))

  ;; Store original function and add advice - use eval-after-load to ensure envrc is ready
  (eval-after-load 'envrc
    '(progn
       (unless (fboundp 'envrc--export-original)
         (fset 'envrc--export-original (symbol-function 'envrc--export)))
       
       (advice-add 'envrc--export :override #'envrc--smart-export)
       ;; (message "✓ Smart envrc advice installed successfully")
       ))))

;; Auto-approve directory-local variables for trusted projects
;; This prevents repeated prompts for .dir-locals.el files in your projects

;; Override the risky local variable function for trusted directories
(defun my-safe-local-variable-advice (original-func sym val)
  "Advice to make variables safe in trusted project directories."
  (or
   ;; Call original function first
   (funcall original-func sym val)
   
   ;; If in trusted directory, approve common variables
   (when (and buffer-file-name
              (string-prefix-p (expand-file-name "~/projects/") 
                              (file-name-directory buffer-file-name)))
     (or
      ;; Approve common data science tool variables
      (memq sym '(jupyter-command python-shell-interpreter R-command 
                  julia-executable conda-env pixi-env))
      
      ;; Approve eval forms that only modify exec-path and PATH
      (and (eq sym 'eval)
           (listp val)
           (eq (car val) 'progn)
           (cl-every (lambda (form)
                      (and (listp form)
                           (or (and (eq (car form) 'add-to-list)
                                   (equal (cadr form) ''exec-path))
                               (and (eq (car form) 'setenv)
                                   (equal (cadr form) "PATH")))))
                    (cdr val)))))))

;; Install advice to override safe-local-variable-p
(advice-add 'safe-local-variable-p :around #'my-safe-local-variable-advice)
;; (message "✓ Directory-local variables auto-approval installed")

;; Function to find jupyter in pixi environment - kept for jupyter-console.el
(defun find-pixi-jupyter ()
  "Find jupyter executable in current pixi environment."
  (let* ((default-directory (locate-dominating-file default-directory ".envrc"))
         (pixi-jupyter (when default-directory
                        (expand-file-name ".pixi/envs/default/bin/jupyter" default-directory))))
    (when (and pixi-jupyter (file-executable-p pixi-jupyter))
      pixi-jupyter)))

;; ;; Simple logging for jupyter debugging
;; (defvar jupyter-debug-log-file (expand-file-name "jupyter-debug.log" default-directory))
;; 
;; (defun jupyter-debug-log (level format-string &rest args)
;;   "Simple logging function."
;;   (let ((message (apply #'format format-string args)))
;;     (message "%s: %s" (upcase (symbol-name level)) message)))

;; WRDS debugging system
(use-package! wrds-debug
  :load-path "~/.doom.d/"
  :config
  ;; Enable debug logging by default for now
  (setq wrds-debug-enabled t))

;; SAS console system  
(use-package! sas-console
  :load-path "~/.doom.d/"
  :after (wrds-debug))

;; TRAMP configuration for WRDS qrsh integration
(use-package! tramp-wrds
  :load-path "~/.doom.d/"
  :after (tramp wrds-debug)
  :config
  ;; Optional: Enable verbose logging for initial testing
  ;; (setq tramp-verbose 6)
  ;; Optional: Set up default WRDS host if you have a specific server
  ;; (add-to-list 'tramp-default-host-alist '("sshx" nil "wrds-cloud.wharton.upenn.edu"))
  )

;; keybindings
(load! "bindings")
