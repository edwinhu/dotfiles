;;; $DOOMDIR/config.el -*- lexical-binding: t; -*- Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

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
(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "CMU Serif" :size 14))

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
              (setq-local nobreak-char-display nil))))

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
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sas . t)
   (R . t)
   (python . t)
   (jupyter . t)
   ))
(add-to-list 'org-src-lang-modes '("sas" . SAS))

;; Edit in same window
;; Doom now treats these buffers as pop-ups
;; which breaks the default behavior unless you tell it to ignore
(after! org
  (set-popup-rule! "^\\*Org Src" :ignore t))
(after! vterm
  (set-popup-rule! "^\\*vterm " :ignore t))
(setq org-src-window-setup 'current-window)
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

;; eval-in-repl
(use-package! eval-in-repl)
(setq eir-repl-placement 'left)
;; python support
(use-package! eval-in-repl-python)
(add-hook 'python-mode-hook
          #'(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))
;; Shell support
(use-package! eval-in-repl-shell)
(add-hook 'sh-mode-hook
          #'(lambda()
             (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))

;; File-based logging for debugging
(defvar jupyter-debug-log-file (expand-file-name "jupyter-debug.log" "~/"))

(defun log-to-file (message)
  "Log MESSAGE to debug file with timestamp"
  (with-temp-buffer
    (insert (format "[%s] %s\n" 
                    (format-time-string "%Y-%m-%d %H:%M:%S") 
                    message))
    (append-to-file (point-min) (point-max) jupyter-debug-log-file)))

(defun clear-jupyter-log ()
  "Clear the jupyter debug log file"
  (interactive)
  (when (file-exists-p jupyter-debug-log-file)
    (delete-file jupyter-debug-log-file))
  (log-to-file "=== Jupyter Debug Log Started ==="))

;; Initialize log
(clear-jupyter-log)

;; Test without loading ZMQ at all to isolate the issue
(log-to-file "Skipping ZMQ loading to test if that's the source of the error")

;; Direnv integration for environment management
(use-package! envrc
  :config
  (envrc-global-mode)
  ;; Ensure envrc updates exec-path and PATH
  (setq envrc-debug t)) ; Enable debug mode for troubleshooting

;; Function to find jupyter in pixi environment
(defun find-pixi-jupyter ()
  "Find jupyter executable in current pixi environment."
  (let* ((default-directory (locate-dominating-file default-directory ".envrc"))
         (pixi-jupyter (when default-directory
                        (expand-file-name ".pixi/envs/default/bin/jupyter" default-directory))))
    (when (and pixi-jupyter (file-executable-p pixi-jupyter))
      pixi-jupyter)))

;; Function to update jupyter paths for current project
(defun update-jupyter-paths ()
  "Update jupyter paths for current project."
  (when-let ((jupyter-path (find-pixi-jupyter)))
    (setq-local jupyter-command jupyter-path
                jupyter-executable jupyter-path
                org-babel-jupyter-command jupyter-path)
    (message "Updated jupyter path to: %s" jupyter-path)))

;; Hook to update jupyter paths when entering a directory
(add-hook 'find-file-hook #'update-jupyter-paths)

;; COMPREHENSIVE JUPYTER DEBUGGING SYSTEM
;; Centralized logging with detailed error capture and environment debugging

(defvar jupyter-debug-log-file (expand-file-name "jupyter-debug.log" default-directory)
  "Centralized log file for Jupyter debugging.")

(defvar jupyter-debug-buffer "*jupyter-debug*"
  "Buffer for interactive Jupyter debugging.")

(defun jupyter-debug-log (level format-string &rest args)
  "Enhanced logging with levels: INFO, WARN, ERROR, DEBUG.
LEVEL: log level (symbol)
FORMAT-STRING and ARGS: format string and arguments for logging."
  (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
         (level-str (upcase (symbol-name level)))
         (message (apply #'format format-string args))
         (log-entry (format "[%s] [%s] %s\n" timestamp level-str message)))
    
    ;; Log to interactive buffer
    (with-current-buffer (get-buffer-create jupyter-debug-buffer)
      (goto-char (point-max))
      (insert log-entry)
      ;; Keep buffer manageable size
      (when (> (point-max) 50000)
        (goto-char (point-min))
        (delete-region (point-min) (+ (point-min) 10000))))
    
    ;; Log to file with error handling
    (condition-case err
        (with-temp-buffer
          (insert log-entry)
          (append-to-file (point-min) (point-max) jupyter-debug-log-file))
      (error (message "Failed to write to jupyter log file: %s" err)))
    
    ;; Also show in messages for immediate feedback
    (when (memq level '(error warn))
      (message "%s: %s" level-str message))))

(defun jupyter-debug-clear-log ()
  "Clear both log file and debug buffer."
  (interactive)
  (when (file-exists-p jupyter-debug-log-file)
    (delete-file jupyter-debug-log-file))
  (when (get-buffer jupyter-debug-buffer)
    (with-current-buffer jupyter-debug-buffer
      (erase-buffer)))
  (jupyter-debug-log 'info "=== Jupyter Debug Session Started ==="))

(defun jupyter-debug-show-log ()
  "Show the jupyter debug buffer."
  (interactive)
  (display-buffer (get-buffer-create jupyter-debug-buffer)))

(defun jupyter-debug-environment ()
  "Log comprehensive environment information for debugging."
  (interactive)
  (jupyter-debug-log 'info "=== Environment Debug Information ===")
  
  ;; System information
  (jupyter-debug-log 'info "Emacs version: %s" emacs-version)
  (jupyter-debug-log 'info "System type: %s" system-type)
  (jupyter-debug-log 'info "Default directory: %s" default-directory)
  
  ;; PATH and executable locations
  (jupyter-debug-log 'info "PATH: %s" (getenv "PATH"))
  (jupyter-debug-log 'info "exec-path: %s" exec-path)
  
  ;; Jupyter executables
  (let ((jupyter-locations (list
                           (executable-find "jupyter")
                           (find-pixi-jupyter)
                           "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter"
                           "jupyter")))
    (dolist (loc jupyter-locations)
      (when loc
        (jupyter-debug-log 'info "Jupyter executable: %s (exists: %s)" 
                          loc (file-executable-p loc)))))
  
  ;; Python executables  
  (let ((python-locations (list
                          (executable-find "python")
                          (executable-find "python3")
                          "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/python")))
    (dolist (loc python-locations)
      (when loc
        (jupyter-debug-log 'info "Python executable: %s (exists: %s)" 
                          loc (file-executable-p loc)))))
  
  ;; Package information
  (jupyter-debug-log 'info "jupyter package loaded: %s" (featurep 'jupyter))
  (jupyter-debug-log 'info "zmq package loaded: %s" (featurep 'zmq))
  (jupyter-debug-log 'info "ob-jupyter package loaded: %s" (featurep 'ob-jupyter))
  
  ;; Jupyter variables
  (when (boundp 'jupyter-command)
    (jupyter-debug-log 'info "jupyter-command: %s" jupyter-command))
  (when (boundp 'jupyter-executable)  
    (jupyter-debug-log 'info "jupyter-executable: %s" jupyter-executable))
  (when (boundp 'jupyter-use-zmq)
    (jupyter-debug-log 'info "jupyter-use-zmq: %s" jupyter-use-zmq))
  
  ;; Direnv status
  (when (boundp 'envrc-mode)
    (jupyter-debug-log 'info "envrc-mode active: %s" envrc-mode))
  
  (jupyter-debug-log 'info "=== Environment Debug Complete ==="))

;; Initialize logging system
(jupyter-debug-clear-log)
(jupyter-debug-log 'info "Jupyter debugging system initialized")

;; INTERACTIVE DEBUG FUNCTIONS
(defun jupyter-debug-quick-test ()
  "Quick diagnostic test - check basic environment and packages."
  (interactive)
  (jupyter-debug-environment)
  (when (file-exists-p (expand-file-name "jupyter-tests.el" doom-user-dir))
    (load (expand-file-name "jupyter-tests.el" doom-user-dir))
    (jupyter-test-quick)))

(defun jupyter-debug-full-test ()
  "Full comprehensive test of all jupyter functionality."
  (interactive)
  (when (file-exists-p (expand-file-name "jupyter-tests.el" doom-user-dir))
    (load (expand-file-name "jupyter-tests.el" doom-user-dir))
    (jupyter-test-comprehensive)))

(defun jupyter-debug-zmq-test ()
  "Specific ZMQ debugging test."
  (interactive)
  (jupyter-debug-log 'info "=== ZMQ Specific Debug Test ===")
  
  ;; Test ZMQ loading approaches
  (jupyter-debug-log 'info "Testing ZMQ loading methods...")
  
  ;; Method 1: Direct require
  (condition-case err
      (progn
        (require 'zmq)
        (jupyter-debug-log 'info "✓ ZMQ direct require successful")
        (when (fboundp 'zmq-version)
          (jupyter-debug-log 'info "ZMQ version: %s" (zmq-version))))
    (error (jupyter-debug-log 'error "✗ ZMQ direct require failed: %s" err)))
  
  ;; Method 2: Check if ZMQ symbols exist
  (let ((zmq-symbols '(zmq-context zmq-socket zmq-REQ zmq-send zmq-recv)))
    (dolist (symbol zmq-symbols)
      (if (fboundp symbol)
          (jupyter-debug-log 'info "✓ ZMQ symbol available: %s" symbol)
        (jupyter-debug-log 'warn "✗ ZMQ symbol missing: %s" symbol))))
  
  ;; Method 3: Test ZMQ context creation
  (when (fboundp 'zmq-context)
    (condition-case err
        (let ((ctx (zmq-context)))
          (jupyter-debug-log 'info "✓ ZMQ context created successfully")
          (when (fboundp 'zmq-context-terminate)
            (zmq-context-terminate ctx)
            (jupyter-debug-log 'info "✓ ZMQ context terminated successfully")))
      (error (jupyter-debug-log 'error "✗ ZMQ context creation failed: %s" err)))))

(defun jupyter-debug-kernel-discovery ()
  "Debug kernel discovery issues."
  (interactive)
  (jupyter-debug-log 'info "=== Kernel Discovery Debug ===")
  
  ;; Test direct jupyter command
  (let ((jupyter-cmd (find-pixi-jupyter)))
    (if jupyter-cmd
        (progn
          (jupyter-debug-log 'info "Found jupyter: %s" jupyter-cmd)
          (condition-case err
              (let ((output (shell-command-to-string (format "%s kernelspec list" jupyter-cmd))))
                (jupyter-debug-log 'info "Kernelspec list output:\n%s" output))
            (error (jupyter-debug-log 'error "Kernelspec list failed: %s" err))))
      (jupyter-debug-log 'error "No jupyter executable found")))
  
  ;; Test jupyter package kernel discovery
  (when (featurep 'jupyter)
    (condition-case err
        (let ((specs (jupyter-available-kernelspecs)))
          (jupyter-debug-log 'info "Jupyter package found %d kernels" (length specs))
          (dolist (spec specs)
            (jupyter-debug-log 'info "Kernel: %s" (jupyter-kernelspec-name spec))))
      (error (jupyter-debug-log 'error "Jupyter kernel discovery failed: %s" err)))))

;; STRATEGY A: PURE PYTHON COMMUNICATION (NO ZMQ)
;; Attempt to use jupyter with websocket/HTTP communication only

(use-package! jupyter
  :defer t
  :commands (jupyter-run-repl jupyter-connect-repl jupyter-available-kernelspecs
             jupyter-repl-associate-buffer org-babel-execute:jupyter)
  :init
  (jupyter-debug-log 'info "Strategy A: Initializing jupyter with NO ZMQ")
  ;; Add autoloads for jupyter functions
  (autoload 'jupyter-run-repl "jupyter" "Run a Jupyter REPL" t)
  (autoload 'jupyter-connect-repl "jupyter" "Connect to a Jupyter kernel" t)
  
  :config
  (jupyter-debug-log 'info "Strategy A: Configuring jupyter package")
  
  ;; CRITICAL: Disable all ZMQ usage
  (setq jupyter-use-zmq nil)
  (jupyter-debug-log 'info "✓ jupyter-use-zmq set to nil")
  
  ;; Ensure we never try to load ZMQ
  (when (featurep 'zmq)
    (jupyter-debug-log 'warn "ZMQ already loaded - this may cause conflicts"))
  
  ;; Set up jupyter executable paths with fallbacks
  (let ((jupyter-path (or (find-pixi-jupyter)
                         "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter"
                         (executable-find "jupyter")
                         "jupyter"))
        (python-path (or "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/python"
                        (executable-find "python3")
                        (executable-find "python"))))
    
    (jupyter-debug-log 'info "Setting jupyter-command to: %s" jupyter-path)
    (jupyter-debug-log 'info "Setting python interpreter to: %s" python-path)
    
    (setq jupyter-command jupyter-path
          jupyter-executable jupyter-path
          org-babel-jupyter-command jupyter-path)
    
    (when python-path
      (setq python-shell-interpreter python-path)))
  
  ;; Configure communication settings
  (setq jupyter-repl-echo-eval-p t
        jupyter-repl-prompt-margin-width 4)
  
  ;; Load ob-jupyter for org-babel integration
  (condition-case err
      (progn
        (require 'ob-jupyter)
        (jupyter-debug-log 'info "✓ ob-jupyter loaded successfully"))
    (error 
     (jupyter-debug-log 'error "✗ Failed to load ob-jupyter: %s" err)))
  
  ;; Configure org-babel languages
  (with-eval-after-load 'org
    (jupyter-debug-log 'info "Configuring org-babel languages")
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (R . t)
       (jupyter . t)))
    (jupyter-debug-log 'info "✓ org-babel languages configured"))
  
  ;; Add error catching advice
  (defun jupyter-strategy-a-error-handler (orig-fun &rest args)
    "Catch and log errors in Strategy A configuration."
    (condition-case err
        (apply orig-fun args)
      (error
       (let ((error-msg (format "%s" err)))
         (jupyter-debug-log 'error "Strategy A error in %s: %s" orig-fun error-msg)
         (when (string-match-p "zmq\\|ZMQ" error-msg)
           (jupyter-debug-log 'error "ZMQ error detected despite jupyter-use-zmq=nil"))
         (signal (car err) (cdr err))))))
  
  ;; Apply error handling to key functions
  (advice-add 'jupyter-run-repl :around #'jupyter-strategy-a-error-handler)
  (advice-add 'org-babel-execute:jupyter :around #'jupyter-strategy-a-error-handler)
  
  (jupyter-debug-log 'info "✓ Strategy A configuration complete")
  
  ) ; End Strategy A jupyter configuration

;; Set default jupyter kernels
(setq org-babel-default-header-args:jupyter '((:async . "yes")
                                              (:session . "py") 
                                              (:kernel . "python")))

;; keybindings
(load! "bindings")

;; Testing utilities (uncomment to test different Unicode approaches)
;; (load! "test-unicode-fix")
