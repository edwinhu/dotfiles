;;; $DOOMDIR/config.el -*- lexical-binding: t; -*- Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; CRITICAL: Prevent ZMQ from ever being enabled for Jupyter
;; This guard prevents accidental ZMQ re-enablement that causes binary conflicts
(defun prevent-zmq-loading ()
  "Guard against ZMQ being loaded for Jupyter."
  (when (and (boundp 'jupyter-use-zmq) jupyter-use-zmq)
    (setq jupyter-use-zmq nil)
    (message "WARNING: ZMQ was enabled for Jupyter - forcing it OFF to prevent binary conflicts")))

;; Run the guard after Jupyter loads
(with-eval-after-load 'jupyter
  (prevent-zmq-loading))

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
;; The eval-in-repl-python functionality is included in the main eval-in-repl package
(add-hook 'python-mode-hook
          #'(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))
;; Shell support
;; The eval-in-repl-shell functionality is included in the main eval-in-repl package
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

;; Simple logging for jupyter debugging
(defvar jupyter-debug-log-file (expand-file-name "jupyter-debug.log" default-directory))

(defun jupyter-debug-log (level format-string &rest args)
  "Simple logging function."
  (let ((message (apply #'format format-string args)))
    (message "%s: %s" (upcase (symbol-name level)) message)))

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
  
  ;; FORCE KERNEL-ONLY MODE (NO NOTEBOOK SERVER)
  ;; Completely disable any notebook server mechanisms
  (setq jupyter-runtime-directory nil)              ; Don't look for existing notebook servers
  (setq jupyter-server-kernel-names nil)           ; Don't try to get kernels from servers
  (setq jupyter-include-other-output nil)          ; Simplified output handling
  
  ;; Disable ALL server-related functionality
  (when (boundp 'jupyter-server-launch-url)
    (setq jupyter-server-launch-url nil))           ; Don't try to connect to notebook server
  (when (boundp 'jupyter-server-buffer-name)
    (setq jupyter-server-buffer-name nil))          ; Don't look for server buffers
  (when (boundp 'jupyter-server-process-buffer)
    (setq jupyter-server-process-buffer nil))       ; Don't manage server processes
  
  ;; Disable notebook-style connection discovery
  (when (boundp 'jupyter-connections-file)
    (setq jupyter-connections-file nil))            ; Don't use connections file
  (when (boundp 'jupyter-connection-dir)  
    (setq jupyter-connection-dir nil))              ; Don't look in connection directory
  
  ;; Force direct kernel process startup
  (setq jupyter-use-kernel-cmd t)                   ; Force using kernel command directly
  (setq jupyter-repl-kernel-cmd "jupyter")          ; Use jupyter kernel command
  
  ;; Prevent any browser/server launching
  (when (boundp 'jupyter-launch-browser)
    (setq jupyter-launch-browser nil))              ; Never launch browser
  (when (boundp 'jupyter-notebook-startup-flags)
    (setq jupyter-notebook-startup-flags nil))      ; No notebook flags
  
  ;; Override kernel startup method to use 'jupyter kernel' directly
  (when (boundp 'jupyter-kernel-startup-method)
    (setq jupyter-kernel-startup-method 'direct))   ; Force direct method
  
  ;; Force console mode behavior
  (setq jupyter-default-timeout 10)                 ; Reasonable timeout for direct kernel communication
  (setq jupyter-long-timeout 30)                    ; Longer timeout for startup
  
  (jupyter-debug-log 'info "✓ Forced kernel-only mode (disabled all notebook server behavior)")
  
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
  
  ;; Add error catching advice with detailed debugging
  (defun jupyter-strategy-a-error-handler (orig-fun &rest args)
    "Catch and log errors in Strategy A configuration."
    (jupyter-debug-log 'debug "Calling %s with args: %s" orig-fun args)
    (condition-case err
        (apply orig-fun args)
      (error
       (let ((error-msg (format "%s" err)))
         (jupyter-debug-log 'error "Strategy A error in %s: %s" orig-fun error-msg)
         (when (string-match-p "zmq\\|ZMQ" error-msg)
           (jupyter-debug-log 'error "ZMQ error detected despite jupyter-use-zmq=nil"))
         (when (string-match-p "Connection refused\\|localhost.*[0-9]+" error-msg)
           (jupyter-debug-log 'error "Network connection error - may be trying to connect to notebook server instead of starting kernel"))
         (signal (car err) (cdr err)))))
  
  ;; Add specific debugging for jupyter-run-repl
  (defun jupyter-debug-repl-startup (kernel-name)
    "Debug function to analyze jupyter-run-repl behavior."
    (jupyter-debug-log 'info "=== Debugging REPL startup for %s ===" kernel-name)
    
    ;; Check available kernelspecs
    (let ((specs (jupyter-available-kernelspecs)))
      (jupyter-debug-log 'info "Available kernelspecs: %s" 
                        (mapcar #'jupyter-kernelspec-name specs))
      
      ;; Find the specific kernel
      (let ((spec (cl-find kernel-name specs :key #'jupyter-kernelspec-name :test #'string=)))
        (if spec
            (progn
              (jupyter-debug-log 'info "Found kernelspec for %s" kernel-name)
              (jupyter-debug-log 'debug "Kernelspec details: %s" spec))
          (jupyter-debug-log 'error "No kernelspec found for %s" kernel-name))))
    
    ;; Check jupyter configuration
    (jupyter-debug-log 'info "Current jupyter configuration:")
    (jupyter-debug-log 'info "  jupyter-command: %s" jupyter-command)
    (jupyter-debug-log 'info "  jupyter-use-zmq: %s" jupyter-use-zmq)
    (when (boundp 'jupyter-server-launch-url)
      (jupyter-debug-log 'info "  jupyter-server-launch-url: %s" jupyter-server-launch-url)))
  
  ;; Override jupyter kernel startup to force direct kernel command
  (defun jupyter-kernel-start-override (orig-fun &rest args)
    "Override to force using 'jupyter kernel' command instead of notebook server."
    (jupyter-debug-log 'info "Overriding kernel startup to use direct 'jupyter kernel' command")
    (let* ((kernel-name (car args))
           (kernel-spec (jupyter-get-kernelspec kernel-name))
           (jupyter-cmd (or (find-pixi-jupyter) 
                           "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter"
                           "jupyter"))
           ;; Force kernel-only command
           (kernel-cmd (list jupyter-cmd "kernel" "--kernel" kernel-name)))
      
      (jupyter-debug-log 'info "Using direct kernel command: %s" (string-join kernel-cmd " "))
      
      ;; Override the kernelspec argv to use direct kernel command
      (when kernel-spec
        (plist-put kernel-spec :argv kernel-cmd))
      
      ;; Call original function with modified spec
      (apply orig-fun args)))
  
  ;; Apply the override to kernel startup functions
  (advice-add 'jupyter-start-kernel :around #'jupyter-kernel-start-override)
  
  ;; Override jupyter-run-repl to prevent notebook server attempts
  (defun jupyter-run-repl-override (orig-fun &rest args)
    "Override jupyter-run-repl to force direct kernel startup."
    (jupyter-debug-log 'info "Overriding jupyter-run-repl to prevent notebook server startup")
    (let* ((kernel-name (car args))
           ;; Force all server-related variables to nil during this call
           (jupyter-runtime-directory nil)
           (jupyter-server-kernel-names nil)
           (jupyter-include-other-output nil)
           (jupyter-use-zmq nil))
      
      (jupyter-debug-log 'info "Forcing direct kernel startup for REPL: %s" kernel-name)
      
      ;; Try the force kernel startup function instead
      (condition-case err
          (jupyter-force-kernel-startup kernel-name)
        (error
         (jupyter-debug-log 'warn "Force startup failed, trying original function: %s" err)
         ;; Fallback to original function with server variables disabled
         (apply orig-fun args)))))
  
  ;; Apply the REPL override
  (advice-add 'jupyter-run-repl :around #'jupyter-run-repl-override)
  
  ;; Enhanced force kernel-only startup function
  (defun jupyter-force-kernel-startup (kernel-name)
    "Force direct kernel startup using 'jupyter kernel' command."
    (interactive (list (jupyter-completing-read-kernelspec)))
    (jupyter-debug-log 'info "=== Force-starting kernel: %s ===" kernel-name)
    
    ;; Use direct jupyter kernel command
    (let* ((jupyter-cmd (or (find-pixi-jupyter) 
                           "/Users/vwh7mb/projects/wander2/.pixi/envs/default/bin/jupyter"
                           "jupyter"))
           (kernel-args (list jupyter-cmd "kernel" "--kernel" kernel-name))
           (connection-file (make-temp-file "jupyter-" nil ".json")))
      
      (jupyter-debug-log 'info "Starting kernel with command: %s" (string-join kernel-args " "))
      (jupyter-debug-log 'info "Connection file: %s" connection-file)
      
      ;; Start the kernel process directly
      (condition-case err
          (let* ((process-name (format "jupyter-kernel-%s" kernel-name))
                 (buffer-name (format "*%s*" process-name))
                 (kernel-process (apply #'start-process 
                                       process-name buffer-name 
                                       (car kernel-args) 
                                       (append (cdr kernel-args) 
                                              (list "--connection-file" connection-file)))))
            
            (if kernel-process
                (progn
                  (jupyter-debug-log 'info "✓ Kernel process started: %s" kernel-process)
                  (message "Kernel %s started successfully using direct kernel command" kernel-name)
                  
                  ;; Wait for connection file to be created
                  (while (not (file-exists-p connection-file))
                    (sleep-for 0.1))
                  
                  ;; Connect to the kernel using the connection file
                  (jupyter-debug-log 'info "Connecting to kernel using connection file: %s" connection-file)
                  (jupyter-connect-repl connection-file))
              (jupyter-debug-log 'error "✗ Failed to start kernel process for %s" kernel-name)))
        (error 
         (jupyter-debug-log 'error "✗ Failed to start kernel %s: %s" kernel-name err)
         (message "Kernel startup failed: %s" err)))))
  
  ;; Apply error handling to org-babel only (jupyter-run-repl already has override)
  (advice-add 'org-babel-execute:jupyter :around #'jupyter-strategy-a-error-handler)
  
  (jupyter-debug-log 'info "✓ Strategy A configuration complete")
  
  ) ; End Strategy A jupyter configuration
  ) ; End use-package! jupyter

;; Set default jupyter kernels
(setq org-babel-default-header-args:jupyter '((:async . "yes")
                                              (:session . "py") 
                                              (:kernel . "python")))

;; keybindings
(load! "bindings")

;; Testing utilities (uncomment to test different Unicode approaches)
;; (load! "test-unicode-fix")
