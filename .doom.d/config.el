;;; $DOOMDIR/config.el -*- lexical-binding: t; -*- Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; CRITICAL: ZMQ completely eliminated - no jupyter package loaded

;; Load jupyter-termint module for org-babel integration (replaces jupyter-console)
(load (expand-file-name "jupyter-termint.el" 
                       (or (bound-and-true-p doom-user-dir)
                          (expand-file-name "~/.doom.d/"))))

;; Define function to find pixi jupyter executable
(defun find-pixi-jupyter ()
  "Find the pixi jupyter executable in current project."
  (let* ((pixi-envs-path (locate-dominating-file default-directory "pixi.toml"))
         (pixi-jupyter (when pixi-envs-path
                        (expand-file-name ".pixi/envs/default/bin/jupyter" pixi-envs-path))))
    (when (and pixi-jupyter (file-executable-p pixi-jupyter))
      pixi-jupyter)))

;; Setup jupyter console org-babel integration with proper overrides
(defun jupyter-console-setup-babel-integration ()
  "Setup jupyter console org-babel integration with proper function overrides."
  (message "jupyter-console: Setting up babel integration")
  
  ;; Force load babel languages if needed
  (when (boundp 'org-babel-load-languages)
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((python . t) (R . t))))
  
  ;; Override the functions
  (defun org-babel-execute:python (body params)
    "Execute Python BODY with PARAMS using jupyter console."
    (message "=== JUPYTER CONSOLE CUSTOM FUNCTION CALLED ===")
    (message "jupyter-console: Executing Python code block")
    (let* ((file (buffer-file-name))
           (buffer (jupyter-console-get-or-create "python3" file))
           (result-params (cdr (assq :result-params params)))
           (has-graphics (jupyter-console-detect-graphics-code body "python3"))
           (graphics-file (when (or (member "file" result-params) has-graphics)
                            (expand-file-name 
                             (format "jupyter-python-%d-%s.png" 
                                     (random 10000)
                                     (format-time-string "%H%M%S"))
                             (or default-directory temporary-file-directory))))
           (result (if graphics-file
                      ;; Execute with graphics support using line-by-line approach
                      (progn
                        (message "jupyter-console: Generating graphics to %s" graphics-file)
                        ;; Send matplotlib setup commands
                        (jupyter-console-send-string buffer "import matplotlib")
                        (jupyter-console-send-string buffer "matplotlib.use('Agg')")
                        (jupyter-console-send-string buffer "import matplotlib.pyplot as plt")
                        (jupyter-console-send-string buffer "plt.ioff()")
                        
                        ;; Send user code line by line
                        (dolist (line (split-string body "\n"))
                          (when (and line (not (string-match-p "^\\s-*$" line)))
                            (jupyter-console-send-string buffer line)))
                        
                        ;; Send save command
                        (let ((save-cmd (format "plt.savefig(r'%s', bbox_inches='tight', dpi=150, facecolor='white', edgecolor='none')" graphics-file)))
                          (jupyter-console-send-string buffer save-cmd))
                        
                        ;; Wait for file creation and return path
                        (let ((max-wait 8)
                              (wait-count 0))
                          (while (and (< wait-count max-wait)
                                     (not (file-exists-p graphics-file)))
                            (sleep-for 0.5)
                            (setq wait-count (1+ wait-count)))
                          (if (file-exists-p graphics-file)
                              (progn
                                (message "jupyter-console: Graphics file created successfully")
                                graphics-file)
                            (progn
                              (message "jupyter-console: Graphics file not created, falling back to text")
                              (jupyter-console-send-string buffer body)))))
                    ;; Regular execution
                    (jupyter-console-send-string buffer body))))
      (message "jupyter-console: Python execution completed, result: %s" 
               (if (stringp result) 
                   (if (file-exists-p result) "image-file" "text-output")
                 result))
      result))
  
  (defun org-babel-execute:R (body params)
    "Execute R BODY with PARAMS using jupyter console."
    (message "=== JUPYTER CONSOLE CUSTOM R FUNCTION CALLED ===")
    (message "jupyter-console: Executing R code block")  
    (let* ((file (buffer-file-name))
           (buffer (jupyter-console-get-or-create "ir" file))
           (result-params (cdr (assq :result-params params)))
           (graphics-file (when (or (member "file" result-params)
                                    (jupyter-console-detect-graphics-code body "ir"))
                            (expand-file-name 
                             (format "jupyter-R-%d-%s.png" 
                                     (random 10000)
                                     (format-time-string "%H%M%S"))
                             temporary-file-directory)))
           (result (if graphics-file
                      ;; Execute with graphics support  
                      (progn
                        (message "jupyter-console: Generating R graphics to %s" graphics-file)
                        (jupyter-console-send-string-with-images buffer body "ir" graphics-file))
                    ;; Regular execution
                    (jupyter-console-send-string buffer body))))
      (message "jupyter-console: R execution completed, result type: %s" 
               (type-of result))
      result))
  
  ;; Define test function for user to run
  (defun jupyter-console-test ()
    "Test if jupyter console integration is working."
    (interactive)
    (message "=== Testing Jupyter Console Integration ===")
    (message "1. Python function defined: %s" (fboundp 'org-babel-execute:python))
    (message "2. Graphics detection: %s" 
             (jupyter-console-detect-graphics-code "plt.plot([1,2,3])" "python3"))
    (message "3. Auto display enabled: %s" jupyter-console-auto-display-images)
    (message "4. Org inline images: %s" org-startup-with-inline-images)
    (let ((func-def (symbol-function 'org-babel-execute:python)))
      (message "5. Function definition: %s" 
               (cond 
                ((and (listp func-def) (eq (car func-def) 'lambda))
                 "Custom jupyter-console function")
                ((functionp func-def)
                 "Standard function")
                (t "Unknown"))))
    (message "6. Function source contains 'JUPYTER CONSOLE CUSTOM': %s"
             (let ((func-def (symbol-function 'org-babel-execute:python)))
               (and (listp func-def)
                    (string-match-p "JUPYTER CONSOLE CUSTOM" (format "%s" func-def)))))
    (message "=== Test files created: complete-test.org ===")
    (message "Open complete-test.org and try C-c C-c on the code blocks!"))
  
  ;; Debug function to test Python execution directly
  (defun jupyter-console-debug-python ()
    "Debug Python execution directly."
    (interactive)
    (let ((test-code "import matplotlib.pyplot as plt; print('Testing matplotlib')")
          (test-params '((:results . "output"))))
      (message "=== DEBUGGING PYTHON EXECUTION ===")
      (message "About to call org-babel-execute:python...")
      (let ((result (org-babel-execute:python test-code test-params)))
        (message "Result: %s" result))))
  
  ;; Add a debug wrapper to see what's actually being called
  (defun jupyter-console-debug-wrapper (original-func body params)
    "Debug wrapper to see what function is being called."
    (message "=== JUPYTER CONSOLE DEBUG: Function called with graphics code ===")
    (funcall original-func body params))
  
  ;; Enable automatic image display after babel execution
  (add-hook 'org-babel-after-execute-hook
            (lambda ()
              (when (and (derived-mode-p 'org-mode)
                         (not (string-match-p "^ \\*temp" (buffer-name))))
                (message "jupyter-console: Auto-displaying inline images")
                (org-display-inline-images))))
  
  ;; Enable inline images by default
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)
  
  ;; Ensure images are properly linked in results
  (setq org-babel-default-header-args:python '((:results . "file")))
  
  (message "jupyter-console: Babel integration setup complete"))

;; Setup integration after org loads completely  
(after! org
  (jupyter-console-setup-babel-integration))

;; Use advice to ensure our function always takes precedence
(defun jupyter-console-advice-org-babel-execute:R (orig-fun body params)
  "Advice function to override org-babel-execute:R with jupyter console integration."
  (message "=== JUPYTER CONSOLE ADVICE FUNCTION CALLED ===")
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "ir" file))
         (result-params (cdr (assq :result-params params)))
         (graphics-file (when (or (member "file" result-params)
                                  (jupyter-console-detect-graphics-code body "ir"))
                          (expand-file-name 
                           (format "jupyter-R-%d-%s.png" 
                                   (random 10000)
                                   (format-time-string "%H%M%S"))
                           temporary-file-directory)))
         (result (if graphics-file
                    (progn
                      (message "jupyter-console: Generating R graphics to %s" graphics-file)
                      (jupyter-console-send-string-with-images buffer body "ir" graphics-file))
                  (jupyter-console-send-string buffer body))))
    (message "jupyter-console: R execution completed, result type: %s" 
             (type-of result))
    result))

;; Apply advice after ob-R loads
(with-eval-after-load 'ob-R
  (advice-add 'org-babel-execute:R :around #'jupyter-console-advice-org-babel-execute:R)
  (message "jupyter-console: Applied advice to org-babel-execute:R"))

;; Use advice to ensure our Stata function always takes precedence
(defun jupyter-console-advice-org-babel-execute:stata (orig-fun body params)
  "Advice function to override org-babel-execute:stata with jupyter console integration."
  (message "=== JUPYTER CONSOLE STATA ADVICE FUNCTION CALLED ===")
  (let* ((file (buffer-file-name))
         (buffer (jupyter-console-get-or-create "stata" file))
         (result-params (cdr (assq :result-params params)))
         (graphics-file (when (or (member "file" result-params)
                                  (jupyter-console-detect-graphics-code body "stata"))
                          (expand-file-name 
                           (format "jupyter-stata-%d-%s.png" 
                                   (random 10000)
                                   (format-time-string "%H%M%S"))
                           temporary-file-directory)))
         (result (if graphics-file
                    (progn
                      (message "jupyter-console: Generating Stata graphics to %s" graphics-file)
                      (jupyter-console-send-string-with-images buffer body "stata" graphics-file))
                  (jupyter-console-send-string buffer body))))
    (message "jupyter-console: Stata execution completed, result type: %s" 
             (type-of result))
    result))

;; Apply advice after ob-stata loads
(with-eval-after-load 'ob-stata
  (advice-add 'org-babel-execute:stata :around #'jupyter-console-advice-org-babel-execute:stata)
  (message "jupyter-console: Applied advice to org-babel-execute:stata"))

;; Force override even after org loads - this ensures our function takes precedence
(with-eval-after-load 'ob-python
  (message "FORCING JUPYTER-CONSOLE OVERRIDE AFTER OB-PYTHON LOADS")
  (jupyter-console-setup-babel-integration))

;; Additional override hook
(add-hook 'org-mode-hook 
          (lambda ()
            (when (and (boundp 'org-babel-load-languages)
                       (assq 'python org-babel-load-languages))
              (jupyter-console-setup-babel-integration))))

;; NOTE: Old jupyter-console setup commented out - now using jupyter-termint
;; Also set up immediately if org is already loaded (for interactive testing)
;; (when (featurep 'org)
;;   (jupyter-console-setup-babel-integration))

;; Use eval-after-load to ensure it runs after all other org setups
;; (eval-after-load 'org
;;   '(jupyter-console-setup-babel-integration))

;; Also hook into org-mode to ensure function is available
;; (add-hook 'org-mode-hook 
;;           (lambda ()
;;             (when (not (get 'org-babel-execute:python 'jupyter-console-setup))
;;               (jupyter-console-setup-babel-integration)
;;               (put 'org-babel-execute:python 'jupyter-console-setup t))))

;; Setup jupyter-termint integration
(with-eval-after-load 'org
  (message "Setting up jupyter-termint integration"))

;; Force override using advice as a backup method
(defun +jupyter-console-force-setup ()
  "Force setup of jupyter console integration."
  (interactive)
  (jupyter-console-setup-babel-integration)
  (message "Jupyter console integration forcibly enabled"))

;; Working advice function for jupyter-console Python execution
(defun +jupyter-console-python-advice (body params)
  "Advice function to use jupyter-console for Python execution."
  (message "=== JUPYTER CONSOLE ADVICE CALLED ===")
  (let* ((result-params (cdr (assq :result-params params)))
         (has-file (member "file" result-params))
         (has-graphics (jupyter-console-detect-graphics-code body "python3")))
    
    (message "DEBUG: result-params = %s" result-params)
    (message "DEBUG: has-file = %s" has-file)
    (message "DEBUG: has-graphics = %s" has-graphics)
    (message "DEBUG: body = %s" body)
    
    (if (or has-file has-graphics)
        (progn
          (message "jupyter-console: Executing graphics path...")
          (let* ((file (buffer-file-name))
                 (buffer (jupyter-console-get-or-create "python3" file))
                 (graphics-file (expand-file-name 
                               (format "jupyter-python-%d-%s.png" 
                                       (random 10000)
                                       (format-time-string "%H%M%S"))
                               (or default-directory temporary-file-directory))))
            
            (message "jupyter-console: Generating graphics to %s" graphics-file)
            
            ;; Execute with line-by-line approach (FIXED!)
            ;; Send matplotlib setup commands
            (jupyter-console-send-string buffer "import matplotlib")
            (jupyter-console-send-string buffer "matplotlib.use('Agg')")
            (jupyter-console-send-string buffer "import matplotlib.pyplot as plt")
            (jupyter-console-send-string buffer "plt.ioff()")
            
            ;; Send user code line by line
            (dolist (line (split-string body "\n"))
              (when (and line (not (string-match-p "^\\s-*$" line)))
                (jupyter-console-send-string buffer line)))
            
            ;; Send save command
            (let ((save-cmd (format "plt.savefig(r'%s', bbox_inches='tight', dpi=150, facecolor='white', edgecolor='none')" graphics-file)))
              (jupyter-console-send-string buffer save-cmd))
            
            ;; Wait for file creation
            (let ((max-wait 10) (wait-count 0))
              (while (and (< wait-count max-wait)
                         (not (file-exists-p graphics-file)))
                (sleep-for 0.5)
                (setq wait-count (1+ wait-count)))
              
              (if (file-exists-p graphics-file)
                  (progn
                    (message "jupyter-console: Graphics file created successfully")
                    graphics-file)
                (progn
                  (message "jupyter-console: Graphics file not created")
                  nil)))))
      (progn
        (message "jupyter-console: Regular execution (no graphics)")
        (let* ((file (buffer-file-name))
               (buffer (jupyter-console-get-or-create "python3" file)))
          (jupyter-console-send-string buffer body))))))

;; Use defadvice instead of advice-add for better compatibility
(defadvice org-babel-execute:python (around jupyter-console-override activate)
  "Override Python execution to use jupyter-console with image support."
  (let ((body (ad-get-arg 0))
        (params (ad-get-arg 1)))
    (message "=== DEFADVICE JUPYTER CONSOLE CALLED ===")
    (setq ad-return-value (+jupyter-console-python-advice body params))))

;; Also try with eval-after-load as backup
(eval-after-load 'org
  '(progn
     (ad-activate 'org-babel-execute:python)
     (message "Jupyter console defadvice activated for org-babel-execute:python")))

;; Also add to doom hook as backup
(add-hook 'doom-init-ui-hook #'+jupyter-console-force-setup t)


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

;; Claude Code IDE - keep original menu but override functions with termint
(use-package claude-code-ide
  :bind (("C-c C-'" . claude-code-ide-menu))
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-window-width 140)
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-vterm-anti-flicker t))

;; Termint configuration for better REPL integration with vterm
(use-package termint
  :demand t
  :config
  ;; Use vterm as backend for proper terminal emulation
  (setq termint-backend 'vterm)
  
  ;; Configure Claude Code with termint for better multi-line handling
  (termint-define "claude-code" "claude" 
    :bracketed-paste-p t
    :send-delayed-final-ret t
    :source-syntax "@{{file}}")
  
  ;; Define custom termint functions that use the auto-generated ones
  (defun termint-claude-code-continue ()
    "Continue most recent Claude Code conversation using termint."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (switch-to-buffer buffer)
      (progn
        (termint-claude-code-start)
        (run-with-timer 0.5 nil (lambda ()
          (termint-claude-code-send-string "claude --continue"))))))
    
  (defun termint-claude-code-resume ()
    "Resume Claude Code session from previous conversation using termint."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (switch-to-buffer buffer)
      (progn
        (termint-claude-code-start)
        (run-with-timer 0.5 nil (lambda ()
          (termint-claude-code-send-string "claude --resume"))))))
    
  (defun termint-claude-code-quit ()
    "Stop current Claude Code session."
    (interactive)
    (when-let ((buffer (get-buffer "*claude-code*")))
      (kill-buffer buffer)))
      
  (defun termint-claude-code-list ()
    "List all Claude Code sessions."
    (interactive)
    (message "Active Claude Code sessions: %s" 
             (mapcar #'buffer-name 
                     (seq-filter (lambda (b) 
                                   (string-match-p "claude-code" (buffer-name b)))
                                 (buffer-list)))))
  
  (defun termint-claude-code-switch-to-buffer ()
    "Switch to Claude Code buffer."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (switch-to-buffer buffer)
      (termint-claude-code-start)))
      
  (defun termint-claude-code-toggle-window ()
    "Toggle Claude Code window visibility."
    (interactive)
    (if-let ((window (get-buffer-window "*claude-code*")))
        (delete-window window)
      (termint-claude-code-switch-to-buffer)))
      
  (defun termint-claude-code-send-prompt ()
    "Send prompt from minibuffer to Claude Code."
    (interactive)
    (let ((prompt (read-string "Claude Code prompt: ")))
      (termint-claude-code-send-string prompt)))
      
  (defun termint-claude-code-insert-selection ()
    "Insert current selection into Claude Code."
    (interactive)
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (termint-claude-code-send-string text))
      (message "No region selected")))
      
  (defun termint-claude-code-insert-newline ()
    "Insert newline in Claude Code."
    (interactive)
    (termint-claude-code-send-string "\n"))
      
  (defun termint-claude-code-send-escape ()
    "Send escape key to Claude Code."
    (interactive)
    (termint-claude-code-send-string "\e"))
    
  (defun termint-claude-code-yolo ()
    "Start Claude Code with yolo (bypass permissions) using termint."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (progn
          (switch-to-buffer buffer)
          (termint-claude-code-send-string "claude --dangerously-skip-permissions"))
      (progn
        (termint-claude-code-start)
        (run-with-timer 0.5 nil (lambda ()
          (termint-claude-code-send-string "claude --dangerously-skip-permissions"))))))
          
  (defun termint-claude-code-yolo-continue ()
    "Continue Claude Code conversation with yolo permissions using termint."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (progn
          (switch-to-buffer buffer)
          (termint-claude-code-send-string "claude --dangerously-skip-permissions --continue"))
      (progn
        (termint-claude-code-start)
        (run-with-timer 0.5 nil (lambda ()
          (termint-claude-code-send-string "claude --dangerously-skip-permissions --continue"))))))
          
  (defun termint-claude-code-yolo-resume ()
    "Resume Claude Code session with yolo permissions using termint."
    (interactive)
    (if-let ((buffer (get-buffer "*claude-code*")))
        (progn
          (switch-to-buffer buffer)
          (termint-claude-code-send-string "claude --dangerously-skip-permissions --resume"))
      (progn
        (termint-claude-code-start)
        (run-with-timer 0.5 nil (lambda ()
          (termint-claude-code-send-string "claude --dangerously-skip-permissions --resume"))))))
        
  (defun termint-claude-code-configuration ()
    "Open Claude Code configuration."
    (interactive)
    (message "Claude Code configuration: Use termint settings"))
    
  (defun termint-claude-code-debugging ()
    "Open Claude Code debugging."
    (interactive)
    (message "Claude Code debugging: Check *claude-code* buffer"))
  
  ;; Override claude-code-ide functions to use termint implementations
  (with-eval-after-load 'claude-code-ide
    ;; Create a wrapper function for starting since termint-claude-code-start is auto-generated
    (defun claude-code-ide-start-session ()
      "Start Claude Code session using termint."
      (interactive)
      (if (fboundp 'termint-claude-code-start)
          (termint-claude-code-start)
        (error "termint-claude-code-start function not available. Make sure termint is loaded properly.")))
    
    ;; Session Management  
    (defalias 'claude-code-ide-continue-conversation 'termint-claude-code-continue)  
    (defalias 'claude-code-ide-resume-session 'termint-claude-code-resume)
    (defalias 'claude-code-ide-stop-session 'termint-claude-code-quit)
    (defalias 'claude-code-ide-list-sessions 'termint-claude-code-list)
    
    ;; Navigation
    (defalias 'claude-code-ide-switch-to-buffer 'termint-claude-code-switch-to-buffer)
    (defalias 'claude-code-ide-toggle-window 'termint-claude-code-toggle-window)
    
    ;; Interaction
    (defalias 'claude-code-ide-insert-selection 'termint-claude-code-insert-selection)
    (defalias 'claude-code-ide-send-prompt 'termint-claude-code-send-prompt)
    (defalias 'claude-code-ide-send-escape 'termint-claude-code-send-escape)
    (defalias 'claude-code-ide-insert-newline 'termint-claude-code-insert-newline)
    
    ;; Special functions
    (defalias 'claude-code-ide-yolo 'termint-claude-code-yolo)
    (defalias 'claude-code-ide-yolo-continue 'termint-claude-code-yolo-continue)
    (defalias 'claude-code-ide-yolo-resume 'termint-claude-code-yolo-resume)
    
    ;; Submenus  
    (defalias 'claude-code-ide-configuration 'termint-claude-code-configuration)
    (defalias 'claude-code-ide-debugging 'termint-claude-code-debugging)
    
    (message "claude-code-ide functions overridden with termint implementations"))
  
  ;; Add yolo submenu to claude-code-ide menu
  (with-eval-after-load 'claude-code-ide
    (require 'transient)
    
    ;; Define yolo submenu
    (transient-define-prefix claude-code-ide-yolo-menu ()
      "Claude Code IDE yolo (bypass permissions) menu."
      ["Claude Code YOLO (Bypass Permissions)"
       ["Session Management with --dangerously-skip-permissions"
        ("s" "Start new session with yolo" termint-claude-code-yolo)
        ("c" "Continue conversation with yolo" termint-claude-code-yolo-continue)
        ("r" "Resume session with yolo" termint-claude-code-yolo-resume)]])
    
    ;; Override the main menu to include yolo submenu
    (transient-define-prefix claude-code-ide-menu ()
      "Claude Code IDE main menu."
      [:description claude-code-ide--session-status]
      ["Claude Code IDE"
       ["Session Management"
        ("s" claude-code-ide--start-if-no-session :description claude-code-ide--start-description)
        ("c" claude-code-ide--continue-if-no-session :description claude-code-ide--continue-description)
        ("r" claude-code-ide--resume-if-no-session :description claude-code-ide--resume-description)
        ("q" "Stop current session" claude-code-ide-stop)
        ("l" "List all sessions" claude-code-ide-list-sessions)]
       ["Navigation"
        ("b" "Switch to Claude buffer" claude-code-ide-switch-to-buffer)
        ("w" "Toggle window visibility" claude-code-ide-toggle-window)]
       ["Interaction"
        ("i" "Insert selection" claude-code-ide-insert-at-mentioned)
        ("p" "Send prompt from minibuffer" claude-code-ide-send-prompt)
        ("e" "Send escape key" claude-code-ide-send-escape)
        ("n" "Insert newline" claude-code-ide-insert-newline)]
       ["Submenus"
        ("Y" "YOLO (Bypass Permissions)" claude-code-ide-yolo-menu)
        ("C" "Configuration" claude-code-ide-config-menu)
        ("d" "Debugging" claude-code-ide-debug-menu)]]))
  
  (message "termint: Claude Code integration configured with vterm backend"))

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

;; NOTE: +jupyter-console-python-advice is defined earlier in the file with graphics support

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

;; DISABLED: This conflicts with our jupyter-console image integration
;; (with-eval-after-load 'org
;;   (with-eval-after-load 'ob
;;     (require 'jupyter-console)
;;     ;; Use advice-add with override to ensure our functions run
;;     (advice-add 'org-babel-execute:python :override #'+jupyter-console-python-advice)
;;     (advice-add 'org-babel-execute:stata :override #'+jupyter-console-stata-advice)
;;     (advice-add 'org-babel-execute:R :override #'+jupyter-console-r-advice)))

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
       )))

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
