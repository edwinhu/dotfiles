# Termint Org-Src Integration Issues Documentation

## Current Status
- ✅ **C-RET keybinding works** - Successfully overrides Doom's --INSERT-- behavior
- ✅ **Code detection works** - Correctly extracts multi-line Python code
- ✅ **Console starts** - Jupyter console launches with direnv command
- ❌ **Window management broken** - Left window changes to jupyter console instead of staying on org-src
- ❌ **Direnv prompts persist** - Permission prompts still appear despite using `direnv exec`

## Issue 1: Window Management Problem

### The Problem
The debug log reveals the critical issue at line 29:
```
[INFO] Displaying console in right split, preserving focus
[INFO] Initial buffer: *jupyter-python*  <-- WRONG! Should be *Org Src termint-jupyter-test.org[ python ]*
[INFO] Restoring buffer to: *jupyter-python*  <-- Restoring wrong buffer!
```

**Root Cause**: When `termint-jupyter-python-start` is called, it switches the current buffer to `*jupyter-python*`. By the time `termint-org-src-display-console-right` runs, it captures the wrong "initial buffer".

### Test Plan for Window Issue

```elisp
;; Test 1: Capture buffer state before and after console start
(emacsclient --eval "
(progn
  (find-file \"/Users/vwh7mb/projects/wander2/termint-jupyter-test.org\")
  (goto-char (point-min))
  (search-forward \"#+begin_src python\")
  (forward-line)
  (org-edit-src-code)
  (let ((org-src-buffer (current-buffer))
        (org-src-name (buffer-name)))
    (message \"Before console start: %s\" org-src-name)
    (termint-jupyter-python-start)
    (message \"After console start: %s\" (buffer-name))
    (list :org-src org-src-name :current (buffer-name))))")

;; Test 2: Check window configuration after C-RET
(emacsclient --eval "
(progn
  (when (get-buffer \"*jupyter-python*\")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer \"*jupyter-python*\")))
  (with-current-buffer \"*Org Src termint-jupyter-test.org[ python ]*\"
    (let ((pre-windows (mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))))
      (call-interactively 'termint-org-src-send-simple)
      (sleep-for 2)
      (let ((post-windows (mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))))
        (list :before pre-windows :after post-windows)))))")

;; Test 3: Verify focus preservation
(emacsclient --eval "
(progn
  (let ((focused-window (selected-window))
        (focused-buffer (current-buffer)))
    (message \"Focused before: Window %s, Buffer %s\" focused-window (buffer-name focused-buffer))
    ;; Run the function
    (termint-org-src-send-simple)
    (message \"Focused after: Window %s, Buffer %s\" (selected-window) (buffer-name))
    (list :expected-buffer (buffer-name focused-buffer) 
          :actual-buffer (buffer-name)
          :same-p (eq focused-buffer (current-buffer)))))")
```

### Proposed Fix for Window Issue

The problem is in `termint-org-src-ensure-console-with-features`. We need to:
1. Capture the org-src buffer BEFORE calling any termint functions
2. Pass the correct buffer to the display function
3. Ensure `termint-jupyter-python-start` doesn't permanently switch buffers

```elisp
(defun termint-org-src-ensure-console-with-features (kernel code)
  "Ensure console for KERNEL is running with direnv and window management, then send CODE."
  (let ((buffer-name "*jupyter-python*")
        (start-func 'termint-org-src-smart-python-start)
        (original-buffer (current-buffer))  ;; CAPTURE THIS FIRST!
        (original-window (selected-window))) ;; AND THIS!
    
    ;; ... rest of function
    ;; Then when calling display:
    (termint-org-src-display-console-right new-buffer original-buffer original-window)
    ;; ... ))
```

## Issue 2: Direnv Permission Prompts

### The Problem
Despite using `direnv exec . pixi run jupyter console`, users still see permission prompts.

### Test Plan for Direnv Issue

```elisp
;; Test 1: Check if direnv is truly allowed
(emacsclient --eval "
(let ((default-directory \"/Users/vwh7mb/projects/wander2\"))
  (with-temp-buffer
    (call-process \"direnv\" nil t nil \"status\")
    (buffer-string)))")

;; Test 2: Check console buffer for permission prompts
(emacsclient --eval "
(progn
  (when (get-buffer \"*jupyter-python*\")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer \"*jupyter-python*\")))
  ;; Start console
  (termint-org-src-smart-python-start)
  (sleep-for 3)
  ;; Check for prompt text
  (when (get-buffer \"*jupyter-python*\")
    (with-current-buffer \"*jupyter-python*\"
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (cond
         ((string-match-p \"direnv: error\" content) 
          (list :status 'error :content content))
         ((string-match-p \"direnv: loading\" content)
          (list :status 'loading :content content))
         ((string-match-p \"Y/n\" content)
          (list :status 'prompt :content content))
         (t (list :status 'ok :length (length content))))))))")

;; Test 3: Compare direnv exec vs direct execution
(emacsclient --eval "
(let ((test-results '()))
  ;; Test with direnv exec
  (with-temp-buffer
    (let ((exit-code (call-process \"sh\" nil t nil \"-c\"
                                   \"cd /Users/vwh7mb/projects/wander2 && direnv exec . echo TEST_WITH_EXEC\")))
      (push (list :method 'direnv-exec :exit exit-code :output (buffer-string)) test-results)))
  ;; Test without direnv
  (with-temp-buffer
    (let ((exit-code (call-process \"sh\" nil t nil \"-c\"
                                   \"cd /Users/vwh7mb/projects/wander2 && echo TEST_WITHOUT\")))
      (push (list :method 'direct :exit exit-code :output (buffer-string)) test-results)))
  test-results)")

;; Test 4: Check if .envrc exists and is allowed
(emacsclient --eval "
(let ((envrc-file \"/Users/vwh7mb/projects/wander2/.envrc\"))
  (list :envrc-exists (file-exists-p envrc-file)
        :direnv-dir (expand-file-name \"~/.config/direnv/allow/\")
        :allowed-files (directory-files \"~/.config/direnv/allow/\" nil \"\\\\.envrc$\")))")
```

### Proposed Fix for Direnv Issue

The issue might be that:
1. The `.envrc` file hash has changed since `direnv allow` was run
2. The `sh -c` wrapper is interfering with direnv's tty detection
3. We need to export DIRENV_LOG_FORMAT='' to suppress prompts

```elisp
(defun termint-org-src-smart-python-start ()
  "Start Python jupyter console with smart direnv command."
  (interactive)
  
  ;; Option 1: Ensure direnv is allowed first
  (let ((default-directory "/Users/vwh7mb/projects/wander2"))
    (call-process "direnv" nil nil nil "allow"))
  
  ;; Option 2: Use different command structure
  (let ((smart-cmd (concat "cd /Users/vwh7mb/projects/wander2 && "
                          "export DIRENV_LOG_FORMAT='' && "  ;; Suppress direnv output
                          "direnv exec . pixi run jupyter console --kernel python3")))
    ;; ...
    
  ;; Option 3: Use envrc-mode if available
  (when (fboundp 'envrc-allow)
    (let ((default-directory "/Users/vwh7mb/projects/wander2"))
      (envrc-allow)))
  ;; ...
```

## Issue 3: Focus Preservation After Code Sending

### The Problem
After sending code, focus should remain in the org-src buffer (left window), but it's switching to the jupyter console (right window).

### Test Plan for Focus Issue

```elisp
;; Test: Track focus through entire execution
(emacsclient --eval "
(progn
  (with-current-buffer \"*Org Src termint-jupyter-test.org[ python ]*\"
    (let ((checkpoints '()))
      (push (list :start (selected-window) (buffer-name)) checkpoints)
      
      ;; Set up advice to track buffer switches
      (defadvice switch-to-buffer (before track-switch activate)
        (push (list :switching-to (ad-get-arg 0)) checkpoints))
      
      ;; Run the function
      (termint-org-src-send-simple)
      
      ;; Remove advice
      (ad-remove-advice 'switch-to-buffer 'before 'track-switch)
      (ad-activate 'switch-to-buffer)
      
      (push (list :end (selected-window) (buffer-name)) checkpoints)
      (reverse checkpoints))))")
```

## Comprehensive Integration Test

```elisp
;; Full integration test capturing all issues
(emacsclient --eval "
(let ((test-report '()))
  ;; Setup
  (when (get-buffer \"*jupyter-python*\")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer \"*jupyter-python*\")))
  
  ;; Open org file and enter org-src mode
  (find-file \"/Users/vwh7mb/projects/wander2/termint-jupyter-test.org\")
  (goto-char (point-min))
  (search-forward \"#+begin_src python\")
  (forward-line)
  (org-edit-src-code)
  
  ;; Capture initial state
  (push (list :initial-buffer (buffer-name)
              :initial-window (selected-window)
              :initial-window-count (length (window-list))) test-report)
  
  ;; Execute C-RET
  (let ((start-time (current-time)))
    (command-execute (key-binding (kbd \"C-<return>\")))
    
    ;; Wait for console to start
    (sleep-for 3)
    
    ;; Capture final state
    (push (list :final-buffer (buffer-name)
                :final-window (selected-window)
                :final-window-count (length (window-list))
                :elapsed-time (float-time (time-subtract (current-time) start-time))) test-report))
  
  ;; Check console buffer for issues
  (when (get-buffer \"*jupyter-python*\")
    (with-current-buffer \"*jupyter-python*\"
      (let ((content (buffer-substring-no-properties (point-min) (min (point-max) 500))))
        (push (list :console-content content
                    :has-prompt (string-match-p \"Y/n\" content)
                    :has-error (string-match-p \"error\" content)) test-report))))
  
  ;; Check window configuration
  (push (list :windows (mapcar (lambda (w) 
                                 (list :window w 
                                       :buffer (buffer-name (window-buffer w))))
                               (window-list))) test-report)
  
  (reverse test-report))")
```

## Summary of Key Issues

1. **Window Management**: `termint-org-src-display-console-right` receives wrong initial buffer because `termint-jupyter-python-start` switches buffers
2. **Direnv Prompts**: Despite using `direnv exec`, permission prompts appear - possibly due to TTY detection or changed .envrc hash
3. **Focus Preservation**: After code execution, focus doesn't return to org-src buffer

## Next Steps

1. Fix buffer capture timing in `termint-org-src-ensure-console-with-features`
2. Implement proper direnv pre-authorization or prompt suppression
3. Add explicit focus restoration after all operations complete
4. Test with actual interactive use vs emacsclient to ensure fixes work in both contexts