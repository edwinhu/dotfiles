#!/bin/bash

# test.sh - Automated test for C-RET :dir parameter extraction
# Tests the hook-based approach for extracting :dir from org-src blocks

set -e

echo "=== Euporie C-RET :dir Parameter Test ==="
echo "$(date): Starting automated test"

# Step 1: Kill existing Emacs processes
echo "Step 1: Killing existing Emacs processes..."
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs -r kill -9 2>/dev/null || true
sleep 2

# Step 2: Clear logs for clean test
echo "Step 2: Clearing debug logs..."
> ~/sas-workflow-debug.log
> ~/euporie-debug.log

# Step 3: Start fresh Emacs WITHOUT environment inheritance issues
echo "Step 3: Starting fresh Emacs.app..."
osascript -e 'tell application "Emacs" to activate' &
sleep 5  # Wait for Emacs to fully load

# Step 3a: Configure euporie environment in Emacs session
echo "Step 3a: Configuring euporie environment in Emacs..."
emacsclient --eval "(progn
  (message \"=== Configuring euporie environment ===\")
  (let ((project-dir \"~/projects/emacs-euporie/\"))
    (cd project-dir)
    (setenv \"PATH\" (concat project-dir \".pixi/envs/default/bin:\" (getenv \"PATH\")))
    (message \"✓ Updated PATH for euporie\")))"

# Step 4: Open test.org file and navigate to SAS block
echo "Step 4: Opening test.org file..."
emacsclient --eval "(progn
  (message \"=== TEST: Opening ~/projects/emacs-euporie/test.org ===\")
  (find-file \"~/projects/emacs-euporie/test.org\")
  (goto-char (point-min))
  (if (search-forward \":dir /sshx:wrds\" nil t)
      (progn
        (beginning-of-line)
        (search-forward \"#+begin_src sas\" nil t)
        (next-line)
        (message \"Found SAS block with :dir parameter\"))
    (error \"Could not find SAS block with :dir parameter\")))"

# Step 5: Load SAS support and enter org-src edit mode (C-')
echo "Step 5: Loading SAS support and entering org-src edit mode (C-')..."
emacsclient --eval "(progn
  (message \"=== TEST: Loading ob-sas and entering org-src edit mode ===\")
  (require 'ob-sas)
  (message \"✓ Loaded ob-sas\")
  (org-edit-special)
  (sleep-for 1)
  (message \"✓ Entered org-src edit buffer: %s\" (buffer-name))
  (message \"Buffer mode: %s\" major-mode)
  (message \"Current euporie-termint-current-dir: %s\" 
           (if (boundp 'euporie-termint-current-dir) 
               euporie-termint-current-dir 
               \"unbound\")))"

# Step 6: Execute C-RET in org-src edit buffer
echo "Step 6: Executing C-RET in org-src edit buffer..."
emacsclient --eval "(progn
  (message \"=== TEST: Executing C-RET ===\")
  (goto-char (point-min))
  (search-forward \"proc print\" nil t)
  (euporie-termint-send-region-or-line)
  (message \"C-RET executed\"))"

# Step 7: Wait for execution and check buffer  
echo "Step 7: Waiting for euporie console to start and execute code..."
sleep 20  # Longer wait for remote connection + euporie startup

# Step 7a: Check for ACTUAL cars table output
echo "Step 7a: Checking for actual cars table output..."
CARS_OUTPUT_SUCCESS=$(emacsclient --eval "(progn
  (message \"=== TEST: Checking SAS buffer for cars output ===\")
  (if (get-buffer \"*euporie-sas*\")
      (with-current-buffer \"*euporie-sas*\"
        (let ((content (buffer-string)))
          (message \"SAS buffer content length: %d chars\" (length content))
          (when (> (length content) 100)
            (message \"SAS buffer content preview: %s\" (substring content 0 (min 500 (length content)))))
          ;; Check for ACTUAL cars table data - must have columns AND data rows
          (let ((has-headers (or (string-match-p \"Make.*Model.*Type\" content)
                                (string-match-p \"Obs.*Make.*Model\" content)
                                (string-match-p \"Make.*Model.*MSRP\" content)))
                (has-acura (string-match-p \"Acura\" content))
                (has-other-makes (or (string-match-p \"BMW\" content)
                                   (string-match-p \"Honda\" content)
                                   (string-match-p \"Toyota\" content)))
                (has-numbers (string-match-p \"[0-9]+\" content)))
            (if (and has-headers has-acura has-numbers)
                (progn 
                  (message \"✓ SUCCESS: Found COMPLETE cars table with headers and data\") 
                  t)
              (progn
                (message \"✗ FAIL: Incomplete cars output\")
                (message \"  - headers: %s\" has-headers)
                (message \"  - has Acura: %s\" has-acura) 
                (message \"  - has other makes: %s\" has-other-makes)
                (message \"  - has numbers: %s\" has-numbers)
                nil)))))
    (progn
      (message \"✗ FAIL: No *euporie-sas* buffer found\")
      nil))")

# Step 7b: Check window split arrangement
echo "Step 7b: Checking window split arrangement..."  
WINDOW_SPLIT_SUCCESS=$(emacsclient --eval "(progn
  (message \"=== TEST: Checking window split arrangement ===\")
  (let ((windows (window-list))
        (current-buf (buffer-name))
        (sas-buf-visible nil)
        (org-src-visible nil))
    (message \"Number of windows: %d\" (length windows))
    (message \"Current buffer: %s\" current-buf)
    (dolist (win windows)
      (let ((buf-name (buffer-name (window-buffer win))))
        (message \"Window contains buffer: %s\" buf-name)
        (when (string-match-p \"*euporie-sas*\" buf-name)
          (setq sas-buf-visible t))
        (when (string-match-p \"*Org Src.*sas\" buf-name)
          (setq org-src-visible t))))
    (if (and (>= (length windows) 2) sas-buf-visible org-src-visible)
        (progn 
          (message \"✓ SUCCESS: Window split with both org-src and *euporie-sas* visible\") 
          t)
      (progn
        (message \"✗ FAIL: Window split incorrect\")
        (message \"  - sas buffer visible: %s\" sas-buf-visible)
        (message \"  - org-src visible: %s\" org-src-visible) 
        (message \"  - total windows: %d\" (length windows))
        nil))))")

# Step 8: Check logs for local vs remote execution
echo "Step 8: Analyzing logs..."

# Check for remote execution indicators
if grep -q "is-remote.*qrsh" ~/sas-workflow-debug.log 2>/dev/null; then
    echo "✓ SUCCESS: Remote execution detected in logs"
    REMOTE_SUCCESS=true
else
    echo "✗ FAIL: No remote execution detected"
    REMOTE_SUCCESS=false
fi

if grep -q "dir: nil" ~/sas-workflow-debug.log 2>/dev/null; then
    echo "✗ FAIL: Found 'dir: nil' in logs - indicates local execution"
    LOCAL_DETECTED=true
else
    echo "✓ SUCCESS: No 'dir: nil' found in logs"  
    LOCAL_DETECTED=false
fi

# Check for :dir parameter extraction
if grep -q "Extracted :dir from" ~/euporie-debug.log 2>/dev/null; then
    echo "✓ SUCCESS: Hook successfully extracted :dir parameter"
    HOOK_SUCCESS=true
else
    echo "✗ FAIL: Hook did not extract :dir parameter"
    HOOK_SUCCESS=false
fi

# Step 9: Take screenshot
echo "Step 9: Taking screenshot..."
osascript -e 'tell application "Emacs" to activate'
sleep 0.5
screencapture -T 0.5 ~/test-results-screenshot.png
echo "Screenshot saved to ~/test-results-screenshot.png"

# Summary
echo ""
echo "=== STRICT TEST SUMMARY ==="
echo "Timestamp: $(date)"

# Convert Emacs boolean results to shell booleans
if [ "$CARS_OUTPUT_SUCCESS" = "t" ]; then
    CARS_SUCCESS=true
else
    CARS_SUCCESS=false  
fi

if [ "$WINDOW_SPLIT_SUCCESS" = "t" ]; then
    SPLIT_SUCCESS=true
else
    SPLIT_SUCCESS=false
fi

echo "Test Results:"
echo "- Cars table output: $CARS_SUCCESS"
echo "- Window split correct: $SPLIT_SUCCESS"  
echo "- Hook extraction: $HOOK_SUCCESS"
echo "- Remote execution: $REMOTE_SUCCESS"
echo "- Local execution avoided: $([ "$LOCAL_DETECTED" = false ] && echo true || echo false)"

if [ "$CARS_SUCCESS" = true ] && [ "$SPLIT_SUCCESS" = true ] && [ "$HOOK_SUCCESS" = true ] && [ "$REMOTE_SUCCESS" = true ] && [ "$LOCAL_DETECTED" = false ]; then
    echo "🎉 OVERALL RESULT: SUCCESS"
    echo "- All criteria met: cars output, window split, remote execution"
    EXIT_CODE=0
else
    echo "❌ OVERALL RESULT: FAILURE"
    echo "- Missing requirements - see details above"
    EXIT_CODE=1
fi

echo ""
echo "Debug files:"
echo "- SAS workflow: ~/sas-workflow-debug.log"
echo "- Euporie debug: ~/euporie-debug.log"
echo "- Screenshot: ~/test-results-screenshot.png"

echo ""
echo "Recent log entries:"
echo "--- sas-workflow-debug.log (last 10 lines) ---"
tail -10 ~/sas-workflow-debug.log 2>/dev/null || echo "No log file found"

echo "--- euporie-debug.log (last 10 lines) ---"  
tail -10 ~/euporie-debug.log 2>/dev/null || echo "No log file found"

exit $EXIT_CODE