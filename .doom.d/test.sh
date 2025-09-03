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

# Step 3: Start fresh Emacs.app
echo "Step 3: Starting fresh Emacs.app..."
osascript -e 'tell application "Emacs" to activate' &
sleep 5  # Wait for Emacs to fully load

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

# Step 5: Enter org-src edit mode (C-')
echo "Step 5: Entering org-src edit mode (C-')..."
emacsclient --eval "(progn
  (message \"=== TEST: Entering org-src edit mode ===\")
  (org-edit-src-code)
  (sleep-for 1)
  (message \"Entered org-src edit buffer: %s\" (buffer-name))
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
echo "Step 7: Waiting for execution to complete..."
sleep 10

# Check if SAS buffer exists and contains output
emacsclient --eval "(progn
  (message \"=== TEST: Checking SAS buffer for cars output ===\")
  (if (get-buffer \"*euporie-sas*\")
      (with-current-buffer \"*euporie-sas*\"
        (let ((content (buffer-string)))
          (message \"SAS buffer content length: %d chars\" (length content))
          (if (or (string-match-p \"Make.*Model\" content)
                  (string-match-p \"Acura\" content)
                  (string-match-p \"cars\" content))
              (message \"✓ SUCCESS: Found cars table output in SAS buffer\")
            (message \"✗ FAIL: No cars table output found in SAS buffer\"))))
    (message \"✗ FAIL: No *euporie-sas* buffer found\")))"

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
if grep -q "Extracted :dir from parent block" ~/euporie-debug.log 2>/dev/null; then
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
echo "=== TEST SUMMARY ==="
echo "Timestamp: $(date)"

if [ "$HOOK_SUCCESS" = true ] && [ "$REMOTE_SUCCESS" = true ] && [ "$LOCAL_DETECTED" = false ]; then
    echo "🎉 OVERALL RESULT: SUCCESS"
    echo "- Hook-based :dir extraction working"
    echo "- Remote execution detected"
    echo "- No local execution fallback"
    EXIT_CODE=0
else
    echo "❌ OVERALL RESULT: FAILURE"
    echo "- Hook extraction: $HOOK_SUCCESS"
    echo "- Remote execution: $REMOTE_SUCCESS" 
    echo "- Local execution avoided: $([ "$LOCAL_DETECTED" = false ] && echo true || echo false)"
    EXIT_CODE=1
fi

echo ""
echo "Debug logs:"
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