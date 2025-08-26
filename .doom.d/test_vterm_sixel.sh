#!/bin/bash

echo "=== VTERM SIXEL GRAPHICS TESTING PROTOCOL ==="
echo "Testing vterm backend implementation for sixel graphics rendering"
echo "Timestamp: $(date)"
echo

# Phase 1: Fresh Start
echo "PHASE 1: Fresh Start Protocol"
echo "Killing all existing Emacs processes..."
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9 2>/dev/null || echo "No Emacs processes found"
sleep 1

echo "Starting fresh Emacs.app..."
osascript -e 'tell application "Emacs" to activate'
sleep 3

echo "Checking for startup errors..."
WARNINGS=$(emacsclient --eval "(with-current-buffer \"*Warnings*\" (buffer-substring-no-properties (max 1 (- (point-max) 2000)) (point-max)))" 2>&1)
echo "Warnings buffer content:"
echo "$WARNINGS"
echo

# Phase 2: Backend Verification
echo "PHASE 2: Backend Verification"
echo "Checking vterm backend configuration..."

TERMINT_BACKEND=$(emacsclient --eval "(bound-and-true-p termint-backend)" 2>&1)
echo "termint-backend: $TERMINT_BACKEND"

VTERM_AVAILABLE=$(emacsclient --eval "(fboundp 'vterm)" 2>&1)
echo "vterm function available: $VTERM_AVAILABLE"

JUPYTER_STATA_AVAILABLE=$(emacsclient --eval "(fboundp 'jupyter-stata)" 2>&1)
echo "jupyter-stata function available: $JUPYTER_STATA_AVAILABLE"
echo

# Phase 3: Clean Buffer Setup
echo "PHASE 3: Clean Buffer Setup"
echo "Cleaning up any existing Stata buffers..."
emacsclient --eval "(when (get-buffer \"*jupyter-stata*\") (let ((kill-buffer-query-functions nil)) (kill-buffer \"*jupyter-stata*\")))" 2>&1
echo

# Phase 4: Create Test Org Buffer
echo "PHASE 4: Creating Test Org Buffer with Stata Code"
emacsclient --eval "(progn
  (switch-to-buffer \"vterm-stata-test.org\")
  (org-mode)
  (erase-buffer)
  (insert \"#+begin_src stata :session *jupyter-stata*\\nsysuse auto, clear\\nscatter price mpg, title(\\\"vterm Sixel Test\\\")\\n#+end_src\")
  (goto-char (point-min))
  (org-babel-next-src-block))" 2>&1

echo "Executing Stata code block..."
EXECUTE_RESULT=$(emacsclient --eval "(org-babel-execute-src-block)" 2>&1)
echo "Execution result: $EXECUTE_RESULT"
echo

# Phase 5: Wait for Jupyter Startup
echo "PHASE 5: Waiting for Jupyter Startup"
sleep 5

# Phase 6: Verify Buffer and Mode
echo "PHASE 6: Buffer and Mode Verification"
BUFFER_EXISTS=$(emacsclient --eval "(get-buffer \"*jupyter-stata*\")" 2>&1)
echo "Jupyter-stata buffer exists: $BUFFER_EXISTS"

if [[ "$BUFFER_EXISTS" != "nil" ]]; then
    BUFFER_MODE=$(emacsclient --eval "(when (get-buffer \"*jupyter-stata*\") (with-current-buffer \"*jupyter-stata*\" (format \"Buffer mode: %s\" major-mode)))" 2>&1)
    echo "Buffer major mode: $BUFFER_MODE"
    
    MODE_LINE=$(emacsclient --eval "(when (get-buffer \"*jupyter-stata*\") (with-current-buffer \"*jupyter-stata*\" (format \"Mode name: %s\" mode-name)))" 2>&1)
    echo "Mode name: $MODE_LINE"
    
    # Check buffer content for sixel sequences vs graphics
    BUFFER_CONTENT=$(emacsclient --eval "(when (get-buffer \"*jupyter-stata*\") (with-current-buffer \"*jupyter-stata*\" (buffer-substring-no-properties (max 1 (- (point-max) 1000)) (point-max))))" 2>&1)
    echo "Buffer content (last 1000 chars):"
    echo "---START BUFFER CONTENT---"
    echo "$BUFFER_CONTENT"
    echo "---END BUFFER CONTENT---"
    
    # Check for sixel sequences
    if echo "$BUFFER_CONTENT" | grep -q ""; then
        echo "SIXEL SEQUENCES DETECTED: Raw sixel strings found in buffer (BAD - should be rendered as graphics)"
    else
        echo "NO RAW SIXEL SEQUENCES: Good - may indicate graphics are being rendered"
    fi
else
    echo "ERROR: Jupyter-stata buffer was not created"
fi
echo

# Phase 7: Screenshot Capture
echo "PHASE 7: Screenshot Capture"
echo "Focusing Emacs and capturing screenshot..."
osascript -e 'tell application "Emacs" to activate'
screencapture -T 0.5 vterm-sixel-test.png
if [[ -f "vterm-sixel-test.png" ]]; then
    sips -Z 1900 vterm-sixel-test.png
    echo "Screenshot saved as vterm-sixel-test.png (resized to max 1900px)"
else
    echo "ERROR: Screenshot capture failed"
fi
echo

# Phase 8: Summary
echo "PHASE 8: Test Summary"
echo "=== VTERM SIXEL GRAPHICS TEST RESULTS ==="
echo "Backend configured: $TERMINT_BACKEND"
echo "vterm available: $VTERM_AVAILABLE"
echo "jupyter-stata available: $JUPYTER_STATA_AVAILABLE"
echo "Buffer created: $BUFFER_EXISTS"
if [[ "$BUFFER_EXISTS" != "nil" ]]; then
    echo "Buffer mode: $BUFFER_MODE"
    echo "Mode name: $MODE_LINE"
fi

echo
echo "KEY QUESTION: Are sixel graphics rendering as actual visual images?"
echo "Check the screenshot (vterm-sixel-test.png) to verify:"
echo "- SUCCESS: If scatter plot appears as actual graphic image"
echo "- FAILURE: If raw sixel escape sequences are visible as text"
echo
echo "Test completed at: $(date)"
