#!/bin/bash

echo "=== VTERM SIXEL GRAPHICS TEST (MINIMAL VERSION) ==="
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

echo "Loading minimal vterm version..."
emacsclient --eval "(progn (load \"~/dotfiles/.doom.d/jupyter-termint-minimal.el\") (message \"Minimal version loaded\"))" 2>&1

# Phase 2: Backend Verification
echo "PHASE 2: Backend Verification"
VTERM_AVAILABLE=$(emacsclient --eval "(fboundp 'vterm)" 2>&1)
echo "vterm function available: $VTERM_AVAILABLE"

TERMINT_AVAILABLE=$(emacsclient --eval "(fboundp 'termint-define)" 2>&1)
echo "termint-define available: $TERMINT_AVAILABLE"

STATA_FUNC_AVAILABLE=$(emacsclient --eval "(fboundp 'jupyter-termint-smart-stata-start)" 2>&1)
echo "jupyter-termint-smart-stata-start available: $STATA_FUNC_AVAILABLE"

# Phase 3: Create Test Org Buffer
echo "PHASE 3: Creating Test Org Buffer with Stata Code"
emacsclient --eval "(progn
  (switch-to-buffer \"vterm-minimal-test.org\")
  (org-mode)
  (erase-buffer)
  (insert \"#+begin_src stata :session *jupyter-stata*\\nsysuse auto, clear\\nscatter price mpg, title(\\\"vterm Backend Test\\\")\\n#+end_src\")
  (goto-char (point-min))
  (org-babel-next-src-block))" 2>&1

# Phase 4: Test vterm console creation
echo "PHASE 4: Testing vterm Console Creation"
CREATE_RESULT=$(emacsclient --eval "(jupyter-termint-smart-stata-start)" 2>&1)
echo "Console creation result: $CREATE_RESULT"

sleep 5

# Phase 5: Verify Buffer and Mode
echo "PHASE 5: Buffer and Mode Verification"
BUFFER_EXISTS=$(emacsclient --eval "(get-buffer \"*jupyter-stata*\")" 2>&1)
echo "Jupyter-stata buffer exists: $BUFFER_EXISTS"

if [[ "$BUFFER_EXISTS" != "nil" ]]; then
    BUFFER_MODE=$(emacsclient --eval "(when (get-buffer \"*jupyter-stata*\") (with-current-buffer \"*jupyter-stata*\" (format \"Buffer mode: %s\" major-mode)))" 2>&1)
    echo "Buffer major mode: $BUFFER_MODE"
    
    # Check if it's vterm-mode
    if echo "$BUFFER_MODE" | grep -q "vterm"; then
        echo "✅ SUCCESS: Buffer is using vterm-mode!"
    else
        echo "❌ FAILURE: Buffer is NOT using vterm-mode"
    fi
    
    # Send test command
    echo "PHASE 6: Sending Test Stata Command"
    emacsclient --eval "(termint-jupyter-stata-send-string \"sysuse auto, clear\")" 2>&1
    sleep 2
    emacsclient --eval "(termint-jupyter-stata-send-string \"scatter price mpg, title(\\\"vterm Sixel Test\\\")\")" 2>&1
    sleep 3
    
    # Check buffer content
    BUFFER_CONTENT=$(emacsclient --eval "(when (get-buffer \"*jupyter-stata*\") (with-current-buffer \"*jupyter-stata*\" (buffer-substring-no-properties (max 1 (- (point-max) 500)) (point-max))))" 2>&1)
    echo "Buffer content (last 500 chars):"
    echo "---START BUFFER CONTENT---"
    echo "$BUFFER_CONTENT"
    echo "---END BUFFER CONTENT---"
    
    # Look for sixel sequences or rendered graphics
    if echo "$BUFFER_CONTENT" | grep -q ""; then
        echo "⚠️  SIXEL SEQUENCES DETECTED: Raw sixel strings found"
        echo "This suggests graphics are being sent but may not be rendered as images"
    else
        echo "✅ NO RAW SIXEL SEQUENCES: Graphics may be rendered properly"
    fi
else
    echo "❌ ERROR: Jupyter-stata buffer was not created"
fi

# Phase 6: Screenshot
echo "PHASE 6: Screenshot Capture"
osascript -e 'tell application "Emacs" to activate'
screencapture -T 0.5 vterm-minimal-test.png
if [[ -f "vterm-minimal-test.png" ]]; then
    sips -Z 1900 vterm-minimal-test.png
    echo "Screenshot saved as vterm-minimal-test.png"
else
    echo "ERROR: Screenshot capture failed"
fi

echo
echo "=== VTERM BACKEND TEST RESULTS ==="
echo "Key Question: Are sixel graphics rendering as actual visual images?"
echo "Check the screenshot (vterm-minimal-test.png) to verify:"
echo "- SUCCESS: If scatter plot appears as actual graphic image"
echo "- FAILURE: If raw sixel escape sequences are visible as text"
echo
echo "Test completed at: $(date)"
