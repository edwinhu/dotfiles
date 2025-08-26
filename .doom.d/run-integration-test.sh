#!/bin/bash

echo "=== Jupyter Stata Integration Test ==="
echo "$(date): Starting comprehensive test..."

# Phase 1: Fresh Start
echo "Phase 1: Killing existing Emacs processes..."
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9 2>/dev/null || true

echo "Starting fresh Emacs.app..."
osascript -e 'tell application "Emacs" to activate'
sleep 3

# Phase 2: Check startup health  
echo "Phase 2: Checking startup status..."
emacsclient --eval "(message \"Emacs ready for testing\")" 2>/dev/null || {
    echo "ERROR: emacsclient not responsive"
    exit 1
}

# Phase 3: Test C-RET workflow
echo "Phase 3: Testing C-RET functionality..."
emacsclient --eval "(progn 
    (find-file \"/Users/vwh7mb/projects/wander2/test.org\")
    (goto-char (point-min))
    (when (search-forward \"begin_src stata\" nil t)
        (org-babel-execute-src-block)
        (message \"C-RET test executed\")))" 

sleep 5

# Phase 4: Test image display
echo "Phase 4: Testing image display..."
emacsclient --eval "(if (file-exists-p \"/Users/vwh7mb/.stata_kernel_cache/graph0.png\")
    (progn
        (jupyter-termint-display-image-inline \"/Users/vwh7mb/.stata_kernel_cache/graph0.png\" \"*jupyter-stata*\")
        (message \"Image display test completed\"))
    (message \"Graphics file not found\"))"

# Phase 5: Screenshot evidence
echo "Phase 5: Capturing screenshot..."
osascript -e 'tell application "Emacs" to activate'
screencapture -T 0.5 final-integration-test.png
sips -Z 1900 final-integration-test.png

echo "=== Test Complete ==="
echo "Check final-integration-test.png for visual verification"
echo "$(date): Integration test finished"
