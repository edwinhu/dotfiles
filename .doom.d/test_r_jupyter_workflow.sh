#!/bin/bash
set -e

echo "=== R Jupyter Workflow Testing Agent Starting ==="
echo "Mission: Verify plotly error fix in R jupyter workflow"
echo ""

# Step 1: Clean Emacs restart
echo "Step 1: Killing existing Emacs processes and starting fresh..."
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9 2>/dev/null || echo "No Emacs processes to kill"
sleep 2

echo "Starting fresh Emacs instance..."
osascript -e 'tell application "Emacs" to activate'
echo "Waiting for Emacs startup and daemon initialization..."
sleep 5

# Step 2: Check Emacs is running and responsive
echo ""
echo "Step 2: Verifying Emacs is responsive..."
emacsclient --eval "(message \"Emacs is responsive - daemon running\")" || {
    echo "ERROR: Emacs daemon not responding - waiting longer..."
    sleep 5
    emacsclient --eval "(message \"Emacs daemon ready\")" || {
        echo "FATAL: Cannot connect to Emacs daemon"
        exit 1
    }
}

# Step 3: Check for startup warnings
echo ""
echo "Step 3: Checking for startup warnings..."
warnings_content=$(emacsclient --eval "(with-current-buffer \"*Warnings*\" (buffer-substring-no-properties (max 1 (- (point-max) 1000)) (point-max)))")
echo "Warnings buffer content:"
echo "$warnings_content"

# Step 4: Open test.org file
echo ""
echo "Step 4: Opening ~/projects/wander2/test.org..."
emacsclient ~/projects/wander2/test.org
sleep 2

# Step 5: Check if jupyter-r function is available
echo ""
echo "Step 5: Verifying jupyter-r function availability..."
jupyter_r_available=$(emacsclient --eval "(fboundp 'jupyter-r)")
echo "jupyter-r function available: $jupyter_r_available"

# Step 6: Check current buffer and prepare for testing
echo ""
echo "Step 6: Checking current buffer and org-mode status..."
current_buffer=$(emacsclient --eval "(buffer-name)")
echo "Current buffer: $current_buffer"

major_mode=$(emacsclient --eval "major-mode")
echo "Major mode: $major_mode"

# Step 7: Take initial screenshot showing test.org file
echo ""
echo "Step 7: Taking initial screenshot showing test.org open..."
osascript -e 'tell application "Emacs" to activate'
sleep 0.5
screencapture -T 0.5 r-test-org-initial.png
echo "Screenshot saved: r-test-org-initial.png"

# Step 8: Execute R code block (this will need to be done interactively)
echo ""
echo "Step 8: READY FOR INTERACTIVE TESTING"
echo "Please navigate to the R code block in test.org and press C-RET"
echo "This script will continue monitoring for results..."
echo ""

# Monitor for jupyter-r buffer creation
echo "Monitoring for *jupyter-r* buffer creation..."
for i in {1..30}; do
    jupyter_buffer=$(emacsclient --eval "(get-buffer \"*jupyter-r*\")")
    if [ "$jupyter_buffer" != "nil" ]; then
        echo "✓ *jupyter-r* buffer detected after ${i} seconds!"
        break
    fi
    echo "Waiting for *jupyter-r* buffer... (${i}/30)"
    sleep 1
done

if [ "$jupyter_buffer" = "nil" ]; then
    echo "WARNING: *jupyter-r* buffer not detected after 30 seconds"
    echo "Manual testing may be required"
else
    echo "✓ *jupyter-r* buffer successfully created"
    
    # Take screenshot showing the buffer
    echo "Taking screenshot showing jupyter-r buffer..."
    sleep 2
    osascript -e 'tell application "Emacs" to activate'
    screencapture -T 0.5 r-jupyter-buffer-created.png
    echo "Screenshot saved: r-jupyter-buffer-created.png"
fi

echo ""
echo "=== Testing Script Complete ==="
echo "Next steps:"
echo "1. Manually navigate to R code block in test.org"  
echo "2. Press C-RET to execute the R ggplot code"
echo "3. Verify no plotly errors appear"
echo "4. Confirm sixel graphics display inline"
echo "5. Take final screenshots documenting success"

