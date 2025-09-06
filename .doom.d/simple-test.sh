#!/usr/bin/env bash

# simple-test.sh - Simplified multi-language test for remote execution framework
# Tests Python, R, and SAS with the generalized remote execution

set -e

echo "=== Simplified Multi-Language Remote Execution Test ==="
echo "$(date): Starting test for Python, R, and SAS"

# Function to test a specific language
test_language() {
    local language="$1"
    local language_title="${language^}"
    
    echo ""
    echo "=== Testing $language_title ==="
    
    # Clear previous buffers
    echo "Cleaning up existing $language_title buffers..."
    emacsclient --eval "(progn
      (when (get-buffer \"*euporie-$language*\")
        (let ((kill-buffer-query-functions nil))
          (kill-buffer \"*euporie-$language*\")))
      (message \"✓ Cleaned up $language_title buffers\"))"
    
    # Open test file and execute
    echo "Opening test.org and executing $language_title block..."
    emacsclient --eval "(progn
      (find-file \"~/projects/emacs-euporie/test.org\")
      (goto-char (point-min))
      (when (string= \"$language\" \"sas\")
        (require 'ob-sas))
      (if (search-forward \"$language_title Test Block\" nil t)
          (progn
            (search-forward \":dir /sshx:wrds|qrsh::/home/nyu/eddyhu/projects/\" nil t)
            (forward-line 1)
            (org-edit-special)
            (sleep-for 2)
            (message \"✓ Entered $language_title org-src edit buffer\")
            (euporie-termint-send-region-or-line)
            (message \"✓ Executed C-RET for $language_title\"))
        (error \"Could not find $language_title test block\")))"
    
    echo "Waiting for $language_title execution..."
    sleep 20
    
    # Check results
    echo "Checking $language_title results..."
    local success
    success=$(emacsclient --eval "(progn
      (let ((euporie-buffer (get-buffer \"*euporie-$language*\")))
        (if euporie-buffer
            (with-current-buffer euporie-buffer
              (let ((content (buffer-string)))
                (message \"$language_title buffer content length: %d\" (length content))
                (cond
                  ((string= \"$language\" \"python\")
                   (if (string-match-p \"Python Remote Execution Test\" content) t nil))
                  ((string= \"$language\" \"r\")
                   (if (string-match-p \"R Remote Execution Test\" content) t nil))
                  ((string= \"$language\" \"sas\")
                   (if (string-match-p \"Acura\" content) t nil))
                  (t nil))))
          nil)))")
    
    if [ "$success" = "t" ]; then
        echo "✓ $language_title: SUCCESS - Output detected"
    else
        echo "✗ $language_title: FAILED - No expected output"
    fi
    
    return 0
}

# Initialize
echo "Initializing test environment..."

# Kill existing Emacs processes
echo "Killing existing Emacs processes..."
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9 2>/dev/null || true
sleep 2

# Start fresh Emacs
echo "Starting fresh Emacs.app..."
osascript -e 'tell application "Emacs" to activate'
sleep 5

# Clear logs
> ~/sas-workflow-debug.log
> ~/euporie-debug.log  
> ~/euporie-termint-debug.log
> ~/tramp-qrsh-debug.log 2>/dev/null || true

# Configure euporie environment
echo "Configuring euporie environment..."
emacsclient --eval "(progn
  (message \"=== Configuring euporie environment ===\")
  (let ((project-dir \"~/projects/emacs-euporie/\"))
    (cd project-dir)
    (setenv \"PATH\" (concat project-dir \".pixi/envs/default/bin:\" (getenv \"PATH\")))
    (message \"✓ Updated PATH for euporie\")))"

# Test each language
LANGUAGES=("python" "r" "sas")

for language in "${LANGUAGES[@]}"; do
    test_language "$language"
done

# Check logs for remote execution
echo ""
echo "=== Log Analysis ==="
echo "Checking for remote execution in logs..."

for language in "${LANGUAGES[@]}"; do
    local language_title="${language^}"
    echo "- $language_title:"
    
    case $language in
        "sas")
            if grep -q "is-remote.*qrsh" ~/sas-workflow-debug.log 2>/dev/null; then
                echo "  ✓ Remote execution detected"
            else
                echo "  ✗ No remote execution detected"
            fi
            ;;
        "python"|"r") 
            if grep -q "is-remote.*qrsh" ~/euporie-termint-debug.log 2>/dev/null; then
                echo "  ✓ Remote execution detected"
            else
                echo "  ✗ No remote execution detected"
            fi
            ;;
    esac
done

# Take screenshot
echo ""
echo "Taking screenshot..."
osascript -e 'tell application "Emacs" to activate'
sleep 0.5
screencapture -T 0.5 ~/simple-test-screenshot.png
echo "Screenshot saved to ~/simple-test-screenshot.png"

# Show recent log entries
echo ""
echo "=== Recent Log Entries ==="
echo "SAS workflow log (last 10 lines):"
tail -10 ~/sas-workflow-debug.log 2>/dev/null || echo "No SAS log found"

echo ""
echo "Euporie termint log (last 10 lines):"
tail -10 ~/euporie-termint-debug.log 2>/dev/null || echo "No euporie log found"

echo ""
echo "TRAMP QRSH log (last 10 lines):"
tail -10 ~/tramp-qrsh-debug.log 2>/dev/null || echo "No TRAMP log found"

echo ""
echo "=== Test Complete ==="