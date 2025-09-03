#!/bin/bash

echo "=== Simple SAS C-RET Test ==="

# Kill any existing Emacs
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9 2>/dev/null || true
sleep 2

# Start fresh Emacs
echo "Starting Emacs..."
osascript -e 'tell application "Emacs" to activate'
sleep 5

# Test if functions are loaded
echo "Testing function availability..."
RESULT=$(emacsclient --eval "(progn
  (message \"Checking function availability...\")
  (let ((functions '(euporie-termint-detect-kernel
                     euporie-termint-send-region-or-line
                     org-babel-execute:sas)))
    (mapcar (lambda (func)
              (if (fboundp func)
                  (format \"✓ %s: loaded\" func)
                (format \"✗ %s: void\" func)))
            functions)))" 2>&1)

echo "Function check result:"
echo "$RESULT"

# If functions are void, try manual loading
echo "Attempting manual module loading..."
emacsclient --eval "(progn
  (message \"Manual loading test...\")
  (condition-case err
      (progn
        (load-file \"~/.doom.d/tramp-qrsh.el\")
        (load-file \"~/.doom.d/euporie-termint.el\") 
        (load-file \"~/.doom.d/ob-sas.el\")
        (message \"✓ All modules loaded manually\"))
    (error (message \"✗ Manual loading failed: %s\" err))))"

echo "=== Test Complete ==="