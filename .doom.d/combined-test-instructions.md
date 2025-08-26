# Testing Agent Instructions

You are a specialized testing agent for Euporie-Stata euporie native graphics integration with eat backend. Your mission is to execute the comprehensive testing protocol in AGENT1.md to verify the newly fixed sixel graphics implementation.

## Critical Context
The developer agent has resolved critical issues:
- Buffer permission problems ("text-read-only" errors)
- Sixel display integration problems  
- Terminal spacing for plot persistence

## Your Task
Execute ALL tests from AGENT1.md in sequence:
1. Fresh Start Protocol - Kill Emacs, clean start, check warnings
2. Module Loading Verification - Verify all functions load correctly
3. Single Stata Plot Test - Execute scatter plot with sixel display
4. Sequential Plot Test - Add histogram, verify both plots persist
5. Buffer Permission Verification - Confirm no modification errors
6. Screenshot Evidence - Capture visual proof of sixel graphics
7. Window Configuration - Verify proper split-window layout

## Success Criteria
- No buffer permission errors
- Actual sixel graphics visible in terminal buffer
- Multiple plots persist simultaneously  
- Screenshots show visual sixel graphics display
- All module functions load without errors

## Output Requirements
1. Execute each test step from AGENT1.md
2. Log all results to ~/stata-sixel-test-results.log with timestamps
3. Take screenshot as stata-sixel-fix-test.png
4. Report final PASS/FAIL status for each component
5. Document any failures with exact error messages

Execute the testing protocol now.# AGENT1: Euporie-Stata Sixel Graphics Testing Protocol

## Mission
Test the newly fixed sixel graphics implementation for Euporie-Stata integration following the comprehensive verification protocol.

## Critical Test Focus - Fixed Sixel Graphics
The developer agent has resolved:
- ✅ Critical buffer permission issues ("text-read-only" errors)
- ✅ Sixel display integration problems
- ✅ Terminal spacing for plot persistence

## Required Tests

### 1. Fresh Start Protocol
```bash
# Kill all existing Emacs processes
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9

# Delete compiled files
find ~/.doom.d -name "*.elc" -delete

# Start fresh Emacs
osascript -e 'tell application "Emacs" to activate'

# Wait 3 seconds for full initialization
sleep 3

# Check for startup errors
emacsclient --eval "(with-current-buffer \"*Warnings*\" (buffer-substring-no-properties (max 1 (- (point-max) 2000)) (point-max)))"
```

### 2. Module Loading Verification
```bash
# Verify jupyter-termint.el loads
emacsclient --eval "(fboundp 'euporie-stata)"

# Check for Stata support
emacsclient --eval "(fboundp 'euporie-stata)"

# Confirm eat backend availability
emacsclient --eval "(fboundp 'eat)"

# Test termint presence
emacsclient --eval "(fboundp 'termint-define)"
```

### 3. Stata Jupyter Testing - Single Plot
```bash
# Clean up any existing Stata buffers
emacsclient --eval "(when (get-buffer \"*euporie-stata*\") (let ((kill-buffer-query-functions nil)) (kill-buffer \"*euporie-stata*\")))"

# Create test org buffer with single Stata plot
emacsclient --eval "(progn
  (switch-to-buffer \"test-stata-single.org\")
  (org-mode)
  (insert \"#+begin_src stata :session *euporie-stata*\\nsysuse auto, clear\\nscatter price mpg, title(\\\"Sixel Fix Test\\\")\\n#+end_src\")
  (goto-char (point-min))
  (org-babel-next-src-block))"

# Execute the code block
emacsclient --eval "(org-babel-execute-src-block)"

# Wait for jupyter startup and plot execution
sleep 5

# Verify buffer exists
emacsclient --eval "(get-buffer \"*euporie-stata*\")"

# Check for sixel content (actual graphics)
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (save-excursion (goto-char (point-min)) (search-forward \"sixel\" nil t)))"
```

### 4. Sequential Plot Test (Critical Fix Verification)
```bash
# Add second plot to same org buffer
emacsclient --eval "(progn
  (with-current-buffer \"test-stata-single.org\")
  (goto-char (point-max))
  (insert \"\\n\\n#+begin_src stata :session *euporie-stata*\\nhistogram price, title(\\\"Second Sixel Test\\\")\\n#+end_src\")
  (org-babel-previous-src-block))"

# Execute second plot
emacsclient --eval "(org-babel-execute-src-block)"

# Wait for execution
sleep 3

# Verify both plots persist in buffer
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (buffer-substring-no-properties (max 1 (- (point-max) 4000)) (point-max)))"
```

### 5. Buffer Permission Verification
```bash
# Test that buffer modification works without errors
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (condition-case err (progn (goto-char (point-max)) (insert \"\\n# Test modification\\n\") (message \"✓ Buffer modification successful\")) (error (message \"✗ Buffer error: %s\" err))))"
```

### 6. Screenshot Evidence Collection
```bash
# Focus Emacs and capture screenshot
osascript -e 'tell application "Emacs" to activate'
screencapture -T 0.5 stata-sixel-fix-test.png
sips -Z 1900 stata-sixel-fix-test.png
```

### 7. Window Configuration Verification
```bash
# Check window split
emacsclient --eval "(length (window-list))"

# Verify org buffer on left
emacsclient --eval "(buffer-name (window-buffer (frame-first-window)))"

# Verify jupyter buffer on right
emacsclient --eval "(buffer-name (window-buffer (next-window)))"
```

## Expected Success Criteria
- ✅ No "text-read-only" buffer permission errors
- ✅ Actual sixel graphics visible in eat terminal buffer  
- ✅ Proper terminal spacing prevents plot overwriting
- ✅ Sequential plots persist in same jupyter buffer
- ✅ img2sixel command produces visible graphics (not just text)
- ✅ Multiple plots coexist with proper character spacing

## Expected Failure Points (Now Fixed)
- ❌ Buffer modification errors → Should be RESOLVED
- ❌ Sixel graphics not displaying → Should be RESOLVED  
- ❌ Plots overwriting each other → Should be RESOLVED
- ❌ Terminal not treating graphics as character space → Should be RESOLVED

## Testing Instructions for Agent
1. Execute all tests in sequence
2. Document exact error messages if any failures occur
3. Capture screenshots showing sixel graphics display
4. Verify buffer behavior matches expected success criteria
5. Create comprehensive test log with timestamps
6. Report final pass/fail status for each component

## Log File
Write all results to: `~/stata-sixel-test-results.log`

## Agent Success Definition
Test passes when:
- Fresh Emacs starts without errors
- Stata jupyter buffer creates successfully
- Single scatter plot displays as actual sixel graphic
- Sequential histogram plot also displays as sixel graphic
- Both plots persist simultaneously in buffer
- No buffer permission errors occur
- Screenshots confirm visual sixel graphics display