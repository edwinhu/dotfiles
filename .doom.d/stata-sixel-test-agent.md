# Stata Euporie Integration Testing Agent

## Mission
Test the updated Stata euporie integration with native graphics display using eat backend following the specific workflow provided. Focus on verifying the regression fix and new 'show' command functionality.

## Test Protocol

### Phase 1: Fresh Emacs Start
1. Kill all existing Emacs processes: `ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9`
2. Start fresh Emacs: `osascript -e 'tell application "Emacs" to activate'`
3. Wait 3 seconds for full initialization
4. Check for startup warnings: `emacsclient --eval "(with-current-buffer \"*Warnings*\" (buffer-substring-no-properties (max 1 (- (point-max) 2000)) (point-max)))"`

### Phase 2: Open Test File and Execute
1. Open test.org: `emacsclient /Users/vwh7mb/projects/wander2/test.org`
2. Navigate to Stata code block and prepare for execution
3. Execute Stata block with C-RET (simulated via elisp)
4. Verify *euporie-stata* buffer creation: `emacsclient --eval "(get-buffer \"*euporie-stata*\")"`
5. Check for configuration messages in buffer

### Phase 3: Test Specific Stata Workflow
Execute this exact sequence in the *euporie-stata* buffer:
```
sysuse auto, clear
scatter price mpg  
show
```

### Phase 4: Verification
1. Monitor debug messages when running 'show' command
2. Check for PNG conversion success
3. Verify img2sixel displays graphics inline
4. Confirm no "Manually triggering sixel display" messages
5. Take screenshots at each step

### Phase 5: Results Documentation
Document:
- Debug messages from 'show' command
- PNG conversion success/failure
- Inline sixel display status  
- Whether regression is fixed
- Screenshot evidence of functionality

## Success Criteria
- 'show' command displays debug messages about finding/converting graphs
- Sixel graphics appear inline in *euporie-stata* buffer
- No unwanted "Manually triggering" messages
- Regression is resolved with reliable inline graphics

## Commands to Execute

```bash
# Phase 1: Fresh start
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9
osascript -e 'tell application "Emacs" to activate'
sleep 3
emacsclient --eval "(with-current-buffer \"*Warnings*\" (buffer-substring-no-properties (max 1 (- (point-max) 2000)) (point-max)))"

# Phase 2: Open test file
emacsclient /Users/vwh7mb/projects/wander2/test.org
sleep 1

# Phase 3: Execute Stata block (via elisp simulation of C-RET)
emacsclient --eval "(progn (goto-char (point-min)) (search-forward \"#+begin_src stata\" nil t) (org-babel-execute-src-block))"
sleep 3

# Check buffer creation
emacsclient --eval "(if (get-buffer \"*euporie-stata*\") (message \"✓ *euporie-stata* buffer created\") (message \"✗ Buffer not created\"))"

# Phase 4: Test the workflow
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (goto-char (point-max)) (insert \"sysuse auto, clear\") (comint-send-input))"
sleep 2
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (goto-char (point-max)) (insert \"scatter price mpg\") (comint-send-input))"  
sleep 3
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (goto-char (point-max)) (insert \"show\") (comint-send-input))"
sleep 3

# Capture buffer content for analysis
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (buffer-substring-no-properties (max 1 (- (point-max) 3000)) (point-max)))"

# Take screenshot
osascript -e 'tell application "Emacs" to activate'
screencapture -T 0.5 stata-sixel-regression-test.png
sips -Z 1900 stata-sixel-regression-test.png
```
