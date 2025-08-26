# Testing Agent: Stata Euporie Implementation with Native Graphics

## Mission
Test the updated euporie-termint.el Stata implementation focusing on the key improvements:
1. **euporie-console instead of jupyter console**: Uses native terminal-based Jupyter environment
2. **eat backend**: Confirmed using `:backend 'eat` (never vterm for sixel graphics support)
3. **Native graphics protocols**: euporie handles sixel/kitty/iterm graphics automatically
4. **euporie native display**: No manual conversion needed - euporie intercepts jupyter graphics at source

## Testing Protocol

### Phase 1: Clean Environment Setup
1. Kill all existing Emacs processes and start fresh
2. Check for any startup warnings in *Warnings* buffer
3. Kill any existing jupyter-stata buffers to start completely fresh
4. Verify euporie-console is available on system: `which euporie-console`

### Phase 2: Module Loading Verification
1. Test that euporie-termint.el loads correctly: `emacsclient --eval "(fboundp 'euporie-stata)"`
2. Check that eat backend is available: `emacsclient --eval "(fboundp 'eat)"`
3. Verify termint functionality: `emacsclient --eval "(fboundp 'termint-define)"`

### Phase 3: Stata Euporie Buffer Creation
1. Create fresh org buffer with Stata code block:
   ```stata
   #+begin_src stata :session *euporie-stata*
   sysuse auto, clear
   scatter price mpg, title("Euporie Native Graphics Test")
   #+end_src
   ```
2. Use C-' to enter org-src edit mode (split window should appear)
3. Execute with C-RET to create *euporie-stata* buffer
4. Verify buffer uses eat backend (never vterm)
5. Check TERM environment is xterm-kitty for euporie graphics support

### Phase 4: Graphics Testing
1. Execute the scatter plot command in euporie-stata buffer
2. Monitor for euporie native graphics display (no conversion needed)
3. Verify graphics render as actual inline images through euporie protocols
4. Take screenshot showing split window with native euporie graphics
5. Confirm euporie handles all graphics conversion internally

### Phase 5: Environment Variables Verification  
1. Check that TERM=xterm-kitty is set in process environment
2. Verify COLORTERM=truecolor is set
3. Confirm these are NOT set to DUMB or other graphics-blocking values

### Phase 6: Documentation
1. Capture screenshots at key stages:
   - Clean Emacs startup
   - Org-src split window creation
   - jupyter-stata buffer with eat backend
   - Final inline sixel graphics display
2. Resize all screenshots below 2000px using sips
3. Log all test results with timestamps to ~/stata-chafa-test-results.log

## Success Criteria
- No syntax errors on fresh Emacs startup
- euporie-stata buffer creates successfully with eat backend
- TERM environment is xterm-kitty (not DUMB)
- euporie-console handles graphics natively without manual conversion
- Graphics display as actual inline images through euporie native protocols
- Split window layout works correctly (org-src left, euporie right)
- Euporie handles all graphics conversion internally

## Failure Investigation
If any test fails:
- Capture exact error messages
- Check *Warnings* and *Messages* buffers
- Verify euporie-console installation and path
- Check TERM environment variables in process
- Take screenshot of failure state
- Document specific step that failed

## Critical Commands for Testing

```bash
# Clean restart
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9
osascript -e 'tell application "Emacs" to activate'

# Wait 3 seconds then check warnings
emacsclient --eval "(with-current-buffer \"*Warnings*\" (buffer-substring-no-properties (max 1 (- (point-max) 2000)) (point-max)))"

# Verify euporie-console availability
which euporie-console

# Clean existing stata buffers
emacsclient --eval "(when (get-buffer \"*euporie-stata*\") (let ((kill-buffer-query-functions nil)) (kill-buffer \"*euporie-stata*\")))"

# Create test org buffer with euporie test
emacsclient --eval "(progn
  (switch-to-buffer \"test-stata-euporie.org\")
  (org-mode)
  (insert \"#+begin_src stata :session *euporie-stata*\\nsysuse auto, clear\\nscatter price mpg, title(\\\"Euporie Native Graphics Test\\\")\\n#+end_src\")
  (goto-char (point-min))
  (org-babel-next-src-block))"

# Execute code block
emacsclient --eval "(org-babel-execute-src-block)"

# Check buffer exists and backend
emacsclient --eval "(get-buffer \"*euporie-stata*\")"
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (message \"Process: %s\" (process-name (get-buffer-process (current-buffer)))))"

# Check environment variables
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (process-environment (get-buffer-process (current-buffer))))"

# Screenshot capture
osascript -e 'tell application "Emacs" to activate'
screencapture -T 0.5 stata-euporie-test.png
sips -Z 1900 stata-euporie-test.png

# Check for euporie usage in logs (~/euporie-debug.log)
tail -20 ~/euporie-debug.log | grep -i euporie

# Verify sixel content
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (save-excursion (goto-char (point-min)) (search-forward \"sixel\" nil t)))"
```

Execute this comprehensive test suite and report back with results, focusing especially on whether euporie-console is working and displaying native graphics inline without manual conversion.