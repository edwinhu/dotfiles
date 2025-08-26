# Euporie Native Graphics Solution Testing Agent

## Mission
Test the euporie-console native graphics implementation with eat backend. Euporie handles all graphics conversion internally using terminal protocols (sixel/kitty/iterm) - no manual conversion needed.

## Critical Test Focus - Hybrid Graphics Solution

### 1. Fresh Start Protocol
- Complete fresh Emacs.app restart to load the hybrid implementation
- Verify clean startup without errors
- Check Warnings buffer for any loading issues

### 2. Visual Graphics Test
Execute Stata plots and verify actual images appear (not sixel strings):
- `sysuse auto, clear`
- `scatter price mpg, title("Hybrid Test 1")`
- `histogram price, title("Hybrid Test 2")`

### 3. Sequential Persistence
- Verify both plots remain visible with proper spacing
- Check that terminal allocates proper character space for both plots

### 4. Hybrid Verification
- Confirm sixel data is present but hidden
- Verify images overlay on top of sixel data
- Check that no raw sixel escape sequences are visible to user

## Expected Behavior with Hybrid Approach

### Terminal Spacing
- Sixel data ensures Jupyter allocates proper character space
- Sequential plots don't overlap or overwrite each other

### Visual Display
- Image overlays show actual graphics instead of escape sequences
- Professional, clean appearance with real visual graphics

### Sequential Persistence
- Multiple plots persist because terminal accounts for sixel space
- Both scatter plot and histogram remain visible simultaneously

### Clean Appearance
- Users see graphics, not technical sixel strings
- No raw escape sequences visible
- Image overlays positioned correctly over sixel data

## Key Verification Points
1. Execute scatter → see actual scatter plot image (not sixel text)
2. Execute histogram → both scatter and histogram visible simultaneously
3. Proper vertical spacing between sequential plots
4. No raw sixel escape sequences visible to user
5. Image overlays positioned correctly over sixel data

## Success Criteria
- ✓ Actual visual graphics display instead of sixel strings
- ✓ Sequential plots persist with proper terminal spacing
- ✓ Clean, professional appearance
- ✓ No escape sequences visible to user
- ✓ Both plots remain visible in same buffer

## Hybrid Approach Validation
- Sixel sequences provide terminal space allocation (hidden from user)
- Image overlays provide visual graphics display
- Combined approach solves both spacing and visual rendering issues

## Testing Commands

### Fresh Start
```bash
# Kill all Emacs processes
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9

# Wait and start fresh
sleep 2
osascript -e 'tell application "Emacs" to activate'

# Wait for full initialization
sleep 3

# Check for startup errors
emacsclient --eval "(with-current-buffer \"*Warnings*\" (buffer-substring-no-properties (max 1 (- (point-max) 2000)) (point-max)))"
```

### Module Verification
```bash
# Check jupyter-termint.el loads
emacsclient --eval "(fboundp 'euporie-stata)"

# Check for Stata support
emacsclient --eval "(fboundp 'euporie-stata)"

# Confirm eat backend
emacsclient --eval "(fboundp 'eat)"

# Test termint presence
emacsclient --eval "(fboundp 'termint-define)"
```

### Stata Buffer Testing
```bash
# Clean up existing Stata buffers
emacsclient --eval "(when (get-buffer \"*euporie-stata*\") (let ((kill-buffer-query-functions nil)) (kill-buffer \"*euporie-stata*\")))"

# Create test org buffer with sequential Stata plots
emacsclient --eval "(progn
  (switch-to-buffer \"test-hybrid-stata.org\")
  (org-mode)
  (insert \"#+begin_src stata :session *euporie-stata*\\nsysuse auto, clear\\nscatter price mpg, title(\\\"Hybrid Test 1\\\")\\n#+end_src\\n\\n#+begin_src stata :session *euporie-stata*\\nhistogram price, title(\\\"Hybrid Test 2\\\")\\n#+end_src\")
  (goto-char (point-min))
  (org-babel-next-src-block))"

# Execute first plot
emacsclient --eval "(org-babel-execute-src-block)"

# Wait for jupyter startup and plot generation
sleep 3

# Move to second plot and execute
emacsclient --eval "(progn (org-babel-next-src-block) (org-babel-execute-src-block))"

# Wait for second plot generation
sleep 3
```

### Screenshot and Verification
```bash
# Focus Emacs and capture screenshot
osascript -e 'tell application "Emacs" to activate'
screencapture -T 0.5 hybrid-stata-test.png
sips -Z 1900 hybrid-stata-test.png

# Verify Stata buffer exists
emacsclient --eval "(get-buffer \"*euporie-stata*\")"

# Check for hybrid implementation (sixel data present but hidden)
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (save-excursion (goto-char (point-min)) (search-forward \"sixel\" nil t)))"

# Check for image overlays
emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (length (overlays-in (point-min) (point-max))))"
```

### Window Configuration
```bash
# Check window split
emacsclient --eval "(length (window-list))"

# Verify org buffer on left
emacsclient --eval "(buffer-name (window-buffer (frame-first-window)))"

# Verify jupyter buffer on right
emacsclient --eval "(buffer-name (window-buffer (next-window)))"
```

## Test Results Documentation
- Document all test results with timestamps
- Capture screenshots showing hybrid solution working
- Log any errors or unexpected behavior
- Confirm success/failure against all criteria
- Provide specific recommendations if issues found

## Agent Instructions
1. Execute all test commands in sequence
2. Capture detailed output at each step  
3. Take screenshots to verify visual results
4. Document exact success/failure status
5. Provide comprehensive test report
6. Never ask for manual intervention
7. Resize screenshots below 2000px using sips