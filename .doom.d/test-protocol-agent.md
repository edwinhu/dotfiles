# Euporie Stata Integration Test Agent Instructions

You are a specialized testing agent for verifying Euporie Stata integration in Doom Emacs. Execute this comprehensive test protocol systematically.

## Test Objective:
Verify that both C-RET execution and the emacsclient image display command work correctly from a fresh Emacs startup.

## Test Protocol:

### Phase 1: Fresh Environment Setup
1. Kill all Emacs processes: `ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9`
2. Start fresh Emacs.app: `osascript -e 'tell application "Emacs" to activate'`
3. Wait 3 seconds for complete initialization
4. Check for startup errors: `emacsclient --eval "(with-current-buffer \"*Warnings*\" (buffer-substring-no-properties (max 1 (- (point-max) 2000)) (point-max)))"`
5. Document any warnings found

### Phase 2: Module Loading Verification
6. Test function availability: `emacsclient --eval "(fboundp 'euporie-termint-display-image-inline)"`
7. Test Stata jupyter function: `emacsclient --eval "(fboundp 'euporie-stata)"`
8. Verify buffer creation capability: `emacsclient --eval "(get-buffer \"*euporie-stata*\")"`

### Phase 3: C-RET Functionality Test
9. Open test file: `emacsclient --eval "(progn (find-file \"/Users/vwh7mb/projects/wander2/test.org\") (message \"Test file opened\"))"`
10. Navigate to Stata src block: `emacsclient --eval "(progn (goto-char (point-min)) (search-forward \"begin_src stata\") (message \"Found Stata block\"))"`
11. Execute C-RET: `emacsclient --eval "(org-babel-execute-src-block)"`
12. Wait 5 seconds for execution
13. Check for split windows: `emacsclient --eval "(length (window-list))"`
14. Verify euporie-stata buffer: `emacsclient --eval "(get-buffer \"*euporie-stata*\")"`

### Phase 4: Graphics Generation Check
15. Check for graphics files:
    - `ls -la ~/.stata_kernel_cache/graph*.png`
    - Document file sizes and timestamps

### Phase 5: Image Display Test
16. Test emacsclient image display: `emacsclient --eval "(euporie-termint-display-image-inline \"/Users/vwh7mb/.stata_kernel_cache/graph0.png\" \"*euporie-stata*\")"`
17. Check for success message in buffer: `emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (save-excursion (goto-char (point-max)) (search-backward \"📊\" nil t)))"`
18. Verify image content appears

### Phase 6: Screenshots and Verification
19. Take screenshot after test: `osascript -e 'tell application "Emacs" to activate' && screencapture -T 0.5 stata-integration-test.png && sips -Z 1900 stata-integration-test.png`
20. Check debug log: `tail -20 ~/euporie-stata-debug.log`

### Phase 7: Results Documentation
21. Create comprehensive test report with pass/fail for each criterion
22. Document exact error messages if any tests fail
23. Provide screenshot evidence of working integration
24. Log results to ~/euporie-stata-test-results.log with timestamp

## Success Criteria - ALL MUST PASS:
- Fresh startup without errors
- C-RET executes without "--INSERT--" message
- Split window layout created properly
- Stata code executes successfully
- Graphics files generated in cache
- emacsclient image display function works
- Image appears inline in euporie-stata buffer
- Complete workflow requires zero manual intervention

Execute each phase systematically and document detailed results for each test criterion.
