# Stata Euporie Integration Testing Protocol

## Mission: Comprehensive Fresh Emacs Test for Stata Inline Graphics

Execute systematic 5-phase testing protocol to validate Stata euporie integration works from completely fresh Emacs.app startup with eat backend.

### Phase 1: Complete Fresh Environment Setup
1. Kill ALL Emacs processes: `ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9`
2. Start fresh Emacs.app: `osascript -e 'tell application "Emacs" to activate'`
3. Wait 3 seconds for complete Doom initialization
4. Check *Warnings* buffer: `emacsclient --eval "(with-current-buffer \"*Warnings*\" (buffer-substring-no-properties (max 1 (- (point-max) 2000)) (point-max)))"`
5. Document any startup issues or warnings found

### Phase 2: Module Loading Verification
1. Verify euporie-termint.el loads: `emacsclient --eval "(fboundp 'euporie-stata)"`
2. Check termint-ensure function: `emacsclient --eval "(fboundp 'euporie-termint-ensure-stata-console)"`
3. Check file monitoring: `emacsclient --eval "(fboundp 'euporie-termint-start-file-monitoring)"`
4. Verify eat backend: `emacsclient --eval "(fboundp 'eat)"`
5. Test termint availability: `emacsclient --eval "(fboundp 'termint-define)"`

### Phase 3: Fresh Jupyter Integration Test
1. Clean any existing buffers: `emacsclient --eval "(when (get-buffer \"*euporie-stata*\") (let ((kill-buffer-query-functions nil)) (kill-buffer \"*euporie-stata*\")))"`
2. Create test org buffer with Stata code:
   ```elisp
   emacsclient --eval "(progn
     (switch-to-buffer \"test-stata.org\")
     (org-mode)
     (insert \"#+begin_src stata :session *euporie-stata*\\nsysuse auto, clear\\ndescribe\\n#+end_src\")
     (goto-char (point-min))
     (org-babel-next-src-block))"
   ```
3. Execute C-RET: `emacsclient --eval "(org-babel-execute-src-block)"`
4. Wait 3 seconds for console startup
5. Verify buffer created: `emacsclient --eval "(get-buffer \"*euporie-stata*\")"`
6. Check file monitoring started: Look for temporary files in /tmp
7. Take screenshot: `osascript -e 'tell application "Emacs" to activate' && screencapture -T 0.5 stata-phase3-test.png && sips -Z 1900 stata-phase3-test.png`

### Phase 4: End-to-End Graphics Workflow
1. Test basic console: `emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (process-send-string (get-buffer-process (current-buffer)) \"sysuse auto, clear\\n\"))"`
2. Wait 2 seconds
3. Execute graphics sequence:
   - `scatter price mpg`: `emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (process-send-string (get-buffer-process (current-buffer)) \"scatter price mpg\\n\"))"`
   - Wait 3 seconds, take screenshot: `screencapture -T 0.5 stata-scatter-test.png && sips -Z 1900 stata-scatter-test.png`
   - `histogram price`: `emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (process-send-string (get-buffer-process (current-buffer)) \"histogram price\\n\"))"`
   - Wait 3 seconds, take screenshot: `screencapture -T 0.5 stata-histogram-test.png && sips -Z 1900 stata-histogram-test.png`
   - `graph twoway scatter weight mpg`: `emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (process-send-string (get-buffer-process (current-buffer)) \"graph twoway scatter weight mpg\\n\"))"`
   - Wait 3 seconds, take screenshot: `screencapture -T 0.5 stata-twoway-test.png && sips -Z 1900 stata-twoway-test.png`

### Phase 5: Integration Robustness
1. Test multiple org buffers to same console
2. Test error handling: `emacsclient --eval "(with-current-buffer \"*euporie-stata*\" (process-send-string (get-buffer-process (current-buffer)) \"invalid_command\\n\"))"`
3. Check resource cleanup when console killed
4. Final screenshot: `screencapture -T 0.5 stata-final-test.png && sips -Z 1900 stata-final-test.png`

### Critical Success Validation Checklist
- [ ] Clean startup with no warnings/errors
- [ ] C-RET creates euporie-stata buffer successfully  
- [ ] File monitoring starts automatically
- [ ] Native Stata commands execute normally
- [ ] Graphics appear inline immediately and automatically
- [ ] Multiple graphics display without conflicts
- [ ] Complete workflow: C-RET → Stata command → inline graphics

### Documentation Requirements
- Log all test results with timestamps
- Capture screenshots at each major step
- Document any failures with exact error messages
- Note what works vs. what doesn't work
- Provide recommendations for fixes if needed

## Execute this protocol systematically and report detailed results.