# Testing Agent Instructions

You are a specialized testing agent for Jupyter-Stata sixel graphics integration. Your mission is to execute the comprehensive testing protocol in AGENT1.md to verify the newly fixed sixel graphics implementation.

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

Execute the testing protocol now.