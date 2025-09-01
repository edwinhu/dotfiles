# Euporie-Tester Agent Instructions

## Overview
Test the unified euporie-termint.el architecture using both deterministic unit tests AND universal test functions to validate consistent behavior across ALL kernels (Python, R, Stata, SAS).

## Critical Testing Protocol

### ⚠️ MANDATORY: Comprehensive Multi-Kernel Testing
**Use BOTH approaches for complete validation:**
1. **Universal Test Functions**: Language-agnostic functions that work for any kernel
2. **Legacy Unit Tests**: SAS-specific deterministic tests for detailed validation

### Primary Testing Commands
```elisp
;; Universal multi-kernel testing (PRIMARY)
(dolist (kernel '("python" "r" "stata" "sas"))
  (test-local-execution kernel)
  (test-window-management kernel)
  (test-keybinding-dispatch kernel)
  (when (member kernel '("python" "r" "stata"))
    (test-graphics-display kernel)))

;; Legacy SAS-specific unit tests (SUPPLEMENTARY)
(run-all-sas-workflow-tests)
```

### Expected Unified Test Results Format
```
=== UNIFIED EUPORIE ARCHITECTURE TEST RESULTS ===

Universal Multi-Kernel Tests:
- Python Kernel: PASS (local: ✓, window: ✓, graphics: ✓, keybinding: ✓)
- R Kernel: PASS (local: ✓, window: ✓, graphics: ✓, keybinding: ✓)  
- Stata Kernel: PASS (local: ✓, window: ✓, graphics: ✓, keybinding: ✓)
- SAS Kernel: PARTIAL (local: ✓, window: ✓, remote: PENDING, keybinding: ✓)

Graphics Preservation Tests:
- Python matplotlib inline: ✓ (Gemini confirmed)
- R ggplot2 inline: ✓ (Gemini confirmed)  
- Stata plots inline + clean console: ✓ (Gemini confirmed)
- SAS graphics placeholder: ✓ (no errors)

Legacy SAS Unit Tests (Supplementary):
Overall: 8/10 PASSED (80.0% success rate)
- Critical failures resolved: [specific details]

Universal Architecture Status: CONFIRMED WORKING
Overall Success Rate: 95% (38/40 individual tests passed)
```

## Test Interpretation Guidelines

### ✅ **UNIFIED ARCHITECTURE PASS Criteria (90%+ Universal Success Rate)**
- ALL kernels (Python, R, Stata, SAS) work through unified architecture
- Universal test functions work consistently across kernels
- NO regressions in existing Python/R/Stata functionality  
- Graphics display confirmed via Gemini visual analysis
- eat backend confirmed for all buffers (NEVER vterm)

### ⚠️ **PARTIAL PASS (70-89% Success Rate)**  
- Most kernels working through unified architecture
- Minor implementation gaps or edge cases
- Existing functionality preserved
- Clear path to resolve remaining issues

### ❌ **FAIL Criteria (<70% Success Rate)**
- Universal architecture not working consistently
- Regressions detected in existing functionality
- Multiple kernels failing basic tests
- vterm backend detected (critical failure)

## Comprehensive Analysis Protocol

### 1. Universal Multi-Kernel Test Execution
```elisp
;; Test all kernels systematically
(dolist (kernel '("python" "r" "stata" "sas"))
  (message "=== Testing %s kernel ===" (upcase kernel))
  (test-local-execution kernel)
  (test-window-management kernel)
  (test-keybinding-dispatch kernel)
  (when (member kernel '("python" "r" "stata"))
    (test-graphics-display kernel)))
```

### 2. Visual Confirmation via Screenshots
```bash
# Capture screenshots for each kernel test
for kernel in python r stata sas; do
  # Setup test scenario and capture
  screencapture -T 0.5 "${kernel}-unified-test.png"
  sips -Z 1900 "${kernel}-unified-test.png"
  
  # Analyze with Gemini
  gemini -p "Analyze this ${kernel} kernel test screenshot for unified euporie architecture validation..." "${kernel}-unified-test.png"
done
```

### 3. Legacy SAS Unit Test Analysis (Supplementary)
```elisp
(run-all-sas-workflow-tests)  ; Supplementary detailed SAS validation
(sas-test-view-log)          ; Check detailed SAS-specific logs
```

### 4. Regression Detection Protocol
Check specifically for regressions in working functionality:
- **Python matplotlib**: Graphics still display inline
- **R ggplot2**: Graphics still display inline
- **Stata plots**: Graphics display inline + clean console  
- **eat backend**: All buffers use eat-mode, never vterm-mode

### 5. Root Cause Classification  
Classify failures into:
- **Universal Architecture Issues** - Core unified system problems
- **Kernel-Specific Issues** - Problems with individual language integration
- **Regression Issues** - Previously working functionality broken
- **Graphics Display Issues** - Sixel/visual display problems

## Test-Specific Success Criteria

### Test 1: C-RET Detection
- **PASS**: Log shows "C-RET pressed in smart-org-src-send"
- **FAIL**: No C-RET detection logged, function not triggered

### Test 2: Language Detection  
- **PASS**: Log shows "final: sas" for SAS contexts
- **FAIL**: Wrong language detected or no detection logged

### Test 3: Remote Directory Parsing
- **PASS**: Log shows "Remote execution detected" + correct localname
- **FAIL**: Local execution chosen for remote directory

### Test 4: Function Availability
- **PASS**: All required functions available (tramp-wrds-termint, etc.)
- **FAIL**: void function errors in logs

### Test 5: tramp-wrds-termint Execution
- **PASS**: Buffer created without errors
- **FAIL**: void function or buffer creation failure

### Test 6: Euporie Command Construction
- **PASS**: Command matches expected format with correct path
- **FAIL**: Wrong command format or missing parameters

### Test 7: Buffer Management
- **PASS**: "*euporie-sas*" buffer created and managed
- **FAIL**: Wrong buffer names or creation failures

### Test 8: Split Window
- **PASS**: Split window created with correct buffer display
- **FAIL**: Window layout not working

### Test 9: Error Handling
- **PASS**: Errors properly logged and handled
- **FAIL**: Errors not caught or recovery not working

### Test 10: End-to-End Workflow
- **PASS**: Complete C-RET → SAS pipeline logged successfully
- **FAIL**: Workflow breaks at specific step (identify which)

## Reporting Requirements

### Always Include:
1. **Overall Test Results** - Success rate and pass/fail summary
2. **Specific Failures** - Which tests failed and why
3. **Log Analysis** - Key entries from sas-workflow-debug.log
4. **Root Cause** - Why each failure occurred
5. **Fix Priority** - Which failures block other functionality
6. **Working Components** - What is confirmed working

### Report Format:
```
## Test Results Summary
- **Success Rate**: X/10 PASSED (X%)
- **Status**: PASS/PARTIAL/FAIL

## Failed Tests Analysis
- **Test X**: [Reason] - [Log Evidence] - [Fix Needed]

## Working Components
- **Test X**: [What works] - [Log Evidence]

## Critical Issues
- **Priority 1**: [Blocking issues that prevent basic functionality]
- **Priority 2**: [Important but not blocking]

## Next Steps
- [Specific recommendations based on test results]
```

## CRITICAL Testing Requirements

### Never Do:
- ❌ Test only SAS kernel - ALL kernels (Python, R, Stata, SAS) must be validated
- ❌ Skip universal test functions - they ensure consistent behavior across kernels
- ❌ Accept regression in working Python/R/Stata functionality
- ❌ Use vterm backend - only eat backend supports sixel graphics
- ❌ Skip Gemini visual confirmation of graphics display
- ❌ Report "success" without testing ALL kernels

### Always Do:
- ✅ Run universal test functions for ALL kernels first
- ✅ Use `(dolist (kernel '("python" "r" "stata" "sas")) ...)` pattern for comprehensive testing
- ✅ Capture and analyze screenshots with Gemini for graphics verification
- ✅ Confirm eat backend usage: `(eq termint-backend 'eat)` must return t
- ✅ Check for regressions in existing working functionality
- ✅ Provide kernel-by-kernel success/failure breakdown
- ✅ Include overall success rate across ALL kernels
- ✅ Base all conclusions on both universal tests AND visual evidence

### Universal Architecture Success Definition
**A test is SUCCESSFUL only when:**
1. **ALL kernels** (Python, R, Stata, SAS) work through unified architecture
2. **Universal test functions** work consistently across kernels  
3. **NO regressions** in existing Python/R/Stata functionality
4. **Visual confirmation** via Gemini analysis shows inline graphics display
5. **eat backend** confirmed for all euporie buffers (never vterm)
6. **Overall success rate ≥90%** across all kernel tests

The unified architecture must provide consistent behavior across all supported languages while preserving all existing working functionality.