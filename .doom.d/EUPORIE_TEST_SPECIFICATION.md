# Euporie Integration Test Specification

## Overview

This document defines the comprehensive testing strategy for the Emacs-euporie integration, using the **working R example** as the reference standard for all languages (Python, R, Stata).

## Reference Standard: Working R Example

### Visual Layout Standard
- **Left side**: Org-src buffer with R code (ggplot2 scatter plot)
- **Right side**: euporie-r buffer with inline graphics display  
- **Split-window setup**: Perfect horizontal split with code left, output right
- **Inline graphics**: Beautiful native ggplot2 plot displayed directly in terminal using sixel protocol

### Code Pattern Standard
```r
library(ggplot2)
data(iris)
p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 2) +
  labs(title = "R Test: Iris Dataset",
       x = "Sepal Length", 
       y = "Petal Length") +
  theme_minimal()
print(p)
```

### Success Criteria
1. **Window Layout**: Horizontal split with org-src left, euporie-* buffer right
2. **Graphics Display**: Inline sixel images appear automatically after code execution
3. **Professional Quality**: High-resolution plots with proper colors and formatting
4. **No Manual Commands**: Zero user intervention required for graphics display
5. **Buffer Integration**: Seamless terminal integration within Emacs eat backend

## Test Categories

### 1. Core Functionality Tests

#### Kernel Detection Tests
- `euporie-integration/kernel-detection-python`
- `euporie-integration/kernel-detection-r`
- `euporie-integration/kernel-detection-stata`

**Purpose**: Verify that org-src buffer languages are correctly mapped to euporie kernels.

**Expected Behavior**: 
- Python code blocks → "python" kernel
- R/r code blocks → "r" kernel  
- Stata code blocks → "stata" kernel

#### Buffer Management Tests
- `euporie-integration/python-buffer-creation`
- `euporie-integration/r-buffer-creation`
- `euporie-integration/stata-buffer-creation`

**Purpose**: Validate euporie console buffer creation and process management.

**Expected Behavior**:
- Buffers created with correct names (*euporie-python*, *euporie-r*, *euporie-stata*)
- Live processes established within 15 seconds
- Console content appears (> 100 characters indicating successful startup)

### 2. Window Layout Tests (Reference Standard Compliance)

#### Split Window Layout Tests
- `euporie-integration/split-window-layout-python`
- `euporie-integration/split-window-layout-r` *(reference standard)*
- `euporie-integration/split-window-layout-stata`

**Purpose**: Ensure all languages achieve the same professional split-window layout as the working R example.

**Expected Behavior**:
- Single window → horizontal split after code execution
- Org-src buffer positioned on left side
- Euporie console buffer positioned on right side
- Focus remains on org-src buffer after execution
- Layout matches reference standard dimensions

### 3. Graphics Display Tests

#### Inline Graphics Tests
- `euporie-integration/python-matplotlib-graphics`
- `euporie-integration/r-ggplot2-graphics` *(reference standard)*
- `euporie-integration/stata-graph-graphics`

**Purpose**: Verify inline graphics display using sixel protocol matching reference quality.

**Target Graphics Code**:

**Python (Target)**:
```python
import matplotlib.pyplot as plt
import numpy as np
x = np.linspace(0, 10, 100)
y = np.sin(x)
plt.plot(x, y)
plt.title('Python Test: Sine Wave')
plt.show()
```

**R (Reference Standard)**:
```r
library(ggplot2)
data(iris)
p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 2) +
  labs(title = "R Test: Iris Dataset") +
  theme_minimal()
print(p)
```

**Stata (Target)**:
```stata
sysuse auto, clear
graph twoway scatter mpg weight, title("Stata Test: Auto Dataset")
```

**Expected Behavior**:
- Graphics appear inline in console buffer
- High-resolution, professional-quality display
- Sixel escape sequences present in buffer content
- No manual commands required for display

### 4. Keybinding Tests

#### C-RET Keybinding Tests
- `euporie-integration/c-ret-keybinding-python`
- `euporie-integration/c-ret-keybinding-r`
- `euporie-integration/c-ret-keybinding-stata`

**Purpose**: Validate C-RET keybinding functionality across all supported languages.

**Expected Behavior**:
- C-RET properly bound in org-src buffers
- Keybinding executes without errors
- Code execution triggers split-window layout
- Works consistently across all three languages

### 5. Integration Workflow Tests

#### Complete Workflow Tests
- `euporie-integration/complete-python-workflow`
- `euporie-integration/complete-r-workflow-reference` *(reference standard)*
- `euporie-integration/complete-stata-workflow`

**Purpose**: Test end-to-end user workflows matching the reference standard.

**Workflow Steps**:
1. Create org-src buffer with plotting code
2. Execute C-RET keybinding
3. Verify split-window layout creation
4. Validate console buffer creation and process startup
5. Confirm graphics display inline
6. Verify focus management (stays on org-src buffer)

### 6. Error Handling Tests

#### Error Condition Tests
- `euporie-integration/missing-kernel-error`
- `euporie-integration/empty-code-handling`
- `euporie-integration/buffer-cleanup-on-kill`

**Purpose**: Ensure robust error handling and resource management.

**Expected Behavior**:
- Graceful handling of unsupported languages
- No errors with empty or whitespace-only code
- Proper buffer cleanup when processes are killed
- Ability to restart consoles after cleanup

## Test Execution Guidelines

### Prerequisites
- `pixi` executable must be available
- Euporie environment properly configured
- termint.el and eat backend installed
- All three kernels (python3, ir, stata) available via pixi

### Test Runner Functions

#### Full Test Suite
```elisp
(euporie-integration-run-all-tests)
```

#### Quick Development Tests
```elisp
(euporie-integration-run-quick-tests)
```

#### Individual Test Execution
```elisp
(ert-run-tests-interactively "euporie-integration/complete-r-workflow-reference")
```

### Debug Logging

All tests write detailed logs to: `~/euporie-integration-test.log`

Log includes:
- Test execution timestamps
- Window layout details
- Buffer creation and process status
- Graphics detection results
- Error conditions and recovery

### Performance Expectations

- Console startup: < 15 seconds
- Graphics display: < 5 seconds after code execution
- Window layout: Immediate after code execution
- Test suite completion: < 5 minutes

## Success Metrics

### Layout Success
- ✅ Horizontal split created automatically
- ✅ Org-src buffer on left, console on right
- ✅ Focus remains on org-src buffer
- ✅ Layout matches reference standard dimensions

### Graphics Success
- ✅ Inline graphics appear automatically
- ✅ Professional quality matching reference standard
- ✅ Sixel protocol functioning correctly
- ✅ No manual intervention required

### Integration Success  
- ✅ C-RET keybinding works consistently
- ✅ All three languages achieve identical behavior
- ✅ Error conditions handled gracefully
- ✅ Resources cleaned up properly

## Target Behaviors Summary

| Language | Target Graphics | Expected Output | Protocol |
|----------|----------------|-----------------|----------|
| Python   | matplotlib plots → sixel display | Like R reference | sixel |
| R        | ggplot2 plots → sixel display | **Reference standard** | sixel |  
| Stata    | graph twoway → sixel display | Like R reference | kitty/sixel |

All three languages should achieve **identical user experience** matching the working R example quality and workflow.

## Test Maintenance

### When to Update Tests
- New euporie features added
- Graphics protocols changed
- Window management behavior modified
- New language support added

### Test File Location
- Main test suite: `/Users/vwh7mb/dotfiles/.doom.d/tests/test-euporie-integration.el`
- Test logs: `~/euporie-integration-test.log`
- This specification: `/Users/vwh7mb/dotfiles/.doom.d/EUPORIE_TEST_SPECIFICATION.md`

### Continuous Integration
Tests should be run:
- Before any euporie-related commits
- After Emacs configuration changes
- When adding new language support
- During troubleshooting sessions

The reference standard ensures consistent, professional-quality emacs-euporie integration across all supported languages.