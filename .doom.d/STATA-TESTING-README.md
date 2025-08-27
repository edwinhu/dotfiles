# Stata Kernel Console Cleanliness Testing Suite

This comprehensive testing suite validates the fixes for Stata kernel graphics issues in euporie console environments. The primary objective is eliminating unwanted graph counter messages while ensuring professional-grade console behavior.

## Problem Statement

**Issue**: Stata kernel displays unwanted counter messages in console output:
```
global stata_kernel_graph_counter = $stata_kernel_graph_counter + 1
```

**Objective**: Clean console output matching Python/R kernel behavior in euporie.

## Test Suite Architecture

### 1. Console Cleanliness Tests (`test-euporie-stata-console-cleanliness.el`)

**Purpose**: Validate clean console output without graph counter messages

**Key Tests**:
- `euporie-stata-console/no-counter-messages-scatter-plot` - Core validation test
- `euporie-stata-console/multiple-plots-no-counter-pollution` - Sequential plot testing  
- `euporie-stata-console/compare-with-python-cleanliness` - Cross-kernel comparison

**Usage**:
```elisp
(euporie-stata-console-run-cleanliness-tests)        ; All tests
(euporie-stata-console-run-critical-tests)          ; Critical tests only
```

### 2. Performance Tests (`test-stata-graphics-performance.el`)

**Purpose**: Validate graphics pipeline performance and optimization

**Key Tests**:
- `stata-graphics-perf/single-plot-latency` - Individual plot performance
- `stata-graphics-perf/no-infinite-loops` - Validates no hanging/infinite loops
- `stata-graphics-perf/multiple-plots-consistency` - Performance consistency

**Performance Targets**:
- Graphics display < 10 seconds
- No infinite loops or hanging
- Consistent performance across multiple plots

**Usage**:
```elisp
(stata-graphics-run-performance-tests)               ; All performance tests
(stata-graphics-run-critical-performance-tests)     ; Critical performance tests
```

### 3. Integration Tests (`test-euporie-stata-integration.el`)

**Purpose**: End-to-end user workflow validation

**Key Tests**:
- `euporie-stata-integration/complete-user-workflow` - Full startup → graphics workflow
- `euporie-stata-integration/c-ret-keybinding-workflow` - Org-mode integration
- `euporie-stata-integration/comparison-with-python-r` - Multi-kernel comparison

**Usage**:
```elisp
(euporie-stata-run-integration-tests)                ; All integration tests
(euporie-stata-run-critical-integration-tests)      ; Critical integration tests
```

### 4. Master Test Suite (`test-stata-master-suite.el`)

**Purpose**: Coordinates all test categories with comprehensive reporting

**Usage**:
```elisp
(stata-master-run-all-tests)                        ; Complete test suite
(stata-master-run-critical-tests-only)              ; Critical tests from all categories
```

## Quick Start Guide

### Method 1: Automated Script (Recommended)

```bash
# Run critical tests (2 minutes)
cd ~/.doom.d
./run-automated-stata-tests.sh critical

# Run complete test suite (5 minutes)  
./run-automated-stata-tests.sh full
```

### Method 2: Interactive Emacs Testing

```bash
# Load and run tests interactively
emacsclient --eval "(progn 
  (add-to-list 'load-path \"~/.doom.d\") 
  (load \"test-stata-master-suite.el\") 
  (stata-master-run-critical-tests-only))"
```

### Method 3: Individual Test Categories

```bash
# Console cleanliness only
./run-stata-console-tests.sh

# Or load specific test files
emacsclient --eval "(progn
  (load \"~/.doom.d/test-euporie-stata-console-cleanliness.el\")
  (euporie-stata-console-run-critical-tests))"
```

## Test Environment Requirements

### Prerequisites
- `euporie-console` executable available
- `pixi` package manager with Stata kernel
- Project directory: `/Users/vwh7mb/projects/emacs-euporie`
- Emacs packages: `euporie-termint`, `termint`, `eat`, `org`

### Validation
```elisp
(stata-master-validate-environment)
```

## Expected Test Results

### Successful Test Indicators
- **Console Output**: No graph counter messages in scatter plot output
- **Performance**: Graphics display < 10 seconds, no hanging
- **Integration**: Complete workflow from startup to graphics display
- **Comparison**: Stata behavior matches Python/R kernel cleanliness

### Log Files
- Master: `~/stata-master-test-results.log`
- Console: `~/euporie-stata-console-test.log`  
- Performance: `~/stata-graphics-performance.log`
- Integration: `~/euporie-stata-integration.log`

### Screenshots
Test execution automatically captures screenshots in `~/.doom.d/`:
- `stata-integration-step1-startup.png`
- `stata-integration-step3-plot.png`
- `stata-integration-complete.png`

## Test Failure Analysis

### Common Issues and Solutions

**Problem**: `euporie-console not found`
```bash
# Solution: Verify pixi environment
cd /Users/vwh7mb/projects/emacs-euporie
pixi run which euporie-console
```

**Problem**: `Buffer creation timeout`
```bash
# Solution: Check Emacs configuration
emacsclient --eval "(fboundp 'euporie-stata-start)"
```

**Problem**: `Counter messages still appearing`
```bash
# Solution: This indicates the core fix needs verification
# Check stata_kernel implementation
```

### Debugging Commands
```elisp
(stata-master-analyze-all-failures)                 ; Analyze all test failures
(stata-master-view-all-logs)                        ; Open all log files
(euporie-stata-console-analyze-test-failures)       ; Console-specific analysis
```

## Interpreting Test Results

### Success Criteria
✅ **Primary Success**: `euporie-stata-console/no-counter-messages-scatter-plot` PASSES
✅ **Performance Success**: Graphics display completes without hanging  
✅ **Integration Success**: Complete user workflow executes cleanly
✅ **Comparison Success**: Stata matches Python/R console behavior

### Failure Investigation
❌ **Console Cleanliness Failure**: Check for counter messages in output
❌ **Performance Failure**: Look for timeouts or infinite loops
❌ **Integration Failure**: Verify euporie-termint.el configuration
❌ **Environment Failure**: Check prerequisites and paths

## Development Integration

### Running Tests During Development
```bash
# Quick validation after code changes
./run-automated-stata-tests.sh critical

# Full validation before commits
./run-automated-stata-tests.sh full
```

### Continuous Integration
The automated test script returns appropriate exit codes:
- Exit 0: All tests passed
- Exit 1: Tests failed or timed out

### Adding New Tests
1. Choose appropriate test file based on category
2. Follow existing test patterns (`ert-deftest` format)
3. Include proper logging with test-specific log functions
4. Add cleanup in `unwind-protect` blocks
5. Update master suite if needed

## Architecture Notes

### Why These Testing Approaches

**ERT Framework**: Standard Emacs testing framework with good isolation
**Multiple Test Files**: Separates concerns (cleanliness, performance, integration)
**File-based Logging**: Essential for debugging in terminal/batch environments
**Screenshot Capture**: Documents actual behavior for verification
**Automated Cleanup**: Prevents test interference with buffer management

### Test Isolation Strategy

Each test category uses:
- Separate log files for parallel debugging
- Buffer cleanup between tests (`unwind-protect`)
- Process cleanup with `kill-buffer-query-functions nil`
- Fresh euporie startup for integration tests

## Troubleshooting

### Test Hangs or Timeouts
```bash
# Kill stuck processes
ps aux | grep -i "euporie\|stata" | awk '{print $2}' | xargs kill -9

# Clear stuck buffers
emacsclient --eval "(dolist (b '(\"*euporie-stata*\" \"*euporie-python*\" \"*euporie-r*\")) 
  (when (get-buffer b) (let ((kill-buffer-query-functions nil)) (kill-buffer b))))"
```

### Environment Issues  
```bash
# Verify pixi environment
cd /Users/vwh7mb/projects/emacs-euporie
pixi info

# Check Emacs packages
emacsclient --eval "(mapcar (lambda (pkg) (list pkg (featurep pkg))) '(euporie-termint termint eat org))"
```

### Log Analysis
```bash
# Check for specific error patterns
grep -E "ERROR|FAILED|should-not.*failed" ~/stata-master-test-results.log

# Look for console cleanliness results
grep "console cleanliness:" ~/stata-master-test-results.log
```

## Contact and Support

For issues with the testing suite:
1. Check log files for detailed error messages
2. Verify environment prerequisites 
3. Run individual test categories to isolate issues
4. Use `(stata-master-analyze-all-failures)` for automated analysis

The test suite is designed to provide clear feedback on the success of Stata kernel console cleanliness fixes and ensure professional-grade user experience.