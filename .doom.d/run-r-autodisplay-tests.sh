#!/bin/bash

# run-r-autodisplay-tests.sh - Test runner for R automatic display investigation

set -e

echo "========================================"
echo "R AUTOMATIC DISPLAY INVESTIGATION"
echo "========================================"
echo "Starting at: $(date)"
echo

# Configuration
DOOM_DIR="/Users/vwh7mb/dotfiles/.doom.d"
PROJECT_DIR="/Users/vwh7mb/projects/emacs-euporie"
LOG_FILE="$HOME/r-autodisplay-test-execution.log"

# Clear previous logs
> "$LOG_FILE"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

log "R autodisplay test execution started"

# Check prerequisites
log "Checking prerequisites..."

if [ ! -d "$PROJECT_DIR" ]; then
    log "ERROR: Project directory not found: $PROJECT_DIR"
    exit 1
fi

if [ ! -f "$DOOM_DIR/test-r-autodisplay.el" ]; then
    log "ERROR: Test file not found: $DOOM_DIR/test-r-autodisplay.el"
    exit 1
fi

if [ ! -f "$DOOM_DIR/r-autodisplay-config.R" ]; then
    log "ERROR: R config file not found: $DOOM_DIR/r-autodisplay-config.R"
    exit 1
fi

log "Prerequisites check passed"

# Check if pixi environment has required R packages
log "Checking pixi environment..."
cd "$PROJECT_DIR"

if ! pixi run R --slave -e "cat('R available\n')" >/dev/null 2>&1; then
    log "ERROR: R not available in pixi environment"
    exit 1
fi

log "Pixi environment check passed"

# Test Emacs setup
log "Testing Emacs configuration..."

# Check if euporie-termint is available
if ! emacsclient --eval "(fboundp 'euporie-r-start)" 2>/dev/null | grep -q "t"; then
    log "ERROR: euporie-r-start function not available in Emacs"
    exit 1
fi

log "Emacs configuration check passed"

# Load test framework
log "Loading R autodisplay test framework..."

emacsclient --eval "(progn
  (load-file \"$DOOM_DIR/test-r-autodisplay.el\")
  (load-file \"$DOOM_DIR/r-graphics-debug-framework.el\")
  (message \"Test framework loaded\"))" 2>&1 | tee -a "$LOG_FILE"

# Run the full debugging pipeline
log "Starting comprehensive R graphics debugging pipeline..."

emacsclient --eval "(r-graphics-debug-full-pipeline)" 2>&1 | tee -a "$LOG_FILE"

# Wait for completion
log "Waiting for debugging pipeline to complete..."
sleep 30

# Run individual ERT tests
log "Running individual ERT tests..."

# Test 1: ggplot2 automatic display
log "Running ggplot2 automatic display test..."
emacsclient --eval "(ert-run-tests-interactively \"r-autodisplay/ggplot2-automatic-display\")" 2>&1 | tee -a "$LOG_FILE"
sleep 10

# Test 2: Base R automatic display  
log "Running base R automatic display test..."
emacsclient --eval "(ert-run-tests-interactively \"r-autodisplay/base-r-automatic-display\")" 2>&1 | tee -a "$LOG_FILE"
sleep 10

# Test 3: Multiple plots test
log "Running multiple plots automatic display test..."
emacsclient --eval "(ert-run-tests-interactively \"r-autodisplay/multiple-plots-automatic-display\")" 2>&1 | tee -a "$LOG_FILE"
sleep 10

# Test 4: Configuration investigation
log "Running R configuration investigation..."
emacsclient --eval "(ert-run-tests-interactively \"r-autodisplay/investigate-r-configuration\")" 2>&1 | tee -a "$LOG_FILE"
sleep 10

# Test 5: Manual vs automatic comparison
log "Running manual vs automatic comparison test..."
emacsclient --eval "(ert-run-tests-interactively \"r-autodisplay/manual-vs-automatic-comparison\")" 2>&1 | tee -a "$LOG_FILE"
sleep 10

# Run helper investigations
log "Running helper investigations..."

emacsclient --eval "(r-autodisplay-investigate-startup-environment)" 2>&1 | tee -a "$LOG_FILE"
sleep 5

emacsclient --eval "(r-autodisplay-investigate-plot-methods)" 2>&1 | tee -a "$LOG_FILE"
sleep 5

# Analyze results
log "Analyzing results..."
emacsclient --eval "(r-graphics-debug-analyze-results)" 2>&1 | tee -a "$LOG_FILE"

# Generate summary report
log "Generating summary report..."

cat >> "$LOG_FILE" << EOF

========================================
SUMMARY REPORT
========================================

Test execution completed at: $(date)

Log files generated:
- Main execution log: $LOG_FILE
- R autodisplay test log: $HOME/r-autodisplay-test.log  
- R graphics debug log: $HOME/r-graphics-debug.log

Next steps:
1. Review the generated log files for test results
2. Identify which tests passed/failed  
3. Review debug analysis and recommendations
4. Implement fixes based on investigation results
5. Re-run tests to verify fixes

Key files for review:
- Test results: $HOME/r-autodisplay-test.log
- Debug analysis: $HOME/r-graphics-debug.log  
- Implementation guide: $DOOM_DIR/R-AUTODISPLAY-TEST-PROTOCOL.md

========================================
EOF

log "Test execution completed. Review log files for results."
echo
echo "Main log file: $LOG_FILE"
echo "R test log: $HOME/r-autodisplay-test.log"
echo "R debug log: $HOME/r-graphics-debug.log"
echo
echo "Review the protocol document for next steps:"
echo "$DOOM_DIR/R-AUTODISPLAY-TEST-PROTOCOL.md"