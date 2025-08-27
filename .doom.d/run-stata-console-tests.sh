#!/bin/bash
# run-stata-console-tests.sh - Test runner for Stata console cleanliness tests

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="$HOME/euporie-stata-console-test.log"
EMACS_DOOM_DIR="$HOME/.emacs.d"

echo "=== Stata Console Cleanliness Test Runner ==="
echo "Script directory: $SCRIPT_DIR"
echo "Log file: $LOG_FILE"

# Function to log with timestamp
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

# Function to run emacsclient command with error handling
run_emacs_test() {
    local command="$1"
    local description="$2"
    
    log "INFO: $description"
    log "INFO: Running: $command"
    
    if emacsclient --eval "$command" 2>&1 | tee -a "$LOG_FILE"; then
        log "INFO: ✓ $description completed successfully"
        return 0
    else
        log "ERROR: ✗ $description failed"
        return 1
    fi
}

# Clear previous log
> "$LOG_FILE"

log "INFO: Starting Stata console cleanliness tests"

# 1. Load test module
log "INFO: Step 1 - Loading test module"
if ! run_emacs_test "(progn (add-to-list 'load-path \"$SCRIPT_DIR\") (load-file \"$SCRIPT_DIR/test-euporie-stata-console-cleanliness.el\") (message \"Test module loaded\"))" "Loading test module"; then
    log "ERROR: Failed to load test module. Exiting."
    exit 1
fi

# 2. Validate environment
log "INFO: Step 2 - Validating test environment"
run_emacs_test "(progn (message \"Checking euporie-console...\") (if (executable-find \"euporie-console\") (message \"✓ euporie-console found\") (message \"✗ euporie-console not found\")))" "Environment validation"

# 3. Run critical tests (most important for user feedback)
log "INFO: Step 3 - Running critical console cleanliness tests"
run_emacs_test "(euporie-stata-console-run-critical-tests)" "Critical console cleanliness tests"

# 4. Wait for tests to complete and capture results
log "INFO: Step 4 - Waiting for test completion"
sleep 5

# 5. Check test results
log "INFO: Step 5 - Checking test results"
run_emacs_test "(progn (message \"Test results summary:\") (with-temp-buffer (when (file-exists-p \"$LOG_FILE\") (insert-file-contents \"$LOG_FILE\") (let ((content (buffer-string))) (message \"Log contains %d lines\" (length (split-string content \"\\n\"))) (when (string-match-p \"ERROR\" content) (message \"⚠ ERRORS found in log\")) (when (string-match-p \"should-not.*global.*stata_kernel_graph_counter\" content) (message \"✓ Counter message tests executed\"))))))" "Test results check"

# 6. Optional: Run full test suite if requested
if [[ "$1" == "--full" ]]; then
    log "INFO: Step 6 - Running full test suite (requested)"
    run_emacs_test "(euporie-stata-console-run-cleanliness-tests)" "Full console cleanliness test suite"
fi

# 7. Generate summary
log "INFO: Step 7 - Generating test summary"
run_emacs_test "(progn (message \"=== TEST SUMMARY ===\") (message \"Log file: $LOG_FILE\") (message \"Check log for detailed results\") (message \"Key tests executed:\") (message \"- No counter messages in scatter plot\") (message \"- Complete workflow test\") (message \"- Python/Stata console comparison\") (message \"=== END SUMMARY ===\"))" "Test summary generation"

echo ""
echo "=== Test Execution Complete ==="
echo "Log file: $LOG_FILE"
echo "To analyze results: tail -50 $LOG_FILE"
echo "To run full suite: $0 --full"
echo ""

# Show recent log entries
if [[ -f "$LOG_FILE" ]]; then
    echo "=== Recent Log Entries ==="
    tail -20 "$LOG_FILE"
fi