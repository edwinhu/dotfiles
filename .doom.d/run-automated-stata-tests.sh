#!/bin/bash
# run-automated-stata-tests.sh - Automated test runner for Stata kernel console cleanliness

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="$HOME"
MASTER_LOG="$LOG_DIR/stata-master-test-results.log"

echo "==============================================="
echo "Stata Kernel Console Cleanliness Test Runner"
echo "==============================================="
echo "Script directory: $SCRIPT_DIR"
echo "Master log: $MASTER_LOG"
echo "Date: $(date)"
echo ""

# Function to log with timestamp
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$MASTER_LOG"
}

# Function to run emacsclient with error handling
run_emacs_command() {
    local command="$1"
    local description="$2"
    local timeout="${3:-60}"
    
    log "INFO: $description"
    log "DEBUG: Running emacs command with $timeout second timeout"
    
    # Use timeout to prevent hanging
    if timeout "$timeout" emacsclient --eval "$command" 2>&1 | tee -a "$MASTER_LOG"; then
        log "INFO: ✓ $description - SUCCESS"
        return 0
    else
        local exit_code=$?
        if [ $exit_code -eq 124 ]; then
            log "ERROR: ✗ $description - TIMEOUT after $timeout seconds"
        else
            log "ERROR: ✗ $description - FAILED (exit code: $exit_code)"
        fi
        return $exit_code
    fi
}

# Clear previous logs
log "INFO: Starting Stata kernel console cleanliness test suite"
log "INFO: Clearing previous test logs"

# Load master test suite
log "INFO: Loading master test suite"
if ! run_emacs_command "(progn (add-to-list 'load-path \"$SCRIPT_DIR\") (load \"$SCRIPT_DIR/test-stata-master-suite.el\") (message \"Master test suite loaded successfully\"))" "Loading master test suite" 30; then
    log "FATAL: Failed to load master test suite. Exiting."
    exit 1
fi

# Validate environment
log "INFO: Validating test environment"
run_emacs_command "(stata-master-validate-environment)" "Environment validation" 20

# Determine test mode
TEST_MODE="${1:-critical}"
case "$TEST_MODE" in
    "full")
        log "INFO: Running FULL test suite (all tests)"
        TEST_COMMAND="(stata-master-run-all-tests)"
        TEST_TIMEOUT=300  # 5 minutes for full suite
        ;;
    "critical"|*)
        log "INFO: Running CRITICAL tests only (quick validation)"
        TEST_COMMAND="(stata-master-run-critical-tests-only)"
        TEST_TIMEOUT=120  # 2 minutes for critical tests
        ;;
esac

# Run the tests
log "INFO: Executing $TEST_MODE tests"
if run_emacs_command "$TEST_COMMAND" "Test execution" "$TEST_TIMEOUT"; then
    log "INFO: Test execution completed successfully"
    TEST_SUCCESS=true
else
    log "ERROR: Test execution failed or timed out"
    TEST_SUCCESS=false
fi

# Generate analysis
log "INFO: Generating test analysis"
run_emacs_command "(stata-master-analyze-all-failures)" "Failure analysis" 30

# Show summary
echo ""
echo "==============================================="
echo "TEST EXECUTION SUMMARY"
echo "==============================================="

if [ "$TEST_SUCCESS" = true ]; then
    echo "✓ Test suite execution: SUCCESS"
else
    echo "✗ Test suite execution: FAILED"
fi

echo "Test mode: $TEST_MODE"
echo "Master log: $MASTER_LOG"

# Show recent log entries
if [[ -f "$MASTER_LOG" ]]; then
    echo ""
    echo "Recent log entries:"
    echo "-------------------"
    tail -15 "$MASTER_LOG"
fi

# Check for specific success indicators
echo ""
echo "Key validation checks:"
echo "---------------------"

if grep -q "console cleanliness: PASSED" "$MASTER_LOG" 2>/dev/null; then
    echo "✓ Console cleanliness tests: PASSED"
else
    echo "? Console cleanliness tests: Check log for details"
fi

if grep -q "No counter messages" "$MASTER_LOG" 2>/dev/null; then
    echo "✓ Counter message elimination: CONFIRMED"
else
    echo "? Counter message elimination: Check log for details"  
fi

if grep -q "COMPLETE" "$MASTER_LOG" 2>/dev/null; then
    echo "✓ Test completion: CONFIRMED"
else
    echo "? Test completion: Check log for details"
fi

# Usage instructions
echo ""
echo "==============================================="
echo "NEXT STEPS"
echo "==============================================="
echo "1. Review master log: $MASTER_LOG"
echo "2. Check individual test logs in $LOG_DIR/"
echo "3. For detailed analysis: emacsclient --eval '(stata-master-analyze-all-failures)'"
echo ""
echo "Usage:"
echo "  $0 critical    # Run critical tests only (default, ~2 min)"
echo "  $0 full        # Run complete test suite (~5 min)"
echo ""

# Exit with appropriate code
if [ "$TEST_SUCCESS" = true ]; then
    log "INFO: Automated test execution completed successfully"
    exit 0
else
    log "ERROR: Automated test execution completed with failures"
    exit 1
fi