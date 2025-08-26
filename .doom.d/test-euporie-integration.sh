#!/bin/bash

# test-euporie-integration.sh - Command line test runner for Euporie integration tests
# Usage: ./test-euporie-integration.sh [test-type]
# Test types: all, quick, graphics, layout, prerequisites

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_LOG="$HOME/euporie-integration-test.log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() {
    echo -e "${BLUE}[$(date +'%Y-%m-%d %H:%M:%S')] $1${NC}"
}

error() {
    echo -e "${RED}[ERROR] $1${NC}" >&2
}

success() {
    echo -e "${GREEN}[SUCCESS] $1${NC}"
}

warning() {
    echo -e "${YELLOW}[WARNING] $1${NC}"
}

# Check if Emacs is running
check_emacs() {
    if ! pgrep -f "[Ee]macs" > /dev/null; then
        error "Emacs is not running. Please start Emacs first."
        exit 1
    fi
    
    # Try emacsclient connection
    if ! emacsclient --eval "(message \"Testing connection\")" > /dev/null 2>&1; then
        error "Cannot connect to Emacs daemon. Please check your Emacs setup."
        exit 1
    fi
    
    log "✓ Emacs daemon connection verified"
}

# Check prerequisites
check_prerequisites() {
    log "Checking prerequisites..."
    
    # Check pixi
    if ! command -v pixi &> /dev/null; then
        error "pixi command not found. Please install pixi first."
        exit 1
    fi
    log "✓ pixi found: $(pixi --version)"
    
    # Check Emacs modules
    local modules_ok=true
    
    if ! emacsclient --eval "(featurep 'termint)" 2>/dev/null | grep -q "t"; then
        error "termint.el not loaded"
        modules_ok=false
    fi
    
    if ! emacsclient --eval "(featurep 'euporie-termint)" 2>/dev/null | grep -q "t"; then
        error "euporie-termint.el not loaded" 
        modules_ok=false
    fi
    
    if [ "$modules_ok" = false ]; then
        error "Required Emacs modules not loaded. Please check your Doom configuration."
        exit 1
    fi
    
    log "✓ Required Emacs modules loaded"
    success "All prerequisites met"
}

# Run tests via emacsclient
run_test() {
    local test_function=$1
    local test_description=$2
    
    log "Running $test_description..."
    
    # Clear log file at start of test run
    > "$TEST_LOG"
    
    # Run the test
    if emacsclient --eval "(progn (load-file \"$SCRIPT_DIR/run-euporie-tests.el\") ($test_function))" 2>&1; then
        success "$test_description completed"
        
        # Show log summary if available
        if [[ -f "$TEST_LOG" ]]; then
            local log_size=$(wc -l < "$TEST_LOG")
            log "Test log written: $TEST_LOG ($log_size lines)"
            
            # Show last few lines of log for quick feedback
            if [[ $log_size -gt 0 ]]; then
                echo ""
                echo "Recent log entries:"
                tail -5 "$TEST_LOG" | while read line; do
                    echo "  $line"
                done
                echo ""
            fi
        fi
    else
        error "$test_description failed"
        exit 1
    fi
}

# Show usage
show_usage() {
    echo "Usage: $0 [test-type]"
    echo ""
    echo "Test types:"
    echo "  all           - Run complete test suite (default)"
    echo "  quick         - Run quick core functionality tests"  
    echo "  graphics      - Run graphics display tests"
    echo "  layout        - Run window layout tests"
    echo "  prerequisites - Check system prerequisites only"
    echo "  help          - Show this help"
    echo ""
    echo "Examples:"
    echo "  $0                    # Run all tests"
    echo "  $0 quick             # Run quick tests"
    echo "  $0 prerequisites     # Check prerequisites"
    echo ""
    echo "Test log: $TEST_LOG"
    echo "Test spec: $SCRIPT_DIR/EUPORIE_TEST_SPECIFICATION.md"
}

# Main script logic
main() {
    local test_type=${1:-all}
    
    echo "Euporie Integration Test Runner"
    echo "==============================="
    echo ""
    
    case $test_type in
        "help"|"-h"|"--help")
            show_usage
            exit 0
            ;;
        "prerequisites")
            check_emacs
            check_prerequisites
            success "Prerequisites check completed"
            exit 0
            ;;
        "all")
            check_emacs
            check_prerequisites
            run_test "run-euporie-tests-interactive" "full test suite"
            ;;
        "quick")
            check_emacs
            check_prerequisites 
            run_test "run-euporie-quick-tests" "quick tests"
            ;;
        "graphics")
            check_emacs
            check_prerequisites
            run_test "run-euporie-graphics-tests" "graphics tests"
            ;;
        "layout")
            check_emacs
            check_prerequisites
            run_test "run-euporie-layout-tests" "layout tests"
            ;;
        *)
            error "Unknown test type: $test_type"
            show_usage
            exit 1
            ;;
    esac
    
    success "Test run completed successfully!"
    log "For detailed results, check: $TEST_LOG"
}

# Run main function with all arguments
main "$@"