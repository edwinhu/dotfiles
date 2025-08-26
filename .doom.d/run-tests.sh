#!/bin/bash

# run-tests.sh - Script to run euporie integration tests

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo -e "${BLUE}=== Euporie Integration Test Runner ===${NC}"
echo "Test directory: $SCRIPT_DIR"

# Check if emacs is available
if ! command -v emacs &> /dev/null; then
    echo -e "${RED}ERROR: emacs not found in PATH${NC}"
    exit 1
fi

# Check if doom emacs directory exists
if [ ! -d "$HOME/.emacs.d" ]; then
    echo -e "${YELLOW}WARNING: Doom Emacs directory not found at ~/.emacs.d${NC}"
fi

# Function to run tests in batch mode
run_batch_tests() {
    echo -e "${BLUE}Running tests in batch mode...${NC}"
    cd "$SCRIPT_DIR"
    emacs --batch --load run-euporie-integration-tests.el --eval "(run-euporie-tests-batch)" || {
        echo -e "${RED}Batch tests failed${NC}"
        return 1
    }
}

# Function to run tests with doom emacs client
run_doom_tests() {
    echo -e "${BLUE}Running tests with doom emacs client...${NC}"
    
    # Check if doom emacs is running
    if ! pgrep -f "emacs.*daemon" > /dev/null; then
        echo -e "${YELLOW}Starting Emacs daemon...${NC}"
        emacs --daemon &
        sleep 3
    fi
    
    cd "$SCRIPT_DIR"
    emacsclient --eval "(progn
        (add-to-list 'load-path \"$SCRIPT_DIR\")
        (load-file \"$SCRIPT_DIR/run-euporie-integration-tests.el\")
        (run-euporie-tests-interactive))" || {
        echo -e "${RED}Doom tests failed${NC}"
        return 1
    }
}

# Function to validate test environment
validate_environment() {
    echo -e "${BLUE}Validating test environment...${NC}"
    cd "$SCRIPT_DIR"
    emacsclient --eval "(progn
        (add-to-list 'load-path \"$SCRIPT_DIR\")
        (load-file \"$SCRIPT_DIR/run-euporie-integration-tests.el\")
        (euporie-integration-validate-test-environment))" || {
        echo -e "${YELLOW}Environment validation had issues - check output${NC}"
    }
}

# Function to run only automatic graphics tests
run_graphics_tests() {
    echo -e "${BLUE}Running automatic graphics display tests...${NC}"
    cd "$SCRIPT_DIR"
    emacsclient --eval "(progn
        (add-to-list 'load-path \"$SCRIPT_DIR\")
        (load-file \"$SCRIPT_DIR/run-euporie-integration-tests.el\")
        (run-euporie-automatic-graphics-tests))" || {
        echo -e "${RED}Graphics tests failed${NC}"
        return 1
    }
}

# Show usage
show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "OPTIONS:"
    echo "  --batch         Run tests in batch mode (no GUI)"
    echo "  --doom          Run tests with doom emacs client (requires running daemon)"
    echo "  --validate      Validate test environment only"
    echo "  --graphics      Run only automatic graphics display tests"
    echo "  --help          Show this help message"
    echo
    echo "Examples:"
    echo "  $0 --validate   # Check if test environment is ready"
    echo "  $0 --graphics   # Run critical automatic graphics tests"
    echo "  $0 --doom       # Run all tests interactively with doom"
    echo "  $0 --batch      # Run all tests in batch mode"
}

# Parse command line arguments
case "${1:-}" in
    --batch)
        run_batch_tests
        ;;
    --doom)
        run_doom_tests
        ;;
    --validate)
        validate_environment
        ;;
    --graphics)
        run_graphics_tests
        ;;
    --help|-h)
        show_usage
        ;;
    "")
        echo -e "${YELLOW}No option specified. Use --help for usage information.${NC}"
        echo
        validate_environment
        ;;
    *)
        echo -e "${RED}Unknown option: $1${NC}"
        show_usage
        exit 1
        ;;
esac

echo -e "${GREEN}Test execution completed.${NC}"
echo -e "Check log file: ${BLUE}~/euporie-integration-test.log${NC}"