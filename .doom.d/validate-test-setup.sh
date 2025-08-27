#!/bin/bash
# validate-test-setup.sh - Validate Stata testing setup

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "========================================="
echo "Stata Test Setup Validation"
echo "========================================="
echo ""

# Function for colored output
success() { echo -e "\033[32m✓\033[0m $1"; }
warning() { echo -e "\033[33m⚠\033[0m $1"; }
error() { echo -e "\033[31m✗\033[0m $1"; }
info() { echo -e "\033[34mℹ\033[0m $1"; }

# Check test files exist
info "Checking test files..."
TEST_FILES=(
    "test-euporie-stata-console-cleanliness.el"
    "test-stata-graphics-performance.el"
    "test-euporie-stata-integration.el"
    "test-stata-master-suite.el"
)

for file in "${TEST_FILES[@]}"; do
    if [[ -f "$SCRIPT_DIR/$file" ]]; then
        success "Test file: $file"
    else
        error "Missing test file: $file"
        exit 1
    fi
done

# Check test runner scripts
info "Checking test runner scripts..."
SCRIPTS=(
    "run-stata-console-tests.sh"
    "run-automated-stata-tests.sh"
    "validate-test-setup.sh"
)

for script in "${SCRIPTS[@]}"; do
    if [[ -f "$SCRIPT_DIR/$script" && -x "$SCRIPT_DIR/$script" ]]; then
        success "Test runner: $script"
    else
        if [[ -f "$SCRIPT_DIR/$script" ]]; then
            warning "Test runner not executable: $script"
            chmod +x "$SCRIPT_DIR/$script"
            success "Made executable: $script"
        else
            error "Missing test runner: $script"
            exit 1
        fi
    fi
done

# Check README
if [[ -f "$SCRIPT_DIR/STATA-TESTING-README.md" ]]; then
    success "Documentation: STATA-TESTING-README.md"
else
    warning "Missing documentation: STATA-TESTING-README.md"
fi

# Check external dependencies
info "Checking external dependencies..."

if command -v euporie-console >/dev/null 2>&1; then
    success "euporie-console: $(which euporie-console)"
else
    error "euporie-console not found in PATH"
fi

if command -v pixi >/dev/null 2>&1; then
    success "pixi: $(which pixi)"
else
    error "pixi not found in PATH"
fi

if command -v emacsclient >/dev/null 2>&1; then
    success "emacsclient: $(which emacsclient)"
else
    error "emacsclient not found in PATH"
fi

# Check project directory
PROJECT_DIR="/Users/vwh7mb/projects/emacs-euporie"
if [[ -d "$PROJECT_DIR" ]]; then
    success "Project directory: $PROJECT_DIR"
else
    error "Missing project directory: $PROJECT_DIR"
fi

# Check Stata cache directory
CACHE_DIR="$HOME/.stata_kernel_cache"
if [[ -d "$CACHE_DIR" ]]; then
    success "Stata cache directory: $CACHE_DIR"
else
    warning "Stata cache directory not found (will be created): $CACHE_DIR"
fi

# Test Emacs can load test modules
info "Testing Emacs test module loading..."

if emacsclient --eval "(progn (add-to-list 'load-path \"$SCRIPT_DIR\") (condition-case nil (progn (load \"test-stata-master-suite\") (message \"Master test suite loaded successfully\")) (error (message \"Failed to load test suite\"))))" 2>/dev/null | grep -q "successfully"; then
    success "Emacs can load test modules"
else
    error "Emacs cannot load test modules"
fi

# Test pixi environment (if available)
if [[ -d "$PROJECT_DIR" ]] && command -v pixi >/dev/null 2>&1; then
    info "Testing pixi environment..."
    cd "$PROJECT_DIR"
    
    if pixi run jupyter kernelspec list 2>/dev/null | grep -q stata; then
        success "Pixi environment has Stata kernel"
    else
        warning "Pixi environment may not have Stata kernel configured"
    fi
fi

echo ""
echo "========================================="
echo "Validation Summary"
echo "========================================="
success "Test suite validation completed"
info "Ready to run: ./run-automated-stata-tests.sh critical"
info "Documentation: cat STATA-TESTING-README.md"
echo ""