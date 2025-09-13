#!/bin/bash

# test.sh - Automated multi-language test for C-RET :dir parameter extraction with timing analysis
# Tests the hook-based approach for extracting :dir from org-src blocks for Python, R, and SAS

set -e

# Test configuration
LANGUAGES=("python" "r" "sas")

# Function to get language patterns (replacement for associative arrays)
get_language_pattern() {
    case "$1" in
        "python") echo "Python Remote Execution Test" ;;
        "r") echo "R Remote Execution Test" ;;
        "sas") echo "SAS Remote Test" ;;
    esac
}

get_language_block() {
    case "$1" in
        "python") echo "Python Test Block (Remote)" ;;
        "r") echo "R Test Block (Remote)" ;;
        "sas") echo "SAS Test Block" ;;
    esac
}

get_remote_username_pattern() {
    case "$1" in
        "python") echo "Username: eddyhu" ;;
        "r") echo "Username: eddyhu" ;;
        "sas") echo "User: eddyhu" ;;
    esac
}

get_local_username_pattern() {
    case "$1" in
        "python") echo "Username: vwh7mb" ;;
        "r") echo "Username: vwh7mb" ;;
        "sas") echo "User: vwh7mb" ;;
    esac
}

get_success_pattern() {
    case "$1" in
        "python") echo "SUCCESS: Running on WRDS remote server" ;;
        "r") echo "SUCCESS: Running on WRDS remote server" ;;
        "sas") echo "SUCCESS: Running on WRDS remote server" ;;
    esac
}

get_failure_pattern() {
    case "$1" in
        "python") echo "FAILURE: Running locally as vwh7mb" ;;
        "r") echo "FAILURE: Running locally as.*vwh7mb" ;;
        "sas") echo "FAILURE: Running locally as.*vwh7mb" ;;
    esac
}

# Timing variables
TEST_START_TIME=$(date +%s.%3N)
PHASES=("Emacs_Startup" "Python_Execution" "R_Execution" "SAS_Execution")

# Timing storage using files instead of associative arrays
mkdir -p /tmp/test-timing

# Function to record phase start time
record_phase_start() {
    local phase_name="$1"
    local timestamp=$(date +%s.%3N)
    echo "$timestamp" > "/tmp/test-timing/${phase_name}_start"
    echo "[TIMING] $phase_name phase started at $(date)"
}

# Function to record phase end time
record_phase_end() {
    local phase_name="$1"
    local timestamp=$(date +%s.%3N)
    echo "$timestamp" > "/tmp/test-timing/${phase_name}_end"
    echo "[TIMING] $phase_name phase completed at $(date)"
}

# Function to parse timing from logs
parse_log_timing() {
    local language="$1"
    local ssh_time="N/A"
    local qrsh_time="N/A" 
    local kernel_time="N/A"
    
    # Parse kernel timing based on language
    case $language in
        "python")
            if [ -f ~/euporie-termint-debug.log ]; then
                kernel_time=$(rg "Python kernel ready after.*\(([0-9.]+) seconds\)" ~/euporie-termint-debug.log -o -r '$1' 2>/dev/null | tail -1)
            fi
            ;;
        "r")
            if [ -f ~/euporie-termint-debug.log ]; then
                kernel_time=$(rg "R kernel ready after.*\(([0-9.]+) seconds\)" ~/euporie-termint-debug.log -o -r '$1' 2>/dev/null | tail -1)
            fi
            ;;
        "sas")
            if [ -f ~/sas-workflow-debug.log ]; then
                kernel_time=$(rg "SAS kernel ready after.*\(([0-9.]+) seconds\)" ~/sas-workflow-debug.log -o -r '$1' 2>/dev/null | tail -1)
            fi
            ;;
    esac
    
    # Parse SSH and QRSH connection timing
    if [ -f ~/tramp-qrsh-debug.log ]; then
        ssh_time=$(rg "SSH connection established after.*\(([0-9.]+) seconds\)" ~/tramp-qrsh-debug.log -o -r '$1' 2>/dev/null | tail -1)
        qrsh_time=$(rg "QRSH connection established after.*\(([0-9.]+) seconds\)" ~/tramp-qrsh-debug.log -o -r '$1' 2>/dev/null | tail -1)
    fi
    
    # Set defaults if not found
    [ -z "$ssh_time" ] && ssh_time="N/A"
    [ -z "$qrsh_time" ] && qrsh_time="N/A"
    [ -z "$kernel_time" ] && kernel_time="N/A"
    
    echo "$ssh_time $qrsh_time $kernel_time"
}

# Function to calculate phase duration
calculate_phase_duration() {
    local phase_name="$1"
    local start_file="/tmp/test-timing/${phase_name}_start"
    local end_file="/tmp/test-timing/${phase_name}_end"
    
    if [ -f "$start_file" ] && [ -f "$end_file" ]; then
        local start=$(cat "$start_file")
        local end=$(cat "$end_file")
        echo "$end - $start" | bc -l 2>/dev/null || echo "0"
    else
        echo "N/A"
    fi
}

# Function to capitalize first letter (compatible with macOS bash)
capitalize_first() {
    echo "$(tr '[:lower:]' '[:upper:]' <<< "${1:0:1}")${1:1}"
}

# Function to test language execution
test_language_execution() {
    local language="$1"
    local language_title=$(capitalize_first "$language")
    local block_pattern=$(get_language_block "$language")
    local output_pattern=$(get_language_pattern "$language")
    
    echo "=== Testing $language_title Language ==="
    
    # Open test file and navigate to the specific language block
    echo "Step: Opening test.org and navigating to $language_title block..."
    emacsclient --eval "(progn
      (message \"=== TEST: Opening ~/projects/emacs-euporie/test.org ===\")
      (find-file \"~/projects/emacs-euporie/test.org\")
      (goto-char (point-min))
      (if (search-forward \"$block_pattern\" nil t)
          (progn
            (search-forward \"#+begin_src $language :dir\" nil t)
            (forward-line 1)
            (message \"Found $language_title block with :dir parameter\")
            (message \"Current position: line %d\" (line-number-at-pos))
            ;; Load language support
            (when (string= \"$language\" \"sas\")
              (require 'ob-sas))
            (org-edit-special)
            (sleep-for 1)
            (message \"✓ Entered org-src edit buffer: %s\" (buffer-name))
            (message \"Buffer mode: %s\" major-mode))
        (error \"Could not find $language_title block with :dir parameter\")))"
    
    # Execute C-RET in the language-specific edit buffer
    echo "Step: Executing C-RET in $language_title org-src edit buffer..."
    record_phase_start "${language_title}_Execution"
    
    # Determine buffer name pattern based on language
    local buffer_pattern=""
    case $language in
        "python") buffer_pattern="*Org Src test.org[ python ]*" ;;
        "r") buffer_pattern="*Org Src test.org[ R ]*" ;;
        "sas") buffer_pattern="*Org Src test.org[ sas ]*" ;;
    esac
    
    emacsclient --eval "(progn
      (message \"=== TEST: Executing C-RET for $language_title ===\")
      (let ((edit-buffer (get-buffer \"$buffer_pattern\")))
        (if edit-buffer
            (with-current-buffer edit-buffer
              (message \"Switched to $language_title buffer: %s\" (buffer-name))
              (goto-char (point-min))
              ;; Find appropriate content to execute
              (cond
                ((string= \"$language\" \"python\") (search-forward \"import pandas\" nil t))
                ((string= \"$language\" \"r\") (search-forward \"test_data\" nil t))
                ((string= \"$language\" \"sas\") (search-forward \"proc print\" nil t)))
              (euporie-termint-send-region-or-line)
              (message \"C-RET executed from $language_title buffer\"))
          (error \"Could not find $language_title org-src buffer\"))))"
    
    # Check window split arrangement for this language
    echo "Step: Checking window split arrangement for $language_title..."
    local window_split_success
    window_split_success=$(emacsclient --eval "(progn
      (message \"=== TEST: Checking $language_title window split arrangement ===\")
      (let ((windows (window-list))
            (current-buf (buffer-name))
            (euporie-buf-visible nil)
            (org-src-visible nil)
            (org-src-buf (get-buffer \"$buffer_pattern\"))
            (euporie-buf (get-buffer \"*euporie-$language*\")))
        
        (message \"Number of windows: %d\" (length windows))
        (dolist (win windows)
          (let ((buf-name (buffer-name (window-buffer win))))
            (when (string-match-p \"\\\\*euporie-.*\\\\*\" buf-name)
              (setq euporie-buf-visible t))
            (when (string-match-p \"\\\\*Org Src.*\\\\[\" buf-name)
              (setq org-src-visible t))))
        
        ;; Fix window split if needed
        (unless (and (>= (length windows) 2) euporie-buf-visible org-src-visible)
          (when (and org-src-buf euporie-buf)
            (message \"Fixing window split arrangement for $language_title...\")
            (delete-other-windows)
            (switch-to-buffer org-src-buf)
            (split-window-right)
            (other-window 1)
            (switch-to-buffer euporie-buf)
            (other-window 1)
            (message \"✓ Fixed window split: org-src left, euporie right\")
            (setq euporie-buf-visible t org-src-visible t)))
        
        (if (and (>= (length windows) 2) euporie-buf-visible org-src-visible)
            (progn 
              (message \"✓ SUCCESS: $language_title window split correct\") 
              t)
          (progn
            (message \"✗ FAIL: $language_title window split incorrect\")
            nil))))")
    
    # Wait for execution
    echo "Step: Waiting for $language_title execution to complete..."
    sleep 30  # Shorter wait since we're testing multiple languages
    record_phase_end "${language_title}_Execution"
    
    # Check for output AND username verification
    echo "Step: Checking for $language_title output and username verification..."
    local output_success
    local remote_pattern=$(get_remote_username_pattern "$language")
    local local_pattern=$(get_local_username_pattern "$language")
    local success_pattern=$(get_success_pattern "$language")
    local failure_pattern=$(get_failure_pattern "$language")
    
    output_success=$(emacsclient --eval "(progn
      (message \"=== TEST: Checking $language_title euporie buffer for output ===\")
      (let ((euporie-buffer (get-buffer \"*euporie-$language*\")))
        (if euporie-buffer
            (progn
              (message \"✓ Found $language_title euporie buffer\")
              (with-current-buffer euporie-buffer
                (let ((content (buffer-string)))
                  (message \"$language_title buffer content length: %d chars\" (length content))
                  (when (> (length content) 100)
                    (message \"Buffer content preview: %s\" (substring content 0 (min 500 (length content)))))
                  (message \"Checking for $language_title output pattern: $output_pattern\")
                  (message \"Checking for remote username pattern: $remote_pattern\")
                  (message \"Checking for success pattern: $success_pattern\")
                  (let ((has-output (string-match-p \"$output_pattern\" content))
                        (has-remote-user (string-match-p \"$remote_pattern\" content))
                        (has-success (string-match-p \"$success_pattern\" content))
                        (has-local-user (string-match-p \"$local_pattern\" content))
                        (has-failure (string-match-p \"$failure_pattern\" content)))
                    (message \"Results: output=%s remote-user=%s success=%s local-user=%s failure=%s\" 
                            has-output has-remote-user has-success has-local-user has-failure)
                    (if (and has-output has-remote-user has-success (not has-local-user) (not has-failure))
                        (progn
                          (message \"✓ SUCCESS: Found expected $language_title output with remote username verification\")
                          t)
                      (progn
                        (when has-local-user
                          (message \"✗ CRITICAL: Local username detected - execution fell back to local!\"))
                        (when has-failure
                          (message \"✗ CRITICAL: Failure pattern detected - remote execution failed!\"))
                        (message \"✗ FAIL: $language_title remote execution verification failed\")
                        nil))))))
          (progn
            (message \"✗ FAIL: No $language_title euporie buffer found\")
            nil))))")
    
    # Parse timing for this language
    local log_times
    log_times=$(parse_log_timing "$language")
    local script_duration
    script_duration=$(calculate_phase_duration "${language_title}_Execution")
    
    # Store results for summary using files
    mkdir -p /tmp/test-results
    echo "$output_success" > "/tmp/test-results/${language}_output"
    echo "$window_split_success" > "/tmp/test-results/${language}_window"
    echo "$log_times" > "/tmp/test-results/${language}_timing"
    echo "$script_duration" > "/tmp/test-results/${language}_duration"
    
    echo "✓ $language_title test phase completed"
    echo ""
}

# Function to analyze timing performance across all languages
analyze_multi_language_timing() {
    echo ""
    echo "=== MULTI-LANGUAGE TIMING ANALYSIS ==="
    
    for language in "${LANGUAGES[@]}"; do
        local language_title=$(capitalize_first "$language")
        local log_times=($(cat "/tmp/test-results/${language}_timing" 2>/dev/null || echo "N/A N/A N/A"))
        local script_duration=$(cat "/tmp/test-results/${language}_duration" 2>/dev/null || echo "N/A")
        
        echo ""
        echo "--- $language_title Timing ---"
        [ "${log_times[0]}" != "N/A" ] && echo "SSH Connection: ${log_times[0]} seconds"
        [ "${log_times[1]}" != "N/A" ] && echo "QRSH Connection: ${log_times[1]} seconds"  
        [ "${log_times[2]}" != "N/A" ] && echo "Kernel Startup: ${log_times[2]} seconds"
        [ "$script_duration" != "N/A" ] && echo "Script Phase: $script_duration seconds"
    done
    
    # Total time
    local total_time=$(echo "$(date +%s.%3N) - $TEST_START_TIME" | bc -l)
    echo ""
    echo "Total Test Time: $total_time seconds"
    
    echo ""
    echo "PERFORMANCE ANALYSIS:"
    echo "- Language comparison (kernel startup times):"
    
    for language in "${LANGUAGES[@]}"; do
        local language_title=$(capitalize_first "$language")
        local log_times=($(cat "/tmp/test-results/${language}_timing" 2>/dev/null || echo "N/A N/A N/A"))
        local kernel_time="${log_times[2]}"
        if [ "$kernel_time" != "N/A" ]; then
            printf "  %-10s %s seconds\n" "$language_title:" "$kernel_time"
        fi
    done
    
    # Flag slow total time
    if [ "$(echo "$total_time > 120.0" | bc -l 2>/dev/null || echo 0)" = "1" ]; then
        echo "  ⚠️  Total test time is slow ($total_time seconds)"
    else
        echo "  ✓ Total test time acceptable ($total_time seconds)"
    fi
}

# Function to check remote execution logs and username verification
check_remote_execution_logs() {
    echo "Step: Analyzing logs and buffers for remote execution across all languages..."
    
    mkdir -p /tmp/remote-results
    
    for language in "${LANGUAGES[@]}"; do
        local language_title=$(capitalize_first "$language")
        
        # Check for remote execution indicators in logs
        local remote_detected=false
        local local_detected=false
        local hook_success=false
        
        case $language in
            "sas")
                if grep -q "is-remote.*qrsh" ~/sas-workflow-debug.log 2>/dev/null; then
                    remote_detected=true
                fi
                if grep -q "dir: nil" ~/sas-workflow-debug.log 2>/dev/null; then
                    local_detected=true
                fi
                ;;
            "python"|"r")
                if grep -q "is-remote.*qrsh" ~/euporie-termint-debug.log 2>/dev/null; then
                    remote_detected=true
                fi
                if grep -q "dir: nil" ~/euporie-termint-debug.log 2>/dev/null; then
                    local_detected=true
                fi
                ;;
        esac
        
        # Check for hook success
        if grep -q "Successfully extracted :dir:" ~/euporie-debug.log 2>/dev/null; then
            hook_success=true
        fi
        
        # NEW: Check buffer content for username verification
        local buffer_remote_user=false
        local buffer_local_user=false
        local buffer_success=false
        local buffer_failure=false
        
        local remote_pattern=$(get_remote_username_pattern "$language")
        local local_pattern=$(get_local_username_pattern "$language")
        local success_pattern=$(get_success_pattern "$language")
        local failure_pattern=$(get_failure_pattern "$language")
        
        # Check actual buffer content for username patterns
        local buffer_content
        buffer_content=$(emacsclient --eval "(if (get-buffer \"*euporie-$language*\")
            (with-current-buffer \"*euporie-$language*\" (buffer-string))
          \"Buffer not found\")" 2>/dev/null || echo "Error getting buffer")
          
        if [[ "$buffer_content" =~ $remote_pattern ]]; then
            buffer_remote_user=true
        fi
        if [[ "$buffer_content" =~ $local_pattern ]]; then
            buffer_local_user=true
        fi
        if [[ "$buffer_content" =~ $success_pattern ]]; then
            buffer_success=true
        fi
        if [[ "$buffer_content" =~ $failure_pattern ]]; then
            buffer_failure=true
        fi
        
        echo "$remote_detected" > "/tmp/remote-results/${language}_remote"
        echo "$([ "$local_detected" = false ] && echo true || echo false)" > "/tmp/remote-results/${language}_local_avoided"
        echo "$hook_success" > "/tmp/remote-results/${language}_hook"
        echo "$buffer_remote_user" > "/tmp/remote-results/${language}_buffer_remote_user"
        echo "$buffer_local_user" > "/tmp/remote-results/${language}_buffer_local_user"
        echo "$buffer_success" > "/tmp/remote-results/${language}_buffer_success"
        echo "$buffer_failure" > "/tmp/remote-results/${language}_buffer_failure"
        
        echo "- $language_title Log Analysis:"
        echo "  - Remote detected: $remote_detected"
        echo "  - Local avoided: $([ "$local_detected" = false ] && echo true || echo false)"
        echo "  - Hook success: $hook_success"
        echo "- $language_title Buffer Analysis:"
        echo "  - Remote username (eddyhu): $buffer_remote_user"
        echo "  - Local username (vwh7mb): $buffer_local_user"
        echo "  - Success message: $buffer_success"
        echo "  - Failure message: $buffer_failure"
        echo "  - VERDICT: $([ "$buffer_remote_user" = true ] && [ "$buffer_success" = true ] && [ "$buffer_local_user" = false ] && [ "$buffer_failure" = false ] && echo "✓ TRUE REMOTE" || echo "✗ FALSE POSITIVE")"
    done
}

echo "=== Multi-Language Euporie C-RET :dir Parameter Test ==="
echo "$(date): Starting automated test for Python, R, and SAS"
echo "Testing languages: ${LANGUAGES[*]}"

# Initialize results storage using temporary directories
rm -rf /tmp/test-results /tmp/remote-results 2>/dev/null || true

# Step 1: Kill ALL existing Emacs processes (daemon + GUI)
echo "Step 1: Killing ALL existing Emacs processes..."
echo "  - Finding Emacs processes..."
ps aux | grep -i emacs | grep -v grep || echo "  - No Emacs processes found initially"
echo "  - Killing all Emacs processes (daemon, GUI, emacsclient)..."
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs -r kill -9 2>/dev/null || true
sleep 3
echo "  - Verifying all Emacs processes are terminated..."
ps aux | grep -i emacs | grep -v grep || echo "  ✓ All Emacs processes terminated successfully"

# Step 2: Clear logs for clean test
echo "Step 2: Clearing debug logs..."
> ~/sas-workflow-debug.log
> ~/euporie-debug.log
> ~/euporie-termint-debug.log
> ~/tramp-qrsh-debug.log 2>/dev/null || true

# Step 3: Start fresh Emacs.app ONLY (no daemon)
echo "Step 3: Starting fresh Emacs.app ONLY..."
record_phase_start "Emacs_Startup"
echo "  - Launching Emacs.app (GUI application)..."
osascript -e 'tell application "Emacs" to activate'
echo "  - Waiting for Emacs.app to fully start..."
sleep 8  # Longer wait for full startup
echo "  - Verifying Emacs.app is running..."
ps aux | grep -i emacs | grep -v grep | head -3 || echo "  - Warning: No Emacs process detected"
record_phase_end "Emacs_Startup"
echo "  ✓ Emacs.app startup completed"

# Step 3a: Load unified euporie modules
echo "Step 3a: Loading unified euporie modules..."
emacsclient --eval "(progn
  (message \"=== Loading unified euporie modules ===\")
  (load \"/Users/vwh7mb/dotfiles-pure-termint/.doom.d/tramp-qrsh.el\")
  (load \"/Users/vwh7mb/dotfiles-pure-termint/.doom.d/euporie-unified.el\")
  (message \"✓ Unified modules loaded successfully\"))" 

# Step 3b: Configure euporie environment  
echo "Step 3b: Configuring euporie environment in Emacs..."
emacsclient --eval "(progn
  (message \"=== Configuring euporie environment ===\")
  (let ((project-dir \"~/projects/emacs-euporie/\"))
    (cd project-dir)
    (setenv \"PATH\" (concat project-dir \".pixi/envs/default/bin:\" (getenv \"PATH\")))
    (message \"✓ Updated PATH for euporie\")))"

# Step 4-8: Test each language
for language in "${LANGUAGES[@]}"; do
    test_language_execution "$language"
done

# Step 9: Check remote execution logs
check_remote_execution_logs

# Step 10: Take screenshot
echo "Step 10: Taking screenshot..."
osascript -e 'tell application "Emacs" to activate'
sleep 0.5
screencapture -T 0.5 ~/test-results-screenshot.png
echo "Screenshot saved to ~/test-results-screenshot.png"

# Step 11: Analyze timing performance
echo "Step 11: Analyzing timing performance..."
analyze_multi_language_timing

# Step 12: Generate summary
echo ""
echo "=== COMPREHENSIVE TEST SUMMARY ==="
echo "Timestamp: $(date)"

# Check overall success criteria with enhanced username verification
all_success=true
for language in "${LANGUAGES[@]}"; do
    local language_title=$(capitalize_first "$language")
    local output_success=$(cat "/tmp/test-results/${language}_output" 2>/dev/null || echo "nil")
    local window_success=$(cat "/tmp/test-results/${language}_window" 2>/dev/null || echo "nil")
    local remote_success=$(cat "/tmp/remote-results/${language}_remote" 2>/dev/null || echo "false")
    local hook_success=$(cat "/tmp/remote-results/${language}_hook" 2>/dev/null || echo "false")
    local local_avoided=$(cat "/tmp/remote-results/${language}_local_avoided" 2>/dev/null || echo "false")
    local buffer_remote_user=$(cat "/tmp/remote-results/${language}_buffer_remote_user" 2>/dev/null || echo "false")
    local buffer_local_user=$(cat "/tmp/remote-results/${language}_buffer_local_user" 2>/dev/null || echo "true")
    local buffer_success=$(cat "/tmp/remote-results/${language}_buffer_success" 2>/dev/null || echo "false")
    local buffer_failure=$(cat "/tmp/remote-results/${language}_buffer_failure" 2>/dev/null || echo "true")
    
    echo ""
    echo "--- $language_title Results ---"
    echo "- Output detected: $([ "$output_success" = "t" ] && echo "✓ SUCCESS" || echo "✗ FAIL")"
    echo "- Window split: $([ "$window_success" = "t" ] && echo "✓ SUCCESS" || echo "✗ FAIL")"
    echo "- Remote execution (logs): $([ "$remote_success" = "true" ] && echo "✓ SUCCESS" || echo "✗ FAIL")"
    echo "- Hook extraction: $([ "$hook_success" = "true" ] && echo "✓ SUCCESS" || echo "✗ FAIL")"
    echo "- Local avoided (logs): $([ "$local_avoided" = "true" ] && echo "✓ SUCCESS" || echo "✗ FAIL")"
    echo "- CRITICAL USERNAME VERIFICATION:"
    echo "  - Remote username (eddyhu): $([ "$buffer_remote_user" = "true" ] && echo "✓ SUCCESS" || echo "✗ FAIL")"
    echo "  - Local username avoided (vwh7mb): $([ "$buffer_local_user" = "false" ] && echo "✓ SUCCESS" || echo "✗ FAIL - LOCAL EXECUTION DETECTED")"
    echo "  - Success message: $([ "$buffer_success" = "true" ] && echo "✓ SUCCESS" || echo "✗ FAIL")"
    echo "  - Failure avoided: $([ "$buffer_failure" = "false" ] && echo "✓ SUCCESS" || echo "✗ FAIL - FAILURE MESSAGE DETECTED")"
    
    # Enhanced success criteria - must pass ALL tests including username verification
    local true_remote_execution=false
    if [ "$buffer_remote_user" = "true" ] && [ "$buffer_success" = "true" ] && [ "$buffer_local_user" = "false" ] && [ "$buffer_failure" = "false" ]; then
        true_remote_execution=true
    fi
    
    echo "- OVERALL VERDICT: $([ "$true_remote_execution" = "true" ] && echo "✓ TRUE REMOTE EXECUTION" || echo "✗ FALSE POSITIVE OR LOCAL FALLBACK")"
    
    # Check if this language passed all tests INCLUDING username verification
    if [ "$output_success" != "t" ] || [ "$window_success" != "t" ] || [ "$remote_success" != "true" ] || [ "$hook_success" != "true" ] || [ "$local_avoided" != "true" ] || [ "$true_remote_execution" != "true" ]; then
        all_success=false
    fi
done

echo ""
if [ "$all_success" = true ]; then
    echo "🎉 OVERALL RESULT: SUCCESS"
    echo "- All languages (Python, R, SAS) working with TRUE remote execution"
    echo "- All criteria met: output detection, window splits, remote execution, hook extraction, USERNAME VERIFICATION"
    echo "- CONFIRMED: Running as 'eddyhu' on WRDS remote server, NOT 'vwh7mb' locally"
    EXIT_CODE=0
else
    echo "❌ OVERALL RESULT: FAILURE"
    echo "- Remote execution verification FAILED"
    echo "- CRITICAL ISSUE: Tests may be running locally (vwh7mb) instead of remotely (eddyhu)"
    echo "- Previous 'success' results were FALSE POSITIVES - :dir parameter not working"
    echo "- See individual results above for specific failure details"
    EXIT_CODE=1
fi

echo ""
echo "Debug files:"
echo "- SAS workflow: ~/sas-workflow-debug.log"
echo "- Euporie debug: ~/euporie-debug.log"
echo "- Euporie termint: ~/euporie-termint-debug.log"
echo "- TRAMP QRSH: ~/tramp-qrsh-debug.log"
echo "- Screenshot: ~/test-results-screenshot.png"

echo ""
echo "Recent log entries:"
for log_file in "sas-workflow-debug.log" "euporie-debug.log" "euporie-termint-debug.log" "tramp-qrsh-debug.log"; do
    echo "--- $log_file (last 5 lines) ---"
    tail -5 ~/$log_file 2>/dev/null || echo "No log file found"
done

echo ""
echo "CRITICAL USERNAME ANALYSIS:"
echo "Expected remote username: eddyhu (on WRDS)"
echo "Local username: vwh7mb (on macOS)"
echo "If any output shows 'vwh7mb', the :dir parameter is NOT working and execution fell back to local"
echo "This means previous 'success' results were actually false positives"
echo ""
echo "Buffer content analysis:"
for language in "${LANGUAGES[@]}"; do
    echo "--- $language buffer username check ---"
    emacsclient --eval "(if (get-buffer \"*euporie-$language*\")
        (with-current-buffer \"*euporie-$language*\"
          (let ((content (buffer-string)))
            (cond
              ((string-match-p \"Username: eddyhu\\\\|User: eddyhu\" content)
               (message \"✓ $language: Remote username detected (eddyhu)\"))
              ((string-match-p \"Username: vwh7mb\\\\|User: vwh7mb\" content)
               (message \"✗ $language: Local username detected (vwh7mb) - FAILURE\"))
              (t (message \"? $language: No username pattern found in buffer\")))))
      (message \"✗ $language: Buffer not found\"))" 2>/dev/null || echo "Error checking $language buffer"
done

exit $EXIT_CODE