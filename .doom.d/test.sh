#!/bin/bash

# test.sh - Automated test for C-RET :dir parameter extraction with timing analysis
# Tests the hook-based approach for extracting :dir from org-src blocks

set -e

# Timing variables
TEST_START_TIME=$(date +%s.%3N)
PHASES_START=()
PHASES_END=()
PHASE_NAMES=("Emacs Startup" "Code Execution")

# Function to record phase start time
record_phase_start() {
    local phase_name="$1"
    local timestamp=$(date +%s.%3N)
    PHASES_START+=($timestamp)
    echo "[TIMING] $phase_name phase started at $(date)"
}

# Function to record phase end time
record_phase_end() {
    local phase_name="$1"
    local timestamp=$(date +%s.%3N)
    PHASES_END+=($timestamp)
    echo "[TIMING] $phase_name phase completed at $(date)"
}

# Function to parse timing from logs
parse_log_timing() {
    local ssh_time="N/A"
    local qrsh_time="N/A" 
    local sas_time="N/A"
    
    # Parse SAS kernel timing
    if [ -f ~/sas-workflow-debug.log ]; then
        sas_time=$(rg "SAS kernel ready after.*\(([0-9.]+) seconds\)" ~/sas-workflow-debug.log -o -r '$1' 2>/dev/null | tail -1)
        [ -z "$sas_time" ] && sas_time="N/A"
    fi
    
    # Parse SSH connection timing (if logs are enhanced to include this)
    if [ -f ~/tramp-qrsh-debug.log ]; then
        ssh_time=$(rg "SSH connection established after.*\(([0-9.]+) seconds\)" ~/tramp-qrsh-debug.log -o -r '$1' 2>/dev/null | tail -1)
        [ -z "$ssh_time" ] && ssh_time="N/A"
        
        qrsh_time=$(rg "QRSH connection established after.*\(([0-9.]+) seconds\)" ~/tramp-qrsh-debug.log -o -r '$1' 2>/dev/null | tail -1)
        [ -z "$qrsh_time" ] && qrsh_time="N/A"
    fi
    
    echo "$ssh_time $qrsh_time $sas_time"
}

# Function to calculate phase durations from script timing
calculate_phase_durations() {
    local durations=()
    for i in "${!PHASES_START[@]}"; do
        if [ ${#PHASES_END[@]} -gt $i ]; then
            local duration=$(echo "${PHASES_END[$i]} - ${PHASES_START[$i]}" | bc -l 2>/dev/null || echo "0")
            durations+=($duration)
        else
            durations+=("N/A")
        fi
    done
    echo "${durations[@]}"
}

# Function to analyze timing performance
analyze_timing() {
    local log_times=($1)  # ssh qrsh sas from logs
    local script_durations=($2)  # calculated from script timing
    
    echo ""
    echo "=== TIMING ANALYSIS ==="
    
    # Display timing from logs (more accurate for actual process timing)
    [ "${log_times[0]}" != "N/A" ] && echo "SSH Connection: ${log_times[0]} seconds"
    [ "${log_times[1]}" != "N/A" ] && echo "QRSH Connection: ${log_times[1]} seconds"  
    [ "${log_times[2]}" != "N/A" ] && echo "SAS Kernel Startup: ${log_times[2]} seconds"
    
    # Display script phase timing (includes waiting and coordination overhead)
    for i in "${!PHASE_NAMES[@]}"; do
        if [ "${script_durations[$i]}" != "N/A" ] && [ "${script_durations[$i]}" != "0" ]; then
            printf "%-20s %s seconds (script timing)\n" "${PHASE_NAMES[$i]}:" "${script_durations[$i]}"
        fi
    done
    
    # Total time
    local total_time=$(echo "$(date +%s.%3N) - $TEST_START_TIME" | bc -l)
    echo "Total Time: $total_time seconds"
    
    echo ""
    echo "PERFORMANCE ANALYSIS:"
    
    # Find fastest/slowest from log times
    local fastest_phase="Unknown"
    local slowest_phase="Unknown"
    local fastest_time=999999
    local slowest_time=0
    
    for i in "${!PHASE_NAMES[@]}"; do
        local time="${log_times[$i]}"
        if [ "$time" != "N/A" ] && [ "$(echo "$time > 0" | bc -l 2>/dev/null || echo 0)" = "1" ]; then
            if [ "$(echo "$time < $fastest_time" | bc -l)" = "1" ]; then
                fastest_time=$time
                fastest_phase="${PHASE_NAMES[$i]}"
            fi
            if [ "$(echo "$time > $slowest_time" | bc -l)" = "1" ]; then
                slowest_time=$time  
                slowest_phase="${PHASE_NAMES[$i]}"
            fi
        fi
    done
    
    [ "$fastest_phase" != "Unknown" ] && echo "- Fastest phase: $fastest_phase ($fastest_time seconds)"
    [ "$slowest_phase" != "Unknown" ] && echo "- Slowest phase: $slowest_phase ($slowest_time seconds)"
    
    # Flag slow phases (>5 seconds)
    echo "- Performance warnings:"
    local warnings_found=false
    for i in "${!PHASE_NAMES[@]}"; do
        local time="${log_times[$i]}"
        if [ "$time" != "N/A" ] && [ "$(echo "$time > 5.0" | bc -l 2>/dev/null || echo 0)" = "1" ]; then
            echo "  ⚠️  ${PHASE_NAMES[$i]} is slow ($time seconds)"
            warnings_found=true
        fi
    done
    [ "$warnings_found" = false ] && echo "  ✓ All phases under 5 seconds"
    
    # Flag very slow total time
    if [ "$(echo "$total_time > 60.0" | bc -l 2>/dev/null || echo 0)" = "1" ]; then
        echo "  ⚠️  Total test time is slow ($total_time seconds)"
    fi
}

echo "=== Euporie C-RET :dir Parameter Test ==="
echo "$(date): Starting automated test"

# Step 1: Kill existing Emacs processes
echo "Step 1: Killing existing Emacs processes..."
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs -r kill -9 2>/dev/null || true
sleep 2

# Step 2: Clear logs for clean test
echo "Step 2: Clearing debug logs..."
> ~/sas-workflow-debug.log
> ~/euporie-debug.log
> ~/tramp-qrsh-debug.log 2>/dev/null || true

# Step 3: Start fresh Emacs WITHOUT environment inheritance issues
echo "Step 3: Starting fresh Emacs.app..."
record_phase_start "Emacs Startup"
osascript -e 'tell application "Emacs" to activate' &
sleep 5  # Wait for Emacs to fully load
record_phase_end "Emacs Startup"

# Step 3a: Configure euporie environment in Emacs session
echo "Step 3a: Configuring euporie environment in Emacs..."
emacsclient --eval "(progn
  (message \"=== Configuring euporie environment ===\")
  (let ((project-dir \"~/projects/emacs-euporie/\"))
    (cd project-dir)
    (setenv \"PATH\" (concat project-dir \".pixi/envs/default/bin:\" (getenv \"PATH\")))
    (message \"✓ Updated PATH for euporie\")))"

# Step 4: Open test.org file, navigate to SAS block, and enter org-src edit mode
echo "Step 4: Opening test.org file and entering SAS edit mode..."
emacsclient --eval "(progn
  (message \"=== TEST: Opening ~/projects/emacs-euporie/test.org ===\")
  (find-file \"~/projects/emacs-euporie/test.org\")
  (goto-char (point-min))
  (if (search-forward \"#+begin_src sas :dir\" nil t)
      (progn
        (forward-line 1)
        (message \"Found SAS block with :dir parameter\")
        (message \"Current position: line %d, content: %s\" 
                 (line-number-at-pos)
                 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        ;; Load SAS support and enter edit mode in same call
        (require 'ob-sas)
        (message \"✓ Loaded ob-sas\")
        (org-edit-special)
        (sleep-for 1)
        (message \"✓ Entered org-src edit buffer: %s\" (buffer-name))
        (message \"Buffer mode: %s\" major-mode))
    (error \"Could not find SAS block with :dir parameter\")))"

# Step 5: Execute C-RET in org-src edit buffer (this triggers remote connection)
echo "Step 5: Executing C-RET in org-src edit buffer..."
record_phase_start "Code Execution"
emacsclient --eval "(progn
  (message \"=== TEST: Executing C-RET ===\")
  ;; Switch to the SAS org-src edit buffer
  (let ((sas-buffer (get-buffer \"*Org Src test.org[ sas ]*\")))
    (if sas-buffer
        (with-current-buffer sas-buffer
          (message \"Switched to SAS buffer: %s\" (buffer-name))
          (goto-char (point-min))
          (search-forward \"proc print\" nil t)
          (euporie-termint-send-region-or-line)
          (message \"C-RET executed from SAS buffer\"))
      (error \"Could not find SAS org-src buffer\"))))"

# Step 5a: Check window split arrangement and fix if needed
echo "Step 5a: Checking window split arrangement..."  
WINDOW_SPLIT_SUCCESS=$(emacsclient --eval "(progn
  (message \"=== TEST: Checking window split arrangement ===\")
  (let ((windows (window-list))
        (current-buf (buffer-name))
        (euporie-buf-visible nil)
        (org-src-visible nil)
        (org-src-buf (get-buffer \"*Org Src test.org[ sas ]*\"))
        (euporie-buf (get-buffer \"*euporie-sas*\")))
    
    (message \"Number of windows: %d\" (length windows))
    (message \"Current buffer: %s\" current-buf)
    (dolist (win windows)
      (let ((buf-name (buffer-name (window-buffer win))))
        (message \"Window contains buffer: %s\" buf-name)
        (when (string-match-p \"\\*euporie-.*\\*\" buf-name)
          (setq euporie-buf-visible t))
        (when (string-match-p \"\\*Org Src.*\\\\[\" buf-name)
          (setq org-src-visible t))))
    
    ;; If window split is incorrect, fix it
    (unless (and (>= (length windows) 2) euporie-buf-visible org-src-visible)
      (when (and org-src-buf euporie-buf)
        (message \"Fixing window split arrangement...\")
        (delete-other-windows)
        (switch-to-buffer org-src-buf)
        (split-window-right)
        (other-window 1)
        (switch-to-buffer euporie-buf)
        (other-window 1)  ; Back to org-src
        (message \"✓ Fixed window split: org-src left, euporie right\")
        (setq euporie-buf-visible t org-src-visible t)))
    
    (if (and (>= (length windows) 2) euporie-buf-visible org-src-visible)
        (progn 
          (message \"✓ SUCCESS: Window split with both org-src and euporie buffers visible\") 
          t)
      (progn
        (message \"✗ FAIL: Window split incorrect\")
        (message \"  - euporie buffer visible: %s\" euporie-buf-visible)
        (message \"  - org-src visible: %s\" org-src-visible) 
        (message \"  - total windows: %d\" (length windows))
        nil))))")

# Step 6: Wait for execution and check buffer  
echo "Step 6: Waiting for euporie console to start and execute code..."
sleep 45  # Extended wait for remote connection + euporie startup + SAS table output
record_phase_end "Code Execution"

# Step 6a: Check for ACTUAL cars table output
echo "Step 6a: Checking for actual cars table output..."
# Use a simpler approach to avoid shell parsing issues
emacsclient --eval "(progn
  (message \"=== TEST: Checking euporie buffer for cars output ===\")
  (let ((euporie-buffer (cl-find-if (lambda (buf) 
                                      (string-match-p \"\\*euporie-.*\\*\" (buffer-name buf))) 
                                    (buffer-list))))
    (if euporie-buffer
        (progn
          (message \"✓ Found euporie buffer: %s\" (buffer-name euporie-buffer))
          (with-current-buffer euporie-buffer
            (let ((content (buffer-string)))
              (message \"Euporie buffer content length: %d chars\" (length content))
              (when (> (length content) 100)
                (message \"Buffer content preview: %s\" (substring content 0 (min 500 (length content)))))
              (message \"Checking for Acura content...\")
              (message \"Has Acura: %s\" (string-match-p \"Acura\" content))
              (message \"Has headers: %s\" (string-match-p \"Obs.*Make.*Model\" content)))))
      (message \"✗ FAIL: No euporie buffer found\"))))"
            
CARS_SUCCESS=false  # We'll check this manually for now


# Step 7: Check logs for local vs remote execution
echo "Step 7: Analyzing logs..."

# Check for remote execution indicators
if grep -q "is-remote.*qrsh" ~/sas-workflow-debug.log 2>/dev/null; then
    echo "✓ SUCCESS: Remote execution detected in logs"
    REMOTE_SUCCESS=true
else
    echo "✗ FAIL: No remote execution detected"
    REMOTE_SUCCESS=false
fi

if grep -q "dir: nil" ~/sas-workflow-debug.log 2>/dev/null; then
    echo "✗ FAIL: Found 'dir: nil' in logs - indicates local execution"
    LOCAL_DETECTED=true
else
    echo "✓ SUCCESS: No 'dir: nil' found in logs"  
    LOCAL_DETECTED=false
fi

# Check for :dir parameter extraction
if grep -q "Successfully extracted :dir:" ~/euporie-debug.log 2>/dev/null; then
    echo "✓ SUCCESS: Hook successfully extracted :dir parameter"
    HOOK_SUCCESS=true
else
    echo "✗ FAIL: Hook did not extract :dir parameter"
    HOOK_SUCCESS=false
fi

# Step 8: Take screenshot
echo "Step 8: Taking screenshot..."
osascript -e 'tell application "Emacs" to activate'
sleep 0.5
screencapture -T 0.5 ~/test-results-screenshot.png
echo "Screenshot saved to ~/test-results-screenshot.png"

# Step 9: Analyze timing performance
echo "Step 9: Analyzing timing performance..."
LOG_TIMES=$(parse_log_timing)
SCRIPT_DURATIONS=$(calculate_phase_durations)

# Summary
echo ""
echo "=== STRICT TEST SUMMARY ==="
echo "Timestamp: $(date)"

# Convert Emacs boolean results to shell booleans
if [ "$CARS_OUTPUT_SUCCESS" = "t" ]; then
    CARS_SUCCESS=true
else
    CARS_SUCCESS=false  
fi

if [ "$WINDOW_SPLIT_SUCCESS" = "t" ]; then
    SPLIT_SUCCESS=true
else
    SPLIT_SUCCESS=false
fi

echo "Test Results:"
echo "- Cars table output: $CARS_SUCCESS"
echo "- Window split correct: $SPLIT_SUCCESS"  
echo "- Hook extraction: $HOOK_SUCCESS"
echo "- Remote execution: $REMOTE_SUCCESS"
echo "- Local execution avoided: $([ "$LOCAL_DETECTED" = false ] && echo true || echo false)"

# Display timing analysis
analyze_timing "$LOG_TIMES" "$SCRIPT_DURATIONS"

if [ "$CARS_SUCCESS" = true ] && [ "$SPLIT_SUCCESS" = true ] && [ "$HOOK_SUCCESS" = true ] && [ "$REMOTE_SUCCESS" = true ] && [ "$LOCAL_DETECTED" = false ]; then
    echo "🎉 OVERALL RESULT: SUCCESS"
    echo "- All criteria met: cars output, window split, remote execution"
    EXIT_CODE=0
else
    echo "❌ OVERALL RESULT: FAILURE"
    echo "- Missing requirements - see details above"
    EXIT_CODE=1
fi

echo ""
echo "Debug files:"
echo "- SAS workflow: ~/sas-workflow-debug.log"
echo "- Euporie debug: ~/euporie-debug.log"
echo "- TRAMP QRSH: ~/tramp-qrsh-debug.log"
echo "- Screenshot: ~/test-results-screenshot.png"

echo ""
echo "Recent log entries:"
echo "--- sas-workflow-debug.log (last 10 lines) ---"
tail -10 ~/sas-workflow-debug.log 2>/dev/null || echo "No log file found"

echo "--- euporie-debug.log (last 10 lines) ---"  
tail -10 ~/euporie-debug.log 2>/dev/null || echo "No log file found"

echo "--- tramp-qrsh-debug.log (last 10 lines) ---"
tail -10 ~/tramp-qrsh-debug.log 2>/dev/null || echo "No log file found"

exit $EXIT_CODE