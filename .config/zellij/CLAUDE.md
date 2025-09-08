# CLAUDE.md - Zellij Configuration Guide

This guide provides comprehensive instructions for configuring Zellij effectively, including agent orchestration, testing protocols, and best practices learned from practical implementation experience.

## Agent Orchestration Protocol for Zellij Development

### Main Claude Session (Opus) - Strategic Planning
**PRIMARY ROLE**: Orchestrate Zellij configuration tasks and coordinate sub-agents
**AVOID**: Direct implementation work - delegate to specialized agents

### Sub-Agent Roles

#### 1. **zellij-researcher Agent** (Web Research & Documentation)
- **Tools**: WebSearch, WebFetch, GitHub CLI (gh)
- **Responsibilities**:
  - Research Zellij features, plugins, and configuration options
  - Find community solutions and best practices
  - Investigate GitHub issues and feature requests
  - Compare Zellij with other terminal multiplexers

**Example Delegation**:
```
Task(subagent_type="general-purpose", description="Research Zellij session management", 
     prompt="Research proper session switching methods in Zellij. Search for community solutions, GitHub issues about session management, and working sessionizer implementations. Focus on finding methods that actually switch sessions rather than just opening interfaces.")
```

#### 2. **zellij-developer Agent** (Configuration & Scripting)
- **Tools**: Read, Edit, Write, Bash
- **Responsibilities**:
  - Implement Zellij configurations and keybindings
  - Create and modify sessionizer scripts
  - Develop custom plugins or integrations
  - Fix configuration syntax and validation issues

#### 3. **zellij-tester Agent** (Testing & Validation)
- **Tools**: Bash, Screenshots, Layout inspection
- **Responsibilities**:
  - Test Zellij configurations and functionality
  - Validate keybinding behavior
  - Take screenshots for visual verification
  - Run comprehensive test suites

## Centralized File-Based Logging Protocol

### Log File Structure
```
~/.cache/zellij-logs/
├── zellij-config-debug.log      # Configuration changes and validation
├── zellij-sessionizer-debug.log # Sessionizer script execution
├── zellij-testing.log           # Test results and validation
└── zellij-agent-coordination.log # Agent task coordination
```

### Logging Implementation
All Zellij-related scripts and configurations should implement centralized logging:

```bash
# Standard logging function for Zellij scripts
ZELLIJ_LOG_DIR="$HOME/.cache/zellij-logs"
mkdir -p "$ZELLIJ_LOG_DIR"

zellij_log() {
    local level="$1"
    local message="$2"
    local logfile="${3:-zellij-config-debug.log}"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" >> "$ZELLIJ_LOG_DIR/$logfile"
}

# Usage examples
zellij_log "INFO" "Starting sessionizer with directory: $selected_dir" "zellij-sessionizer-debug.log"
zellij_log "ERROR" "Session switching failed: $error_message" "zellij-sessionizer-debug.log"
```

## Zellij Testing Protocol

### 1. Configuration Validation
```bash
# Always validate configuration after changes
zellij setup --check
```

### 2. Plugin Permission Approval
**CRITICAL**: Plugin permissions must be granted every time plugin configurations are modified.

#### Method 1: Direct Permission Grant (Recommended)
When you see the permission dialog at the bottom:
```
This plugin asks permission to: ReadApplicationState, ChangeApplicationState, RunCommand. Allow? (y/n)
```

**Steps:**
1. Press `Ctrl+p` to enter pane mode
2. Press `j` to move to the status bar pane containing the permission dialog
3. Press `Enter` to focus the permission dialog
4. Press `y` to grant permissions

#### Method 2: Alternative Navigation (if Method 1 fails)
1. Press `Esc` to close any tip popups
2. Navigate to the permission dialog pane
3. Press `y` to grant permissions

#### Method 3: AppleScript Automation (Most Reliable)
For automated testing and permission granting:
```bash
# Grant permissions using AppleScript (works reliably for automation)
osascript -e '
tell application "WezTerm" to activate
delay 1
tell application "System Events"
    key code 35 using control down -- Ctrl+p
    delay 0.5
    key code 38 -- j
    delay 0.5
    key code 36 -- Enter
    delay 0.5
    key code 16 -- y
end tell'

# Combined test with permission grant and screenshot
sleep 3 && osascript -e '
tell application "WezTerm" to activate
delay 1
tell application "System Events"
    key code 35 using control down -- Ctrl+p
    delay 0.5
    key code 38 -- j
    delay 0.5
    key code 36 -- Enter
    delay 0.5
    key code 16 -- y
end tell' && sleep 2 && screencapture -T 0.5 ~/zjstatus-test.png
```

**AppleScript Key Codes:**
- `35` = Ctrl+p (pane mode)
- `38` = j (navigate down) 
- `36` = Enter (focus dialog)
- `16` = y (grant permissions)

#### Method 4: Manual Terminal Approach
If automation fails, manually:
1. Open fresh terminal session
2. Run `zellij --session test`
3. When permission dialog appears, press `y`
4. zjstatus should begin working immediately

#### Common Issues & Troubleshooting

**Plugin Permission Issues:**
- **Tips popup blocking permissions**: Press `Esc` first to clear tips
- **Permission dialog not responding**: Use AppleScript automation method (most reliable)
- **WezTerm CLI send-text fails**: AppleScript System Events works when CLI commands don't

**Configuration Issues:**
- **Config regeneration**: Zellij may regenerate config files, check that `default_layout` points to correct zjstatus layout
- **Plugin not loading**: Switch from remote URL to local file: `plugin location="file:~/.config/zellij/plugins/zjstatus.wasm"`
- **Complex formatting errors**: Test with minimal configuration first, then add features incrementally

**Plugin Cache Issues:**
- **Clear Zellij cache**: `rm -rf ~/Library/Caches/org.Zellij-Contributors.Zellij/`
- **Plugin corruption**: Re-download plugin files to `~/.config/zellij/plugins/`
- **Permission persistence**: Permissions must be granted after each plugin configuration change

**Testing Workflow:**
1. **Start minimal**: Use simple `format_left "test"` configuration first
2. **Grant permissions**: Use AppleScript method for reliable automation  
3. **Add complexity**: Gradually add colors, modules, and advanced features
4. **Use local files**: Prefer local plugin files over remote URLs for reliability

#### Verification & Testing
**Success Indicators:**
- ✅ Custom status bar content (not default Zellij bars)
- ✅ No "ERROR IN PLUGIN" message at bottom
- ✅ No permission dialog asking for approval
- ✅ Custom text/formatting visible in status area

**Failure Indicators:**  
- ❌ "ERROR IN PLUGIN - check logs for more info" at bottom
- ❌ Default Zellij status bar still showing
- ❌ Permission dialog persisting after grant attempts

**Explicit Error Detection Test:**
```bash
# Launch test session and check for plugin errors
wezterm cli spawn -- zellij --layout simple-zjstatus-test --session error-check-test

# Grant permissions with AppleScript
sleep 3 && osascript -e '
tell application "WezTerm" to activate
delay 1
tell application "System Events"
    key code 35 using control down -- Ctrl+p
    delay 0.5
    key code 38 -- j
    delay 0.5
    key code 36 -- Enter
    delay 0.5
    key code 16 -- y
end tell'

# Take screenshot and check for error text
sleep 2 && screencapture -T 0.5 ~/plugin-error-check.png

# Visual inspection: Look for "ERROR IN PLUGIN" text at bottom of screenshot
# If error persists, plugin configuration or file has issues
```

**Debugging Failed Tests:**
If "ERROR IN PLUGIN" persists after permission grant:
1. **Check plugin file integrity**: `file ~/.config/zellij/plugins/zjstatus.wasm`
2. **Test with different plugin version**: Download fresh from GitHub releases
3. **Try minimal configuration**: Use `simple-zjstatus-test.kdl` layout
4. **Clear all caches**: Remove `~/Library/Caches/org.Zellij-Contributors.Zellij/`
5. **Check Zellij version compatibility**: Some plugin versions require specific Zellij versions

### 2. Layout Testing
```bash
#!/bin/bash
# Test script for Zellij layouts and functionality

test_zellij_layouts() {
    zellij_log "INFO" "Starting layout tests" "zellij-testing.log"
    
    # Test default layout
    if zellij setup --check > /dev/null 2>&1; then
        zellij_log "PASS" "Configuration validation passed" "zellij-testing.log"
    else
        zellij_log "FAIL" "Configuration validation failed" "zellij-testing.log"
        return 1
    fi
    
    # Test session creation
    test_session="zellij-test-$(date +%s)"
    if zellij --session "$test_session" --detached > /dev/null 2>&1; then
        zellij_log "PASS" "Session creation test passed" "zellij-testing.log"
        zellij kill-session "$test_session" 2>/dev/null
    else
        zellij_log "FAIL" "Session creation test failed" "zellij-testing.log"
    fi
}
```

### 3. Screenshot Testing Protocol
```bash
# Screenshot testing for visual verification using WezTerm CLI
take_zellij_screenshot() {
    local test_name="$1"
    local screenshot_dir="$HOME/.cache/zellij-logs/screenshots"
    mkdir -p "$screenshot_dir"
    
    # Focus WezTerm window
    osascript -e 'tell application "WezTerm" to activate'
    
    # Take screenshot with delay for focus
    sleep 1
    screencapture -T 0.5 "$screenshot_dir/zellij-${test_name}-$(date +%s).png"
    
    zellij_log "INFO" "Screenshot taken: zellij-${test_name}" "zellij-testing.log"
}

# Launch Zellij in WezTerm for testing
launch_zellij_wezterm() {
    local session_name="$1"
    local layout="${2:-}"
    
    # Start WezTerm if not running
    if ! pgrep -x "wezterm-gui" > /dev/null; then
        open -a wezterm
        sleep 2
    fi
    
    # Launch Zellij session using WezTerm CLI
    if [[ -n "$layout" ]]; then
        wezterm cli spawn -- zellij --session "$session_name" --layout "$layout"
    else
        wezterm cli spawn -- zellij --session "$session_name"
    fi
    
    zellij_log "INFO" "Launched Zellij session '$session_name' in WezTerm" "zellij-testing.log"
}

# Usage in tests
test_sessionizer_interface() {
    # Launch Zellij with sessionizer in WezTerm
    launch_zellij_wezterm "sessionizer-test"
    sleep 2
    
    # Trigger sessionizer and take screenshot
    take_zellij_screenshot "sessionizer-interface"
    
    # Send commands to test sessionizer behavior
    wezterm cli send-text -- "Ctrl+s"  # Activate sessionizer keybinding
    sleep 1
    take_zellij_screenshot "sessionizer-selection"
}
```

## Zellij Configuration Best Practices

### 1. Keybinding Strategy

#### The clear-defaults Problem
**CRITICAL**: Using `clear-defaults=true` in Zellij 0.42.2 breaks status bar hint display (GitHub Issue #4119).

**Solution**: Either:
- **Option A**: Don't use `clear-defaults=true` - extend default bindings instead
- **Option B**: If using `clear-defaults=true`, you must define ALL keybindings that should show hints

```kdl
// RECOMMENDED: Extend defaults instead of clearing them
keybinds {
    normal {
        bind "Ctrl f" { /* custom binding */ }
    }
}

// PROBLEMATIC: This breaks hint display
// keybinds clear-defaults=true { ... }
```

#### Keybinding Testing Protocol
```bash
# Test keybinding functionality
test_keybindings() {
    zellij_log "INFO" "Testing keybindings" "zellij-testing.log"
    
    # Start test session
    zellij --session keybinding-test --detached
    
    # Test each major keybinding category
    # (This requires manual verification or automation tools)
    
    take_zellij_screenshot "keybinding-normal-mode"
    # Simulate key press and test mode changes
    take_zellij_screenshot "keybinding-after-prefix"
    
    zellij kill-session keybinding-test
}
```

### 2. Session Management Architecture

#### Sessionizer Implementation Lessons
Based on extensive testing, the optimal sessionizer approach for Zellij is:

1. **For Outside Zellij**: Direct session switching works
   ```bash
   exec zellij attach --create "$session_name" --default-cwd "$directory"
   ```

2. **For Inside Zellij**: Session switching is architecturally limited
   ```bash
   # Create session in background
   (cd "$directory" && zellij attach --create-background "$session_name")
   
   # Use session manager for user-friendly switching
   zellij action launch-or-focus-plugin zellij:session-manager
   ```

#### Session Management Testing
```bash
test_session_management() {
    zellij_log "INFO" "Testing session management" "zellij-testing.log"
    
    # Test session creation
    test_dir="/tmp/zellij-test-$(date +%s)"
    mkdir -p "$test_dir"
    
    # Test sessionizer script
    if "$ZELLIJ_SESSIONIZER_SCRIPT" "$test_dir"; then
        zellij_log "PASS" "Sessionizer execution passed" "zellij-testing.log"
    else
        zellij_log "FAIL" "Sessionizer execution failed" "zellij-testing.log"
    fi
    
    # Cleanup
    rm -rf "$test_dir"
    zellij list-sessions | grep -E '^test-' | while read session; do
        zellij kill-session "$session" 2>/dev/null
    done
}
```

### 3. Plugin Configuration

#### Recommended Plugin Setup
```kdl
plugins {
    tab-bar location="zellij:tab-bar"
    status-bar location="zellij:status-bar"      // For hint display
    strider location="zellij:strider"
    compact-bar location="zellij:compact-bar"
    session-manager location="zellij:session-manager"
}
```

#### UI Configuration for Optimal Hints
```kdl
// Essential for proper hint display
default_layout "fallback"        // Uses status-bar plugin
simplified_ui false              // Enables full UI with hints
display_pane_names true          // Shows additional UI elements
pane_frames true                 // Visual feedback for panes
```

### 4. Debugging and Troubleshooting

#### Zellij Log Location and Access

**CRITICAL**: Zellij maintains comprehensive logs that are essential for debugging plugin issues and configuration problems.

**Primary Log Location:**
```bash
# Zellij logs are stored in a temporary directory with the format:
/var/folders/{hash}/T/zellij-{uid}/zellij-log/

# To find your specific log directory:
find /var/folders -name "zellij-log" 2>/dev/null | head -1

# Common location pattern (uid 503 is typical for first user):
/var/folders/01/wzs3mqmn3jx2b81f0dcq9w8h0000gq/T/zellij-503/zellij-log/
```

**Log Files Available:**
- `zellij.log` - Main application log (can be very large, 10MB+)
- `zellij-{number}.log` - Rotated log files for historical data

**Accessing Recent Log Entries:**
```bash
# Find and read the most recent Zellij log entries
ZELLIJ_LOG_DIR=$(find /var/folders -name "zellij-log" 2>/dev/null | head -1)
if [[ -n "$ZELLIJ_LOG_DIR" ]]; then
    echo "Zellij logs found at: $ZELLIJ_LOG_DIR"
    
    # Read last 100 lines of main log
    tail -100 "$ZELLIJ_LOG_DIR/zellij.log"
    
    # Search for plugin-related errors
    grep -i "plugin\|wasm\|error" "$ZELLIJ_LOG_DIR/zellij.log" | tail -20
    
    # Search for specific plugin errors (e.g., zjstatus)
    grep -i "zjstatus\|panic" "$ZELLIJ_LOG_DIR/zellij.log" | tail -10
else
    echo "Zellij logs not found. Make sure Zellij has been run recently."
fi
```

**Plugin Error Analysis:**
```bash
# Comprehensive plugin error analysis function
analyze_plugin_errors() {
    local log_dir=$(find /var/folders -name "zellij-log" 2>/dev/null | head -1)
    
    if [[ -z "$log_dir" ]]; then
        echo "ERROR: Zellij logs not found"
        return 1
    fi
    
    echo "=== Zellij Plugin Error Analysis ==="
    echo "Log directory: $log_dir"
    echo "Main log size: $(ls -lh "$log_dir/zellij.log" | awk '{print $5}')"
    echo
    
    echo "=== Recent Plugin Errors ==="
    grep -i "error\|panic\|failed" "$log_dir/zellij.log" | grep -i "plugin\|wasm" | tail -10
    echo
    
    echo "=== WebAssembly Runtime Issues ==="
    grep -i "wasm\|memory fault\|out of bounds" "$log_dir/zellij.log" | tail -5
    echo
    
    echo "=== zjstatus Specific Errors ==="
    grep -i "zjstatus\|src/bin/zjstatus" "$log_dir/zellij.log" | tail -5
    echo
    
    echo "=== Permission Related Messages ==="
    grep -i "permission\|ReadApplicationState\|ChangeApplicationState" "$log_dir/zellij.log" | tail -5
}
```

**Common Plugin Error Patterns in Logs:**
1. **WebAssembly Runtime Panics:**
   ```
   PANIC IN PLUGIN!
   Location: src/bin/zjstatus.rs, line 35, col: 1
   ```

2. **Memory Access Violations:**
   ```
   memory fault at wasm address 0x... in linear memory
   wasm trap: out of bounds memory access
   ```

3. **Thread Local Storage Issues:**
   ```
   std::thread::local::LocalKey<T>::with::h...
   ```

4. **WebAssembly Execution Failures:**
   ```
   wasm trap: wasm `unreachable` instruction executed
   ```

**Log Cleanup and Management:**
```bash
# Clear Zellij cache and logs (nuclear option for troubleshooting)
clear_zellij_cache() {
    echo "Stopping all Zellij sessions..."
    zellij kill-all-sessions 2>/dev/null
    
    echo "Clearing application cache..."
    rm -rf ~/Library/Caches/org.Zellij-Contributors.Zellij/
    
    echo "Note: Logs in /var/folders/ are managed by system and will be recreated"
    echo "To find new log location after restart, use: find /var/folders -name 'zellij-log' 2>/dev/null"
}
```

#### Configuration Debugging
```bash
debug_zellij_config() {
    zellij_log "INFO" "Starting configuration debug" "zellij-config-debug.log"
    
    # Validate configuration
    if ! zellij setup --check; then
        zellij_log "ERROR" "Configuration validation failed" "zellij-config-debug.log"
        return 1
    fi
    
    # Check for common issues
    if grep -q "clear-defaults=true" ~/.config/zellij/config.kdl; then
        zellij_log "WARNING" "clear-defaults=true detected - may break hints" "zellij-config-debug.log"
    fi
    
    # Log current configuration state
    zellij_log "INFO" "Zellij version: $(zellij --version)" "zellij-config-debug.log"
    zellij_log "INFO" "Config location: $(zellij setup --check 2>&1 | grep 'CONFIG FILE')" "zellij-config-debug.log"
    
    # Find and log Zellij system logs location
    local zellij_log_dir=$(find /var/folders -name "zellij-log" 2>/dev/null | head -1)
    if [[ -n "$zellij_log_dir" ]]; then
        zellij_log "INFO" "System logs location: $zellij_log_dir" "zellij-config-debug.log"
        
        # Check for recent plugin errors in system logs
        if grep -qi "error\|panic" "$zellij_log_dir/zellij.log" 2>/dev/null; then
            zellij_log "WARNING" "Plugin errors detected in system logs - run analyze_plugin_errors()" "zellij-config-debug.log"
        fi
    else
        zellij_log "WARNING" "Zellij system logs not found - may need to start Zellij first" "zellij-config-debug.log"
    fi
}
```

#### Visual Debugging with Screenshots
```bash
debug_visual_state() {
    local debug_session="visual-debug-$(date +%s)"
    
    # Start clean session for debugging
    zellij --session "$debug_session" --detached
    
    # Take baseline screenshot
    take_zellij_screenshot "debug-baseline"
    
    # Test mode transitions with screenshots
    # (requires automation or manual steps)
    
    # Cleanup
    zellij kill-session "$debug_session"
}
```

## Common Zellij Issues and Solutions

### Issue 1: Hints Not Displaying
**Symptom**: Status bar doesn't show keybinding hints
**Cause**: `clear-defaults=true` or improper plugin configuration
**Solution**: Remove `clear-defaults=true` or use proper plugin setup

### Issue 2: Session Switching Not Working
**Symptom**: Sessionizer opens interface but doesn't switch sessions
**Cause**: Zellij architectural limitation - no direct session switching from within session
**Solution**: Use background session creation + session manager approach

### Issue 3: Terminal Attribute Errors
**Symptom**: `could not get terminal attribute: ENODEV`
**Cause**: Running Zellij commands in non-TTY environment
**Solution**: Ensure proper terminal context or use detached operations

### Issue 4: Session Manager Missing Keybinding Hints
**Symptom**: Built-in session manager doesn't show keybinding hints like other modes
**Cause**: Architectural difference - session manager is a plugin, other modes are built into Zellij core
**Root Issue**: Plugin API has limited hint display capabilities compared to core modes
**GitHub Issues**: #4119 (hint display), #3542 (session manager crashes), Discussion #3225 (plugin limitations)
**Solutions**:
- **zjstatus plugin**: Highly customizable status bar with proper hint support
- **Alternative session managers**: zbuffers, zellij-sessionizer, zviewer
- **Workaround**: Use "non-colliding" keybinding preset
- **Future**: Wait for architectural improvements in Zellij's plugin system

## Comprehensive Test Suite

### Master Test Runner
```bash
#!/bin/bash
# Comprehensive Zellij test suite

ZELLIJ_LOG_DIR="$HOME/.cache/zellij-logs"
SCREENSHOT_DIR="$ZELLIJ_LOG_DIR/screenshots"

run_zellij_tests() {
    zellij_log "INFO" "Starting comprehensive Zellij test suite" "zellij-testing.log"
    
    # Configuration tests
    test_zellij_layouts
    debug_zellij_config
    
    # Functionality tests
    test_session_management
    test_keybindings
    
    # Visual tests
    debug_visual_state
    
    # Generate test report
    generate_test_report
}

generate_test_report() {
    local report_file="$ZELLIJ_LOG_DIR/test-report-$(date +%s).md"
    
    cat > "$report_file" << EOF
# Zellij Test Report - $(date)

## Configuration Status
$(zellij setup --check 2>&1)

## Test Results
$(grep -E '\[(PASS|FAIL)\]' "$ZELLIJ_LOG_DIR/zellij-testing.log" | tail -20)

## Screenshots
$(ls -la "$SCREENSHOT_DIR"/ | tail -10)

## Issues Found
$(grep -E '\[ERROR\]|\[WARNING\]' "$ZELLIJ_LOG_DIR"/*.log | tail -10)
EOF

    zellij_log "INFO" "Test report generated: $report_file" "zellij-testing.log"
}
```

## Agent Coordination Examples

### Research Task Example
```
Task(subagent_type="general-purpose", description="Research Zellij plugin ecosystem", 
     prompt="Research the current state of Zellij plugins, focusing on:
     1. Session management plugins with directory support
     2. Status bar customization options
     3. Community solutions for hint display issues
     4. Latest developments in Zellij plugin API
     
     Use web search to find current information and provide specific plugin recommendations with installation instructions.")
```

### Implementation Task Example  
```
Task(subagent_type="general-purpose", description="Implement improved sessionizer", 
     prompt="Implement a working Zellij sessionizer based on research findings. Requirements:
     1. Must use centralized logging to ~/.cache/zellij-logs/zellij-sessionizer-debug.log
     2. Must handle both inside and outside Zellij execution
     3. Must include comprehensive error handling
     4. Must be tested with the provided test framework
     
     Create the script at ~/.config/zellij/scripts/sessionizer.sh and ensure it integrates with the keybinding configuration.")
```

### Testing Task Example
```
Task(subagent_type="general-purpose", description="Validate Zellij configuration", 
     prompt="Run comprehensive tests on the Zellij configuration:
     1. Validate configuration syntax with zellij setup --check
     2. Test keybinding functionality with screenshots
     3. Test sessionizer script with various scenarios
     4. Generate a test report with visual evidence
     
     Use the centralized logging system and screenshot protocol defined in CLAUDE.md.")
```

## Maintenance and Updates

### Regular Maintenance Tasks
```bash
# Weekly Zellij maintenance script
maintain_zellij_config() {
    # Clean old logs (keep last 30 days)
    find "$ZELLIJ_LOG_DIR" -name "*.log" -mtime +30 -delete
    find "$SCREENSHOT_DIR" -name "*.png" -mtime +7 -delete
    
    # Run configuration validation
    debug_zellij_config
    
    # Run basic functionality tests
    run_zellij_tests
    
    # Check for Zellij updates
    zellij_log "INFO" "Current Zellij version: $(zellij --version)" "zellij-config-debug.log"
}
```

### Configuration Backup Strategy
```bash
backup_zellij_config() {
    local backup_dir="$HOME/.config/zellij/backups"
    local timestamp=$(date +%Y%m%d_%H%M%S)
    
    mkdir -p "$backup_dir"
    cp ~/.config/zellij/config.kdl "$backup_dir/config-${timestamp}.kdl"
    
    zellij_log "INFO" "Configuration backed up to: config-${timestamp}.kdl" "zellij-config-debug.log"
}
```

---

This guide should be regularly updated based on new experiences and Zellij version changes. Always test configuration changes in isolated sessions before applying to main workflow.