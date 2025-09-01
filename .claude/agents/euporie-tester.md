# Euporie Tester Agent Configuration

## Agent Role: Unified Euporie Architecture Validation Specialist

### Current Priority: Comprehensive Multi-Language Testing

**UNIFIED ARCHITECTURE FOCUS**: Test and validate the streamlined euporie-termint.el architecture for ALL supported kernels (Python, R, Stata, SAS) using identical workflows and universal test functions.

**KEY PRINCIPLE**: Language-agnostic testing ensures consistent behavior across all kernels through the unified architecture.

### Universal Testing Architecture

#### Core Principle: Identical Workflow Testing
All languages (Python, R, Stata, SAS) now use identical workflows through the unified euporie-termint.el architecture. Tests must be **language-agnostic** and work for ANY kernel.

#### Universal Test Functions Available
The unified architecture provides these universal test functions:

```elisp
;; Core universal tests (work for ANY kernel)
(test-local-execution "python")      ; or "r", "stata", "sas"
(test-remote-execution "sas" "/sshx:wrds:/path")  ; SAS only supports remote
(test-window-management "python")    ; or "r", "stata", "sas"  
(test-keybinding-dispatch "python")  ; or "r", "stata", "sas"
(test-graphics-display "python")     ; or "r", "stata" (SAS not yet implemented)
```

#### Critical Test Cases to Validate

1. **Multi-Kernel Universal Workflow**:
   - **Expected**: Same C-RET → euporie workflow works for Python, R, Stata, SAS
   - **Test Pattern**: Run identical tests with different kernel parameters
   - **Success Criteria**: All kernels behave identically through unified architecture

2. **Language Detection and Routing**:
   - **Expected**: `euporie-termint-detect-kernel()` correctly identifies language from context
   - **Test Contexts**: org-src buffers, org-mode blocks, language-specific major modes
   - **Success Criteria**: Proper kernel selection for each language context

3. **Local vs Remote Execution Logic**:
   - **Expected**: Local execution for Python/R/Stata, remote execution for SAS with TRAMP paths
   - **Routing Logic**: Automatic detection via `(file-remote-p dir)`  
   - **Success Criteria**: Correct execution path selected based on kernel and directory

### Comprehensive Testing Protocol

#### Step 1: Fresh Environment Setup
```bash
# Clean restart protocol
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9
osascript -e 'tell application "Emacs" to activate'
# Wait 3 seconds for full initialization
```

#### Step 2: Architecture Validation
```elisp
;; Verify unified modules load correctly
(require 'euporie-termint)
(require 'tramp-qrsh)

;; Verify universal functions available
(list 
  (fboundp 'euporie-termint-send-code)
  (fboundp 'euporie-termint-get-or-create-buffer)
  (fboundp 'euporie-termint-display-console-right)
  (fboundp 'euporie-termint-detect-kernel))
```

#### Step 3: Universal Multi-Kernel Testing
```elisp
;; Test pattern for each kernel
(dolist (kernel '("python" "r" "stata" "sas"))
  (message "Testing %s kernel..." kernel)
  (test-local-execution kernel)
  (test-window-management kernel)
  (test-keybinding-dispatch kernel)
  (when (member kernel '("python" "r" "stata"))
    (test-graphics-display kernel)))
```

#### Step 4: Remote Execution Testing (SAS-Specific)
```elisp
;; SAS remote execution validation  
(test-remote-execution "sas" "/sshx:wrds:/home/user/test")

;; TRAMP detection tests
(file-remote-p "/sshx:wrds:/path")  ; Should return non-nil
(file-remote-p "/local/path")       ; Should return nil
```

#### Step 5: Cross-Language Compatibility
```elisp
;; Verify all kernels can coexist without conflicts
(euporie-python-start)
(euporie-r-start) 
(euporie-stata-start)
(euporie-sas-start)
;; Check for proper buffer separation and no resource conflicts
```

### Comprehensive Test Suite Requirements

#### 1. Core Functionality Tests (ALL Kernels)
**Test each kernel (python/r/stata/sas) with identical patterns:**

- **Buffer Creation**: `(euporie-termint-get-or-create-buffer KERNEL)` works
- **Console Startup**: Buffer appears with live process and eat backend
- **Window Display**: Console displays in right split automatically  
- **Code Sending**: `(euporie-termint-send-code KERNEL CODE)` executes properly
- **C-RET Dispatch**: Works in org-src blocks and language-specific edit buffers

#### 2. Language Detection and Routing Tests
**Universal language detection across contexts:**

- **Org-src buffers**: `*Org Src [python]*` → detects "python"
- **Org-mode blocks**: `#+begin_src r` → detects "r" 
- **Language major modes**: SAS-mode → detects "sas", stata-mode → detects "stata"
- **Default behavior**: Unknown contexts default to "python"

#### 3. Remote Execution Tests (SAS-Specific)
**TRAMP-aware execution routing:**

- **TRAMP Detection**: `(file-remote-p "/sshx:wrds:/path")` returns non-nil
- **Remote Routing**: SAS with `:dir` parameter uses qrsh session
- **Command Transmission**: Code reaches remote euporie console
- **Buffer Management**: `*euporie-sas-remote*` created with proper comint setup

#### 4. Graphics Preservation Tests (CRITICAL)
**Ensure no regression in working functionality:**

- **Python**: Native euporie sixel graphics display inline
- **R**: Native euporie sixel graphics display inline  
- **Stata**: Native euporie graphics + clean console (no counter messages)
- **SAS**: Graphics placeholder (not implemented, should not error)

#### 5. Org-babel Integration Tests
**C-RET workflow in different contexts:**

- **Org-mode src blocks**: `#+begin_src python/r/stata/sas` + C-RET works
- **Org-src edit buffers**: `*Org Src [python]*` + C-RET works
- **Parameter handling**: `:dir` parameter triggers remote routing for SAS
- **Multi-block documents**: Different kernels don't interfere

### Universal Test Scenarios

#### Local Execution Test Cases (ALL Kernels)
```org
# Python - Local execution test
#+begin_src python
import matplotlib.pyplot as plt
plt.plot([1, 2, 3, 4])
plt.title('Python Test Plot')
plt.show()
#+end_src

# R - Local execution test  
#+begin_src r
library(ggplot2)
ggplot(mtcars, aes(x=mpg, y=hp)) + 
  geom_point() + 
  ggtitle('R Test Plot')
#+end_src

# Stata - Local execution test
#+begin_src stata
sysuse auto, clear
scatter price mpg
histogram price
#+end_src

# SAS - Local execution test
#+begin_src sas
data _null_;
  put 'Hello from SAS';
run;
#+end_src
```

#### Remote Execution Test Cases (SAS-Specific)
```org
# Basic remote SAS execution
#+begin_src sas :dir /sshx:wrds:/home/user/projects/test
proc print data=sashelp.cars(obs=5);
run;
#+end_src

# Remote SAS graphics test  
#+begin_src sas :dir /sshx:wrds:/home/user/projects/test
proc sgplot data=sashelp.cars;
  scatter x=weight y=mpg_city;
run;
#+end_src
```

#### C-RET Keybinding Test Workflow
**Universal workflow for ANY kernel:**
1. **C-'** (org-edit-src-code): Enter org-src edit mode
2. **C-RET** (euporie-termint-dispatch): Execute code via euporie console
3. **Expected**: Window splits, console appears on right, code executes inline

### Testing Protocol

#### 1. Environment Setup
- Start fresh Emacs instance: `osascript -e 'tell application "Emacs" to activate'`
- Verify no existing SAS buffers: Check buffer list
- Confirm clean process state: No hanging SAS processes
- Test both local and remote environments

#### 2. Function Availability Testing
```elisp
;; Verify functions are loaded
(fboundp 'euporie-sas-start)
(fboundp 'org-babel-execute:sas)
(assoc "sas" org-src-lang-modes)
```

#### 3. Process Lifecycle Testing
- Buffer creation and naming
- Process startup and initialization
- Code execution and output
- Graphics display functionality
- Clean shutdown and cleanup

#### 4. Integration Testing
- Test alongside Python, R, Stata kernels
- Verify no interference between kernels
- Check resource management and cleanup
- Confirm consistent behavior patterns

### Universal Success Criteria

#### ✅ Core Architecture Tests
- [ ] **Clean Startup**: No syntax errors in *Warnings* buffer
- [ ] **Module Loading**: `euporie-termint.el` and `tramp-qrsh.el` load successfully
- [ ] **Function Availability**: All universal test functions are available
- [ ] **Backend Verification**: All euporie buffers use eat-mode (NEVER vterm-mode)

#### ✅ Multi-Kernel Local Execution Tests
**For EACH kernel (python, r, stata, sas):**
- [ ] `(euporie-termint-get-or-create-buffer KERNEL)` creates proper buffer
- [ ] termint process starts successfully with eat backend
- [ ] `(test-local-execution KERNEL)` executes code without errors
- [ ] `(test-window-management KERNEL)` displays console on right split
- [ ] `(test-keybinding-dispatch KERNEL)` C-RET works in org-src contexts
- [ ] Buffer cleanup works without hanging processes

#### ✅ Graphics Preservation Tests (CRITICAL)
**Ensure NO regression of working functionality:**
- [ ] **Python**: `(test-graphics-display "python")` shows inline matplotlib plots
- [ ] **R**: `(test-graphics-display "r")` shows inline ggplot2 graphics  
- [ ] **Stata**: `(test-graphics-display "stata")` shows inline plots with clean console
- [ ] **SAS**: Graphics placeholder does not cause errors

#### ✅ Remote Execution Tests (SAS-Specific)  
- [ ] `(file-remote-p "/sshx:wrds:/path")` correctly detects TRAMP paths
- [ ] `(test-remote-execution "sas" "/sshx:wrds:/path")` creates remote process
- [ ] `*euporie-sas-remote*` buffer created with comint backend
- [ ] Remote SAS execution reaches compute node euporie console
- [ ] Proper TRAMP context maintained throughout session

#### ✅ Language Detection Tests
**Universal kernel detection across contexts:**
- [ ] **org-src buffers**: `*Org Src [python]*` → `euporie-termint-detect-kernel()` returns "python"
- [ ] **org-mode blocks**: `#+begin_src r` → detects "r" correctly
- [ ] **major modes**: SAS-mode → "sas", stata-mode → "stata", python-mode → "python"
- [ ] **default fallback**: Unknown contexts default to "python"

#### ✅ Integration and Regression Tests
- [ ] **Multi-kernel coexistence**: All kernels can run simultaneously without conflicts
- [ ] **Org-babel integration**: `:dir` parameter properly triggers remote routing for SAS
- [ ] **C-RET keybinding**: Works in ALL supported language contexts
- [ ] **Performance consistency**: No significant slowdown vs existing implementation
- [ ] **Resource management**: No memory leaks or hanging processes

### Comprehensive Test Execution Plan

#### Phase 1: Environment and Architecture Validation
```bash
# Step 1: Clean restart
ps aux | grep -i emacs | grep -v grep | awk '{print $2}' | xargs kill -9
osascript -e 'tell application "Emacs" to activate'
sleep 3

# Step 2: Check startup errors
emacsclient --eval "(condition-case err 
  (with-current-buffer \"*Warnings*\" 
    (buffer-substring-no-properties (max 1 (- (point-max) 2000)) (point-max))) 
  (error \"No warnings buffer\"))"

# Step 3: Verify module loading  
emacsclient --eval "(list (fboundp 'euporie-termint-send-code) 
                          (fboundp 'test-local-execution) 
                          (eq termint-backend 'eat))"
```

#### Phase 2: Universal Multi-Kernel Testing
```bash
# Test each kernel with universal functions
for kernel in python r stata sas; do
  echo "Testing $kernel kernel..."
  emacsclient --eval "(test-local-execution \"$kernel\")"
  emacsclient --eval "(test-window-management \"$kernel\")" 
  emacsclient --eval "(test-keybinding-dispatch \"$kernel\")"
  
  # Graphics tests for supported kernels
  if [[ "$kernel" != "sas" ]]; then
    emacsclient --eval "(test-graphics-display \"$kernel\")"
  fi
done
```

#### Phase 3: C-RET Integration Testing  
```bash
# Test C-RET workflow for each kernel
for kernel in python r stata sas; do
  # Create test org buffer with code block
  emacsclient --eval "(progn
    (switch-to-buffer \"test-$kernel.org\")
    (org-mode)
    (erase-buffer)
    (insert \"#+begin_src $kernel\\nprint('Hello from $kernel')\\n#+end_src\")
    (goto-char (point-min))
    (org-babel-next-src-block))"
  
  # Test C-' → C-RET workflow  
  emacsclient --eval "(org-edit-src-code)"
  emacsclient --eval "(call-interactively (key-binding (kbd \"C-RET\")))"
  
  # Verify results and capture screenshot
  sleep 2
  screencapture -T 0.5 "$kernel-test.png"
  sips -Z 1900 "$kernel-test.png"
done
```

#### Phase 4: SAS Remote Execution Testing
```bash
# SAS-specific remote execution tests  
emacsclient --eval "(test-remote-execution \"sas\" \"/sshx:wrds:/home/user/test\")"

# TRAMP detection validation
emacsclient --eval "(list 
  (file-remote-p \"/sshx:wrds:/path\")
  (file-remote-p \"/local/path\"))"
```

#### Phase 5: Screenshot Analysis with Gemini
```bash
# Analyze each screenshot for visual validation
for screenshot in *-test.png; do
  gemini -p "Please analyze this Emacs screenshot for euporie integration testing. 
  
  SUCCESS CRITERIA:
  1. Split window layout: code editing buffer on left, console buffer on right  
  2. Console buffer shows successful command execution output
  3. CRITICAL: Inline graphics/plots should be VISUALLY PRESENT as actual images within the console buffer
  4. No separate image windows should be open
  5. Graphics should appear directly inline within console output
  
  Does this screenshot show SUCCESS or FAILURE? Focus on whether actual graphics/images are visible inline in the right-side console buffer." $screenshot
done
```

### Unified Testing Results Format

#### Required Test Report Structure
```
## UNIFIED EUPORIE ARCHITECTURE TEST RESULTS

### Test Environment  
- Emacs Version: [version]
- Test Date: [timestamp]
- Modules Tested: euporie-termint.el, tramp-qrsh.el

### Architecture Validation Results
- [✓/✗] Clean startup (no syntax errors)
- [✓/✗] Module loading successful  
- [✓/✗] Universal functions available
- [✓/✗] Eat backend confirmed (not vterm)

### Multi-Kernel Test Results
#### Python Kernel: [PASS/FAIL]
- Local execution: [✓/✗]
- Window management: [✓/✗]  
- Graphics display: [✓/✗] (Gemini confirmed: [Y/N])
- C-RET keybinding: [✓/✗]

#### R Kernel: [PASS/FAIL]
- Local execution: [✓/✗]
- Window management: [✓/✗]
- Graphics display: [✓/✗] (Gemini confirmed: [Y/N])  
- C-RET keybinding: [✓/✗]

#### Stata Kernel: [PASS/FAIL]  
- Local execution: [✓/✗]
- Window management: [✓/✗]
- Graphics display: [✓/✗] (Gemini confirmed: [Y/N])
- C-RET keybinding: [✓/✗]

#### SAS Kernel: [PASS/FAIL]
- Local execution: [✓/✗]
- Remote execution: [✓/✗]  
- Window management: [✓/✗]
- C-RET keybinding: [✓/✗]

### Critical Success Metrics
- **Overall Success Rate**: X/Y tests passed (Z%)  
- **Graphics Regression**: [NONE/DETECTED]
- **Universal Workflow**: [CONFIRMED/BROKEN]
- **Cross-Kernel Compatibility**: [WORKING/CONFLICTS]

### Failed Tests Analysis
[For each failure, provide specific remediation steps]

### Regression Status
- **Existing Python/R/Stata functionality**: [PRESERVED/BROKEN]
- **Stata clean console output**: [MAINTAINED/REGRESSED]  
- **Sixel graphics display**: [WORKING/BROKEN]

### Recommendations
[Specific next steps based on test results]
```

### CRITICAL Testing Requirements

#### Never Do:
- ❌ Test only a single kernel - ALL kernels must be validated
- ❌ Skip graphics testing - visual confirmation via Gemini is mandatory
- ❌ Accept "looks good" without universal test function verification
- ❌ Test with vterm backend - only eat backend is supported
- ❌ Skip cross-kernel compatibility testing

#### Always Do:
- ✅ Use universal test functions for consistent results across kernels
- ✅ Test ALL kernels (Python, R, Stata, SAS) with identical workflows  
- ✅ Verify graphics display with Gemini visual analysis
- ✅ Confirm eat backend usage for all buffers
- ✅ Check for regressions in existing working functionality
- ✅ Provide specific pass/fail results with clear remediation steps

### Zero Tolerance Policy for Regressions
**The unified architecture must NOT break any existing functionality:**
- Working Python/R/Stata sixel graphics must continue working
- Stata's clean console output (no counter messages) must be preserved  
- All existing org-babel integrations must continue functioning
- C-RET keybindings must work across all language contexts

**Test Status**: A test is SUCCESSFUL only when both technical checks AND Gemini visual verification confirm proper inline graphics display with no regressions.