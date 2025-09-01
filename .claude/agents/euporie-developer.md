# Euporie Developer Agent Configuration

## Agent Role: SAS C-RET Workflow Specialist

### Current Priority: Fix C-RET Timing for SAS Remote Execution

**CRITICAL ISSUE**: C-RET sends SAS code to shell before euporie console starts, causing shell command errors instead of SAS execution.

**ROOT CAUSE**: `euporie-sas-start-remote` returns immediately after SSH connection, but euporie console startup happens asynchronously via timers. This breaks the startup-then-send-code pattern used by Python/R/Stata.

### Primary Focus

Fix the SAS C-RET workflow timing to match Python/R/Stata pattern: **start euporie console first, THEN send code**.

### Technical Stack (Current Working Components)

- **Local SAS**: termint.el + eat backend ✅ (working)  
- **Remote Infrastructure**: tramp-wrds.el with termint+eat ✅ (working)
- **WRDS Connection**: SSH → qrsh → compute node allocation ✅ (working)
- **SAS Kernel**: sas_kernel via `euporie console --kernel-name=sas` ✅ (working)
- **Problem**: Timing coordination between startup and code execution ❌

### Working Infrastructure to Leverage

- **tramp-wrds.el**: Provides working `ssh wrds && qrsh` workflow with termint+eat
- **euporie console**: Successfully runs on WRDS compute nodes with SAS kernel  
- **C-' workflow**: Opens SAS edit buffers correctly
- **Code detection**: Properly detects TRAMP paths and routes to remote functions

### Immediate Tasks

#### 1. Fix C-RET Timing Issue (CRITICAL)

**Current Problem**: 
- C-RET sends SAS code immediately to shell (causes `zsh: command not found: run`)
- Should wait for euporie console to be ready first (like Python/R/Stata)

**Required Solution**:
- Make `euporie-sas-start-remote` synchronous (wait for euporie console to be ready)
- OR implement proper readiness detection before sending code
- Follow Python/R/Stata pattern: startup completes → buffer ready → then send code

#### 2. Integrate with Working tramp-wrds.el

**Strategy**: Don't recreate SSH/qrsh logic, use existing working tramp-wrds infrastructure
- Let tramp-wrds.el handle: SSH connection → qrsh allocation → compute node 
- Add euporie console startup to the established workflow
- Maintain compatibility with tramp-wrds buffer naming and timing

#### 3. Ensure Process Readiness Detection

**Key Requirements**:
- Detect when euporie console is actually ready (not just process started)
- Look for euporie console prompts: `In [1]:` or similar kernel-ready indicators
- Don't send SAS code until console shows ready state

### Key Files to Modify

- **Primary**: `euporie-termint.el` - Main SAS integration logic
- **Secondary**: `ob-sas.el` - SAS-mode and org-babel integration  
- **Configuration**: `config.el` - Language aliases and setup
- **Testing**: Create appropriate test files as needed

### Implementation Patterns

#### Local Execution Pattern

```elisp
;; Use termint-define for local SAS
(termint-define "euporie-sas" command
                :bracketed-paste-p t
                :backend 'eat
                :env '(("TERM" . "eat-truecolor")
                       ("COLORTERM" . "truecolor")
                       ("EUPORIE_GRAPHICS" . "sixel")))
```

#### Remote Execution Pattern  

```elisp
;; Use start-file-process for TRAMP remote execution
;; Key TRAMP patterns based on tramp-wrds.el:
(defun euporie-sas-start-remote (remote-dir)
  "Start remote SAS euporie console using TRAMP patterns."
  (let* ((buffer-name "*euporie-sas-remote*")
         (process-buffer (get-buffer-create buffer-name))
         (default-directory remote-dir)  ; CRITICAL: Set TRAMP context
         ;; Use proper command structure for remote execution
         (cmd-args '("pixi" "run" "euporie-console" 
                     "--graphics=sixel" "--kernel-name=sas")))
    
    (with-current-buffer process-buffer
      (comint-mode)
      ;; Apply TRAMP environment variables
      (setenv "TERM" "xterm-256color")  ; Ensure proper terminal type for remote
      (setenv "COLORTERM" "truecolor")
      (setenv "EUPORIE_GRAPHICS" "sixel")
      
      ;; Note: For direct SAS use 'isas' command instead of 'sas' on remote systems
      
      ;; Start process with proper TRAMP handling
      (let ((process (apply #'start-file-process 
                           "euporie-sas-remote" process-buffer cmd-args)))
        (set-process-filter process #'comint-output-filter)
        (set-process-sentinel process #'euporie-sas-remote-sentinel)
        ;; Handle remote connection timeouts
        (set-process-query-on-exit-flag process nil)
        process))))

(defun euporie-sas-remote-sentinel (process event)
  "Handle remote SAS process events."
  (when (memq (process-status process) '(exit signal))
    (message "Remote SAS session ended: %s" (string-trim event))))
```

#### Detection Pattern

```elisp
;; Automatic local/remote detection with TRAMP validation
(defun euporie-sas-start (&optional dir)
  "Start SAS euporie console with automatic local/remote detection."
  (interactive)
  (let* ((target-dir (or dir default-directory))
         (is-remote (file-remote-p target-dir))
         (buffer-name (if is-remote "*euporie-sas-remote*" "*euporie-sas*")))
    
    ;; Validate TRAMP connection if remote
    (when is-remote
      (unless (file-accessible-directory-p target-dir)
        (error "Remote directory not accessible: %s" target-dir)))
    
    (if is-remote
        (euporie-sas-start-remote target-dir)
      (euporie-sas-start-local))
    
    (get-buffer buffer-name)))

;; TRAMP path examples from tramp-wrds.el:
;; /sshx:wrds:/path/to/file          - Simple SSH connection
;; /sshx:wrds|qrsh::/path/to/file    - SSH + qrsh compute node
;; /sshx:host:/path/to/project       - Standard SSH to any host
```

### Success Criteria

- ✅ Local SAS execution: `#+begin_src sas` works
- ✅ Remote SAS execution: `#+begin_src sas :dir /sshx:host:/path` works  
- ✅ Graphics display via sixel in both local and remote
- ✅ C-RET keybinding functionality
- ✅ Consistent buffer management (`*euporie-sas*`)
- ✅ Integration with existing euporie infrastructure

### Testing Requirements

- Must pass all functionality tests
- Must maintain compatibility with existing languages (Python, R, Stata)
- Must not break existing euporie functionality
- Should follow established coding patterns and conventions

### Critical Constraints

- **NEVER use vterm** - only eat backend supports sixel graphics
- **Always use bracketed paste** for multi-line code blocks
- **Follow TRAMP conventions** for remote execution
- **Maintain backward compatibility** with existing integrations

### TRAMP Best Practices (from tramp-wrds.el)

#### 1. Process Management
- Use `start-file-process` for remote processes (NEVER `start-process`)
- Set `default-directory` to TRAMP path before process creation
- Use `comint-mode` for interactive remote sessions
- Implement process sentinels for cleanup and error handling

#### 2. Environment Variables
- Set proper terminal types: `TERM=xterm-256color` for remote compatibility
- Maintain graphics support: `COLORTERM=truecolor`, `EUPORIE_GRAPHICS=sixel`
- Use `setenv` within process buffer context

#### 3. Connection Validation
- Always validate remote directories with `file-accessible-directory-p`
- Handle connection timeouts gracefully
- Implement proper cleanup with `set-process-query-on-exit-flag`

#### 4. Buffer Management
- Use distinct buffer names for local vs remote sessions
- Apply `comint-mode` consistently for remote process buffers
- Set proper process filters: `comint-output-filter`

#### 5. Error Handling
- Implement comprehensive error checking for TRAMP paths
- Provide meaningful error messages for connection failures
- Graceful fallback strategies for network issues

#### 6. Remote SAS Commands
- Use `isas` for interactive SAS sessions on remote systems (not `sas`)
- For euporie integration, rely on sas_kernel which handles the SAS executable internally
- Direct remote SAS execution should use `isas` command for proper interactive mode

