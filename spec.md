# Unified Euporie Integration Architecture

## Core Insight

The working solution is **layering euporie on top of tramp-qrsh's proven termint foundation** rather than reinventing connection management.

## Architecture Overview

### Foundation: tramp-qrsh Custom Buffer Function
- ✅ **Already solves**: termint + SSH + qrsh (the hardest part)
- ✅ **Proper polling**: Waits for SSH connection, then qrsh connection
- ✅ **Dynamic termint-define**: Creates functions at runtime with session IDs
- ✅ **Clean separation**: Connection establishment vs command execution

### Euporie Layer: Add Console Management
- Parse org-babel headers to determine execution context
- Use conditional connection logic (local/SSH/SSH+qrsh)
- Send `euporie console --kernel-name=LANG` to established buffer
- Wait for euporie prompt readiness

## Unified Function Design

```elisp
(defun euporie-unified-execute (language body params)
  "Unified euporie execution for local, SSH, and SSH+qrsh contexts."
  (let* ((dir (cdr (assoc :dir params)))
         (buffer-name (format "*euporie-%s*" language))
         (kernel-name (euporie-language-to-kernel language))
         (execution-context (euporie-parse-context dir)))
    
    ;; Step 1: Create appropriate termint buffer
    (pcase execution-context
      ('local 
       (euporie-create-local-buffer buffer-name kernel-name))
      ('ssh 
       (euporie-create-ssh-buffer buffer-name kernel-name dir))
      ('ssh-qrsh 
       (euporie-create-qrsh-buffer buffer-name kernel-name dir)))
    
    ;; Step 2: Send code to established euporie console
    (euporie-send-code buffer-name body)
    
    "")) ; Return empty for org-babel
```

## Execution Contexts

### 1. Local Execution
**Trigger**: No `:dir` parameter or local path
**Implementation**: 
- Direct `termint-define` with euporie command
- No SSH/qrsh complexity

### 2. SSH Execution  
**Trigger**: `:dir` with SSH path (e.g., `/ssh:user@host:/path`)
**Implementation**:
- Use tramp-qrsh foundation but skip qrsh step
- SSH connection + euporie console startup

### 3. SSH+qrsh Execution
**Trigger**: `:dir` with multi-hop TRAMP path (e.g., `/sshx:wrds|qrsh::/path`)
**Implementation**:
- Full tramp-qrsh workflow: SSH → qrsh → euporie console
- Leverages existing proven polling and connection logic

## Key Functions

### `euporie-parse-context (dir)`
- Parse org-babel `:dir` parameter
- Return: 'local, 'ssh, or 'ssh-qrsh
- Handle nil dir (default to local)

### `euporie-create-local-buffer (buffer-name kernel-name)`
```elisp
(let ((session-id (euporie-buffer-to-session buffer-name)))
  (eval `(termint-define ,session-id 
                        ,(format "euporie console --kernel-name=%s" kernel-name)
                        :bracketed-paste-p t
                        :backend 'eat))
  (funcall (intern (format "termint-%s-start" session-id))))
```

### `euporie-create-qrsh-buffer (buffer-name kernel-name dir)`
```elisp
(let ((qrsh-buffer (tramp-qrsh-with-custom-buffer "interactive.q" buffer-name)))
  (when qrsh-buffer
    ;; Send euporie command to established qrsh session
    (let ((session-id (euporie-buffer-to-session buffer-name)))
      (funcall (intern (format "termint-%s-send-string" session-id))
               (format "euporie console --kernel-name=%s" kernel-name)))
    ;; Poll for euporie prompt readiness
    (euporie-wait-for-prompt qrsh-buffer)
    qrsh-buffer))
```

### `euporie-send-code (buffer-name body)`
- Use termint-generated send function for the session
- Proper bracketed paste support for multi-line code
- No custom process communication

## Benefits of This Architecture

### 1. **Builds on Proven Foundation**
- tramp-qrsh already solved termint+SSH+qrsh
- No reinventing connection management
- Leverages working polling and detection logic

### 2. **Clean Separation of Concerns**
- **Connection**: Handled by tramp-qrsh (SSH) or termint-define (local)  
- **Console Management**: Euporie layer sends startup commands
- **Code Execution**: Standard termint send functions

### 3. **Unified Interface**
- Single function handles all execution contexts
- Conditional logic based on org-babel parameters
- Same user experience across local/remote

### 4. **Maintains termint Patterns**
- Uses termint-generated functions exclusively
- No custom process communication
- Proper bracketed paste and eat backend support

## Implementation Strategy

1. **Reuse tramp-qrsh**: Extend existing custom buffer function
2. **Add context detection**: Parse org-babel `:dir` parameters  
3. **Layer euporie startup**: Send console commands to established buffers
4. **Maintain termint usage**: Use generated functions, not custom ones
5. **Test incrementally**: Local → SSH → SSH+qrsh

## Critical Success Factors

- ✅ **Don't reinvent**: Build on working tramp-qrsh foundation
- ✅ **Layer properly**: Connection → Console → Execution
- ✅ **Use termint patterns**: Generated functions, not custom communication
- ✅ **Handle contexts**: Conditional logic for local/SSH/qrsh
- ✅ **Proper polling**: Wait for connection and console readiness