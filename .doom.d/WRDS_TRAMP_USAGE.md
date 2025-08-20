# WRDS TRAMP Interactive Sessions Usage Guide

This configuration enables interactive computing sessions on WRDS (Wharton Research Data Services) using Emacs TRAMP with sshx and qrsh.

## Main Commands

### Interactive Sessions
- `M-x tramp-wrds` - Open interactive shell on WRDS compute node (automatically selects best terminal)
- `M-x tramp-wrds-simple` - Simple shell connection (most reliable, works with any setup)
- `M-x tramp-wrds-python` - Launch Python (ipython3) on WRDS
- `M-x tramp-wrds-r` - Launch R on WRDS
- `M-x tramp-wrds-sas` - Launch SAS on WRDS
- `M-x tramp-wrds-stata` - Launch Stata on WRDS

### File Operations
- `M-x wrds-find-file` - Open a file on WRDS compute node
- `M-x wrds-dired` - Browse files on WRDS compute node

### Utilities
- `M-x tramp-wrds-status` - Show active WRDS sessions
- `M-x tramp-wrds-cleanup` - Clean up WRDS connections

## Queue Options

All functions support different WRDS queues:

- **Default**: Interactive queue (no wait, for testing)
- **High Memory**: `C-u M-x tramp-wrds` → select "highmem" (128GB RAM nodes)
- **Immediate**: `C-u M-x tramp-wrds` → select "now" (execute immediately if resources available)

## TRAMP Paths for Org-Babel

Use these paths in org-src blocks:

### Basic File Access
```
/sshx:wrds|qrsh::/path/to/file
```

### High-Memory Queue
```
/sshx:wrds|qrshmem::/path/to/file
```

### Immediate Execution
```
/sshx:wrds|qrshnow::/path/to/file
```

## Example Org-Babel Usage

```org
#+begin_src python :session /sshx:wrds|qrsh::
import pandas as pd
import numpy as np
print("Running on WRDS compute node!")
#+end_src

#+begin_src R :session /sshx:wrds|qrsh::
library(data.table)
cat("R session on WRDS\n")
#+end_src
```

## Setup Requirements

1. **SSH Configuration**: Ensure you have WRDS configured in your `~/.ssh/config`:
   ```
   Host wrds
       HostName wrds-cloud.wharton.upenn.edu
       User yourusername
       # Add any additional SSH options
   ```

2. **WRDS Account**: Valid WRDS account with qrsh access

3. **Terminal Backend**: Configure your preferred terminal in Doom:
   - vterm (recommended for best compatibility)
   - eat (alternative terminal emulator)
   - shell (basic shell mode)

## Interactive Workflow

1. **Start a session**: `M-x tramp-wrds`
2. **Wait for queue allocation** (usually 10-30 seconds)
3. **You'll be connected to a WRDS compute node**
4. **Launch your software**:
   - Python: `ipython3` (or use `M-x tramp-wrds-python`)
   - R: `R` (or use `M-x tramp-wrds-r`)
   - SAS: `isas` (or use `M-x tramp-wrds-sas`)
   - Stata: `stata` (or use `M-x tramp-wrds-stata`)

## Advanced Usage

### Multiple Sessions
You can have multiple WRDS sessions open simultaneously:
- Each gets its own buffer (e.g., "*WRDS VTerm (qrsh)*")
- Different queues create separate sessions
- Use `M-x tramp-wrds-status` to see active sessions

### Custom Keybindings
Uncomment and customize in `tramp-wrds.el`:
```elisp
(global-set-key (kbd "C-c w w") #'tramp-wrds)
(global-set-key (kbd "C-c w p") #'tramp-wrds-python)
(global-set-key (kbd "C-c w r") #'tramp-wrds-r)
(global-set-key (kbd "C-c w s") #'tramp-wrds-sas)
```

## Session Limits

- **Time Limit**: 12 hours maximum on WRDS
- **Auto-reconnect**: Use `M-x tramp-wrds` to start new session
- **Queue Wait**: Allow 10-60 seconds for resource allocation

## Troubleshooting

### Connection Issues
1. Test basic sshx connection: `C-x C-f /sshx:wrds:/`
2. Check SSH config: `ssh wrds` should work from terminal
3. Enable verbose logging: uncomment `(setq tramp-verbose 6)` in tramp-wrds.el

### Queue Problems
- **No resources**: Try different queue or wait
- **Long wait**: Use "now" queue for immediate execution (if available)
- **Session timeout**: Sessions expire after 12 hours

### Terminal Issues
- **vterm not working**: Install vterm or use `M-x tramp-wrds-shell`
- **eat not working**: Use vterm or shell as alternative
- **Display problems**: Try different terminal backend

### Debug Commands
```elisp
;; Check methods are loaded
(assoc "qrsh" tramp-methods)

;; Clean up connections
M-x tramp-wrds-cleanup

;; Enable debug logging
(setq tramp-verbose 6)
```

## Technical Details

### Queue Information
- `interactive.q`: Default queue for interactive work
- `highmem.q`: High-memory nodes (128GB RAM, limited slots)
- `all.q`: All compute nodes (batch jobs primarily)

### Connection Chain
1. Local Emacs → sshx → WRDS head node
2. WRDS head node → qrsh → compute node
3. Interactive session established on compute node

### File Locations
- Configuration: `~/.doom.d/tramp-wrds.el`
- Documentation: `~/.doom.d/WRDS_TRAMP_USAGE.md`
- Integration: `~/.doom.d/config.el`