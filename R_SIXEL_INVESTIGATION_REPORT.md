# R Sixel Integration Investigation Report

## Executive Summary

The R sixel integration fails on first run because **the .Rprofile is not being executed during Jupyter R console startup**. The sixel configuration itself works perfectly when manually sourced.

## Investigation Findings

### Environment Analysis
- **Working Directory**: `/Users/vwh7mb/projects/wander2` ✓
- **Sixel Config File**: `.jupyter/startup/01-r-sixel-config.R` exists ✓  
- **JUPYTER_CONSOLE**: Empty string `""` (should be `"1"`) ❌
- **TERM**: `"xterm-256color"` (should be `"xterm-kitty"` for sixel) ❌

### Critical Discovery
- **.Rprofile debug messages**: Completely absent from startup
- **Manual sourcing**: Works perfectly, displays sixel graphics inline
- **Print override**: Successfully installed and functional after manual sourcing

### Evidence Screenshots
- `r-startup-investigation.png`: Shows missing .Rprofile output
- `r-first-plot-test.png`: First plot fails (no sixel display)
- `r-manual-source-test.png`: Shows successful manual sourcing with all debug messages
- `r-post-manual-plot.png`: Shows working sixel graphics after manual sourcing

## Root Cause

**Jupyter R console kernels do not load .Rprofile by default** - this is a known limitation of Jupyter console mode. The R kernel starts without executing the user's profile.

## Solutions

### Option 1: Modify Termint Command (Recommended)
Add explicit R profile loading to the termint jupyter command:

```elisp
;; In jupyter-termint.el, modify the R command:
"sh -c 'cd /Users/vwh7mb/projects/wander2 && pixi run jupyter console --kernel ir --no-confirm-exit'"
```

And add explicit profile loading to the R startup.

### Option 2: Kernel Configuration
Create a custom Jupyter R kernel that loads .Rprofile.

### Option 3: Auto-Source in First Command
Modify the termint R functions to automatically source the sixel config on first use.

## Verification Test

After implementing any fix:
1. Kill existing `*jupyter-r*` buffer
2. Start fresh R console: `(jupyter-termint-smart-r-start)`
3. Look for .Rprofile debug messages in console startup
4. Test first plot: `library(ggplot2); ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()`
5. Should see `[SIXEL] ggplot print override called` and inline sixel graphics

## Status

- **Problem**: Identified ✓
- **Root Cause**: Confirmed ✓  
- **Fix**: Ready to implement
- **Test Protocol**: Established ✓