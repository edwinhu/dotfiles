# R Automatic Display Investigation Protocol

## Problem Statement

R euporie integration is not truly automatic - it still requires `print(p)` to display ggplot objects instead of displaying them automatically when the object is created.

**Current Behavior (BROKEN):**
```r
library(ggplot2)
p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
# Nothing displays - requires manual print(p)
```

**Expected Behavior (TARGET):**
```r
library(ggplot2)
p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
# Should auto-display immediately without print(p)
```

## Test Protocol

### Phase 1: Environment Setup and Validation

**Objective**: Verify that the R test environment is properly configured

**Test Files Created:**
- `/Users/vwh7mb/dotfiles/.doom.d/test-r-autodisplay.el` - Comprehensive test suite
- `/Users/vwh7mb/dotfiles/.doom.d/r-autodisplay-config.R` - R startup configuration
- `/Users/vwh7mb/dotfiles/.doom.d/r-graphics-debug-framework.el` - Debug framework

**Setup Commands:**
```elisp
;; Load the test framework
(load-file "~/.doom.d/test-r-autodisplay.el")
(load-file "~/.doom.d/r-graphics-debug-framework.el")

;; Start debug session
(r-graphics-debug-start-session)
(r-graphics-debug-setup-monitoring)
```

### Phase 2: Diagnostic Tests

#### Test 1: ggplot2 Automatic Display Test
```elisp
(ert-run-tests "r-autodisplay/ggplot2-automatic-display")
```

**PASS Criteria:**
- ggplot object displays immediately after assignment without print()
- Sixel graphics appear in euporie buffer
- No manual intervention required

**FAIL Criteria:**
- No graphics display after object creation
- Requires manual print() to display
- Error messages in R output

#### Test 2: Base R Automatic Display Test
```elisp
(ert-run-tests "r-autodisplay/base-r-automatic-display")
```

**PASS Criteria:**
- `plot(x, y)` displays immediately
- Graphics appear inline in buffer
- No additional commands needed

**FAIL Criteria:**
- No graphics after plot() command
- Blank or error output

#### Test 3: Multiple Plot Sequence Test
```elisp
(ert-run-tests "r-autodisplay/multiple-plots-automatic-display")
```

**PASS Criteria:**
- Each plot displays automatically in sequence
- Multiple graphics visible in buffer
- No manual display commands required

**FAIL Criteria:**
- Only some plots display
- Requires manual intervention
- Graphics overlap or corrupt

### Phase 3: Root Cause Investigation

#### Investigation Commands:
```elisp
;; Full diagnostic pipeline
(r-graphics-debug-full-pipeline)

;; Individual investigations
(r-graphics-debug-investigate-irkernel)
(r-graphics-debug-test-display-methods)
```

**Key Investigation Areas:**

1. **IRkernel Configuration**
   - Is IRdisplay properly loaded?
   - Are plot mimetypes configured correctly?
   - Is Jupyter environment detected?

2. **Print Method Overrides**
   - Is print.ggplot properly overridden?
   - Are automatic display hooks installed?
   - Do base R plot methods work?

3. **Graphics Pipeline**
   - Are sixel graphics properly supported?
   - Is the euporie graphics protocol working?
   - Are temporary files created and displayed?

### Phase 4: Solution Implementation

Based on diagnostic results, implement fixes in these areas:

#### Option 1: R Startup Configuration Fix
**File**: `r-autodisplay-config.R`
**Issues to fix:**
- IRdisplay not properly initialized
- Missing plot method overrides
- Incorrect environment detection

#### Option 2: Emacs Integration Fix  
**File**: `euporie-termint.el`
**Issues to fix:**
- Missing R-specific startup script loading
- Incorrect environment variables
- Graphics protocol mismatch

#### Option 3: IRkernel Configuration Fix
**Issues to fix:**
- IRdisplay package not installed
- Kernel configuration missing
- Plot output format issues

## Clear Success Criteria

### MUST PASS Tests

1. **ggplot2 Automatic Display**
   ```r
   library(ggplot2)
   p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
   # Graphics must display here without print(p)
   ```

2. **Base R Automatic Display**
   ```r
   plot(1:10, 1:10)
   # Graphics must display immediately
   ```

3. **Assignment-based Display**
   ```r
   library(ggplot2)
   # This assignment should trigger automatic display:
   my_plot <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point()
   ```

### Performance Criteria

- Graphics display within 2 seconds of object creation
- No manual print() commands required
- No error messages in R output
- Sixel graphics properly rendered in euporie buffer

### Regression Tests

Ensure these still work after implementing automatic display:

1. **Manual print() still works**
   ```r
   p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
   print(p)  # Should still display
   ```

2. **C-RET keybinding integration**
   - Create ggplot in org-src buffer
   - Press C-RET on assignment line
   - Graphics should display automatically

## Implementation Guidance for Developer Agent

### Priority 1: Fix R Startup Configuration

1. **Load r-autodisplay-config.R on R startup**
   ```elisp
   ;; Add to euporie-r-start function:
   (let ((config-file "~/.doom.d/r-autodisplay-config.R"))
     (when (file-exists-p config-file)
       (termint-euporie-r-send-string (format "source('%s')" config-file))))
   ```

2. **Verify IRdisplay installation in pixi environment**
   ```bash
   cd /Users/vwh7mb/projects/emacs-euporie
   pixi add r-irdisplay
   pixi install
   ```

### Priority 2: Test Print Method Overrides

1. **Verify print.ggplot override works**
   ```r
   # In R session, check:
   exists("print.ggplot", mode = "function")
   body(print.ggplot)
   ```

2. **Test IRdisplay functionality**
   ```r
   # Test direct IRdisplay:
   IRdisplay::display_png("path/to/test.png")
   ```

### Priority 3: Debug Graphics Pipeline

1. **Check environment variables in R**
   ```r
   Sys.getenv("TERM")        # Should be "xterm-kitty"
   Sys.getenv("COLORTERM")   # Should be "truecolor"  
   ```

2. **Verify euporie graphics protocol**
   ```bash
   # Check euporie startup command includes graphics:
   pixi run euporie-console --graphics=sixel --kernel-name=ir
   ```

## Expected Outcomes

After implementing fixes:

1. **All ERT tests pass**
   - `r-autodisplay/ggplot2-automatic-display` ✓
   - `r-autodisplay/base-r-automatic-display` ✓  
   - `r-autodisplay/multiple-plots-automatic-display` ✓

2. **User workflow is seamless**
   - Open R org-src buffer
   - Type: `p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()`
   - Press C-RET
   - Graphics display automatically in euporie buffer

3. **No regression issues**
   - Manual print() still works
   - Existing keybindings functional
   - Other languages (Python, Stata) unaffected

## Debugging Resources

- **Test log**: `~/r-autodisplay-test.log`
- **Debug log**: `~/r-graphics-debug.log`
- **R config**: `~/.doom.d/r-autodisplay-config.R`

Run `(r-graphics-debug-full-pipeline)` for comprehensive diagnostics.