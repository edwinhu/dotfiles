# Euporie Sixel Integration Setup

This documents the complete setup for euporie-console with automatic native graphics in split windows for both Python and R using **termint architecture with eat backend**.

## Architecture Overview

- **Process Management**: termint.el (not comint) with bracketed paste support
- **Terminal Backend**: eat (not vterm) for proper terminal emulation  
- **Graphics Display**: img2sixel for inline sixel graphics in both Python and R
- **Window Management**: Split window layout (left: source, right: euporie-console)

## Components

### 1. Automatic Sixel Configuration
**Location**: `/Users/vwh7mb/projects/wander2/.jupyter/`

- `jupyter_console_config.py` - Main config that loads startup scripts
- `startup/00-sixel-config.py` - Automatic sixel matplotlib backend setup for Python
- `startup/01-r-sixel-config.R` - Automatic sixel graphics setup for R

#### Python Configuration
This configuration automatically:
- Sets matplotlib to use 'Agg' backend for file output
- Overrides `plt.show()` to use `img2sixel` for inline terminal display
- Loads whenever Jupyter console starts from wander2 directory

#### R Configuration  
This configuration automatically:
- Sets optimal graphics parameters for sixel display
- Overrides `ggplot2::print.ggplot` to use `img2sixel` for inline terminal display
- Overrides `dev.off()` to auto-display plots with sixel
- Overrides `IRdisplay::display_png()` to use sixel instead of native display
- Supports both base R graphics and ggplot2 plots with automatic sixel display
- Loads whenever IRkernel starts from wander2 directory

### 2. Org-Src Integration

#### Python Integration
**Function**: `python-send-to-jupyter-sixel`
- **Keybindings**: C-RET and C-c C-e in org python source blocks
- **Buffer**: Creates `*euporie-python-sixel*` using termint

#### R Integration
**Function**: `r-send-to-jupyter-sixel` (from jupyter-r-sixel.el)
- **Keybindings**: C-RET and C-c C-e in org R source blocks and ESS R mode
- **Buffer**: Creates `*euporie-r-sixel*` using termint

**Behavior** (both languages):
  - Uses **termint** (not comint) for process management
  - Uses **eat** backend (not vterm) for terminal emulation
  - Uses **bracketed paste** for multi-line code blocks
  - Creates `*euporie-{lang}-sixel*` buffers with live euporie-console
  - Displays console in right split window
  - Sends code from source block
  - Maintains focus on source block  
  - Graphics display inline automatically via **img2sixel**

### 3. Technical Implementation
- **Process Management**: termint.el with bracketed paste support
- **Backend**: eat (not vterm) for proper terminal emulation
- **Environment**: `TERM=xterm-kitty COLORTERM=truecolor`
- **Command**: `pixi run euporie-console --kernel {python3|ir}` 
- **Graphics**: img2sixel for inline sixel display in both Python and R

## Usage

### Python
1. Open org file with python source block
2. Press C-RET or C-c C-e from within the source block
3. Jupyter console opens in right split automatically
4. Any matplotlib plots display inline using sixel graphics
5. Focus remains on source block for continued editing

### R
1. Open org file with R source block  
2. Press C-RET or C-c C-e from within the source block
3. R Jupyter console opens in right split automatically
4. Any R plots (base graphics or ggplot2) display inline using sixel graphics
5. Focus remains on source block for continued editing

#### R Usage Examples
```r
# Base R graphics - displays inline automatically
plot(1:10, 1:10, main="Base R Test Plot")

# ggplot2 - displays inline automatically  
library(ggplot2)
ggplot(mtcars, aes(mpg, hp)) + geom_point() + ggtitle("Cars Plot")

# Manual display via IRdisplay
png("/tmp/myplot.png")
plot(rnorm(100))  
dev.off()
IRdisplay::display_png(file="/tmp/myplot.png")
```

## Key Features

✅ **Split window behavior** - Console appears on right  
✅ **Automatic sixel graphics** - No manual configuration needed  
✅ **Focus management** - Source block stays active  
✅ **High quality plots** - Full color inline graphics  
✅ **Multi-line support** - Bracketed paste for complex code  
✅ **Multi-language support** - Works with both Python and R
✅ **IRdisplay integration** - R graphics automatically use sixel display

## Implementation Notes

- Uses existing termint infrastructure with eat backend
- Python sixel configuration loads automatically via `.jupyter/startup/00-sixel-config.py`
- R sixel configuration loads automatically via `.jupyter/startup/01-r-sixel-config.R` 
- **R Integration**: Uses REAL euporie-console creating `*euporie-console[ir]*` buffers 
- **Python Integration**: Uses dedicated `*euporie-python-sixel*` buffer with custom implementation
- Keybindings set up automatically in org-src python and R buffers
- Compatible with existing org-babel and jupyter-termint workflows
- **Important**: Do NOT use `--simple-prompt` flag as it interferes with IRdisplay functionality

## Test Results

✅ **R Implementation Tested**: Successfully tested with `/Users/vwh7mb/projects/wander2/test.org`
- `*euporie-r*` buffers work perfectly with Emacs native image display
- Both base R graphics and ggplot2 display automatically in dedicated `*R-Plot-*` buffers
- Automatic plot display via `repr.plot.png` override system
- Manual plot display via `IRdisplay::display_png()` override
- Org-src integration (C-RET and C-c C-e) uses `r-send-to-jupyter-sixel` function
- Proper window splitting: console on right, org-src buffer on left
- R configuration auto-loads via `.Rprofile` in Jupyter environments

This provides the ideal workflow: edit code on left, see results with graphics on right, all inline in terminal for both Python and R.