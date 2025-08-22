# Jupyter Sixel Integration Setup

This documents the complete setup for Jupyter console with automatic sixel inline graphics in split windows.

## Components

### 1. Automatic Sixel Configuration
**Location**: `/Users/vwh7mb/projects/wander2/.jupyter/`

- `jupyter_console_config.py` - Main config that loads startup scripts
- `startup/00-sixel-config.py` - Automatic sixel matplotlib backend setup

This configuration automatically:
- Sets matplotlib to use 'Agg' backend for file output
- Overrides `plt.show()` to use `img2sixel` for inline terminal display
- Loads whenever Jupyter console starts from wander2 directory

### 2. Org-Src Integration
**Function**: `python-send-to-jupyter-sixel`

- **Keybindings**: C-RET and C-c C-e in org python source blocks
- **Behavior**: 
  - Starts `*jupyter-python-sixel*` with eat backend
  - Displays console in right split window
  - Sends code from source block
  - Maintains focus on source block
  - Graphics display inline automatically via sixel

### 3. Terminal Configuration
- **Backend**: eat (not vterm)
- **Environment**: `TERM=xterm-kitty COLORTERM=truecolor`
- **Bracketed paste**: Enabled for multi-line code blocks

## Usage

1. Open org file with python source block
2. Press C-RET or C-c C-e from within the source block
3. Jupyter console opens in right split automatically
4. Any matplotlib plots display inline using sixel graphics
5. Focus remains on source block for continued editing

## Key Features

✅ **Split window behavior** - Console appears on right  
✅ **Automatic sixel graphics** - No manual configuration needed  
✅ **Focus management** - Source block stays active  
✅ **High quality plots** - Full color inline graphics  
✅ **Multi-line support** - Bracketed paste for complex code  

## Implementation Notes

- Uses existing termint infrastructure with eat backend
- Sixel configuration loads automatically via `.jupyter/jupyter_console_config.py`
- Keybindings set up automatically in org-src python buffers
- Compatible with existing org-babel and jupyter-termint workflows

This provides the ideal workflow: edit code on left, see results with graphics on right, all inline in terminal.