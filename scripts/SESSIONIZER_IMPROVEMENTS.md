# Zellij Sessionizer Improvements

## Recent Changes

### Floating Pane Sizing Fix (2025-09-07)

**Problem**: The fzf interface was not filling the entire floating pane area, leaving wasted space around the edges.

**Root Cause**: Double space constraint issue:
- Zellij floating pane: 80% width × 70% height of terminal
- fzf height setting: Limited to 60% of available space
- Result: fzf only used 60% of the already-constrained 70% height

**Solution**: Updated fzf configuration to fill entire available terminal space:

#### Changes Made:
1. **Removed height constraint**: Eliminated `--height=60%` to allow fzf to use full terminal
2. **Enhanced border styling**: Changed from `--border` to `--border=rounded` for better visual appearance
3. **Added inline info**: Added `--info=inline` to optimize space usage
4. **Optimized preview window**: Reduced preview from `right:50%` to `right:45%` to provide more space for the main selection list

#### Benefits:
- fzf now fills the entire 80% × 70% floating pane
- Better space utilization with no wasted empty areas  
- Improved visual aesthetics with rounded borders
- More space for directory/session selection list
- Maintains full preview functionality

#### Technical Details:
- When no `--height` is specified, fzf defaults to full-screen mode
- In a constrained environment (floating pane), this means filling the available space
- The floating pane dimensions are controlled by Zellij config, not fzf
- Preview window still maintains full functionality with directory listings and session info

This fix ensures the sessionizer interface properly utilizes the allocated floating pane space for optimal user experience.