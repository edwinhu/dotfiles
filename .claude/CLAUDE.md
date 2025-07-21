# Project Context

## Available Command Line Utilities

The following additional command line tools are available for use:

- `rg` (ripgrep) - Fast recursive text search
- `gh` - GitHub CLI
- `xan` - CSV analysis tool
- `fd` - Fast file finder
- `gh copilot` - GitHub Copilot CLI for sub-agent tasks (Usage: `gh copilot [explain|suggest] "{PROMPT}"`)
- `gemini` - Gemini CLI for sub-agent tasks (Usage: `gemini -p "{PROMPT}"`)
- `wezterm` - Terminal emulator with advanced features

## Package Management

### Data Projects (Python/R)

This project uses **pixi** for package management. Always check `pixi.toml` for available packages before writing code that imports Python/R packages.

To check for missing packages:

1. First examine `pixi.toml` to see what's already available
2. If a package is missing, suggest adding it to `pixi.toml` rather than using pip/conda directly

### System Configuration

System packages are managed via **nix-darwin**. My nix configuration is found in `~/nix/`

To rebuild the system configuration after changes:

```bash
nix run .#build-switch
```

## Best Practices

1. Always implement centralized file-based logging for debugging and information purposes.
2. Use `rg` instead of `grep` for file searching when possible (it's faster)
3. Use `fd` instead of `find` for locating files
4. Check `pixi.toml` before suggesting any Python/R package installations
5. System-level changes should be made through nix-darwin configuration
6. Report times in EST even if logs are in UTC.
7. Always remember to clean up after yourself, especially with temporary files or directories.
