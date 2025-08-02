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

### Taking Notes

If I ask you to write up notes, write them up in markdown format and append to the file in `~/Documents/Notes/foam/journals/{YYYY-MM-DD}.md`, under the heading # Work, and start the heading level at ##.

I use the VSCode Foam extension for note-taking, so please follow the Foam conventions.

## Best Practices

1. Always implement centralized file-based logging for debugging and information purposes.
2. Use parallel subagents wherever possible. Write AGENTS#.md files to share information between agents.
3. Use `rg` instead of `grep` for file searching when possible (it's faster)
4. Use `fd` instead of `find` for locating files
5. Check `pixi.toml` before suggesting any Python/R package installations
6. System-level changes should be made through nix-darwin configuration
7. Report times in EST even if logs are in UTC.
8. Always remember to clean up after yourself, especially with temporary files or directories.
