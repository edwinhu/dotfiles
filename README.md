# Dotfiles

Personal configuration files for macOS development environment.

## Overview

This repository contains my dotfiles and configuration for various development tools. The setup includes shell configurations, editor settings, terminal emulators, and system utilities.

## Structure

```
.
├── .config/              # XDG config directory
│   ├── ghostty/         # Ghostty terminal configuration
│   ├── karabiner/       # Keyboard customization
│   ├── nixpkgs/         # Nix package manager config
│   ├── nvim/            # Neovim configuration (LazyVim)
│   ├── sioyek/          # PDF reader configuration
│   └── wezterm/         # WezTerm terminal configuration
├── .doom.d/             # Doom Emacs configuration
├── .logseq/             # Logseq configuration and plugins
├── .shell_aliases       # Shell aliases
├── .shell_common        # Common shell configuration
└── .shell_env           # Environment variables
```

## Components

### Shell Configuration

- **.shell_common**: Core shell configuration loaded by both bash and zsh. Includes prompt setup, history configuration, and tool initialization (like zoxide).
- **.shell_aliases**: Command aliases and helper functions
- **.shell_env**: Environment variable definitions

### Editor Configurations

- **Neovim** (`.config/nvim/`): LazyVim-based configuration with custom settings
- **Doom Emacs** (`.doom.d/`): Emacs configuration using the Doom framework

### Terminal Emulators

- **Ghostty** (`.config/ghostty/`): Modern GPU-accelerated terminal
- **WezTerm** (`.config/wezterm/`): Cross-platform terminal with Lua configuration

### System Tools

- **Karabiner** (`.config/karabiner/`): Advanced keyboard customization for macOS
- **Sioyek** (`.config/sioyek/`): PDF reader optimized for research papers
- **Logseq** (`.logseq/`): Knowledge management and note-taking system with citation manager

## Installation

### Prerequisites

- macOS (configurations are optimized for macOS)
- Git
- [nix-darwin](https://github.com/LnL7/nix-darwin) for system package management

### Setup

1. **Clone the repository:**
   ```bash
   git clone https://github.com/yourusername/dotfiles.git ~/dotfiles
   cd ~/dotfiles
   ```

2. **Create symlinks:**
   
   For individual files:
   ```bash
   ln -sf ~/dotfiles/.shell_common ~/.shell_common
   ln -sf ~/dotfiles/.shell_aliases ~/.shell_aliases
   ln -sf ~/dotfiles/.shell_env ~/.shell_env
   ```
   
   For config directories:
   ```bash
   ln -sf ~/dotfiles/.config/* ~/.config/
   ln -sf ~/dotfiles/.doom.d ~/.doom.d
   ```

3. **Install system packages:**
   
   If using nix-darwin:
   ```bash
   cd ~/nix
   nix run .#build-switch
   ```

## Key Features

### Command Line Tools

The environment includes several modern CLI tools:

- `rg` (ripgrep) - Fast recursive text search
- `fd` - Fast file finder
- `gh` - GitHub CLI
- `zoxide` - Smarter cd command

### Terminal Integration

- Custom key bindings (Shift+Enter for newline in Ghostty)
- Claude AI integration with terminal shortcuts
- GPU-accelerated rendering support

## Shell Functions

The configuration includes various helpful aliases and functions. Some notable ones:

- Git shortcuts and aliases
- Directory navigation helpers
- Development workflow optimizations

## Dependencies

### Google Drive Integration

Some applications depend on Google Drive being synced at a specific relative location:

- **Sioyek**: The PDF reader's shared database (`.config/sioyek/shared.db`) is no longer synced via Google Drive. Annotations are managed locally.
- **Logseq Citation Manager**: References PDFs stored in `~/Library/CloudStorage/GoogleDrive-*/My Drive/Paperpile/` for paper attachments

For Logseq, the paths use relative symlinks for portability, but require Google Drive to be set up with the same folder structure.

## Notes

- Shell configurations are designed to work with both bash and zsh
- The setup assumes `pixi` for Python/R package management in data projects
- System-level changes should be made through nix-darwin configuration

## Contributing

Feel free to fork and adapt these configurations to your needs. If you have suggestions or improvements, please open an issue or submit a pull request.