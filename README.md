# My Dotfiles

This repository contains my personal dotfiles, managed with [GNU Stow](https://www.gnu.org/software/stow/).

## GNU Stow

GNU Stow is a symlink farm manager which takes distinct packages of software and/or data located in separate directories on the filesystem, and makes them appear to be installed in the same place.

This is useful for managing dotfiles because it allows you to keep your configurations in a version-controlled directory (like this one) and then symlink them into place in your home directory.

## Structure

Each directory in this repository (e.g., `bash`, `vim`, `git`) represents a "package" for Stow. Files within these directories will be symlinked to your home directory.

For example, `bash/.bashrc` in this repository will be symlinked to `~/.bashrc`.

## Installation

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/yourusername/dotfiles.git ~/dotfiles
    ```
    (Replace `yourusername` with your actual GitHub username if you plan to host this on GitHub.)

2.  **Navigate to the dotfiles directory:**
    ```bash
    cd ~/dotfiles
    ```

3.  **Use Stow to create the symlinks:**
    To install all dotfiles:
    ```bash
    stow .
    ```
    To install dotfiles for a specific package (e.g., `bash`):
    ```bash
    stow bash
    ```

## Uninstallation

To remove the symlinks for a specific package:
```bash
stow -D bash
```

To remove all symlinks managed by Stow from this directory:
```bash
stow -D .
```

## Adding New Dotfiles

1.  Create a new directory for the application (e.g., `nvim`).
2.  Move or copy the configuration files into this new directory, maintaining the structure they would have in your home directory (e.g., `nvim/.config/nvim/init.vim`).
3.  Navigate to your `dotfiles` directory.
4.  Run `stow nvim` (or the name of your new directory).

## Contributing

Feel free to fork this repository and adapt it to your own needs. If you have suggestions or improvements, please open an issue or submit a pull request.