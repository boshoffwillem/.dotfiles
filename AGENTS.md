# Copilot Instructions

## Build, test, and lint commands

This repository is primarily dotfiles and setup scripts; it does not define a repo-wide automated build, test, or lint suite.

| Task | Command |
| --- | --- |
| Bootstrap base packages (Debian/Ubuntu) | `./install_debian.sh` |
| Bootstrap base packages (Fedora) | `./install_fedora.sh` |
| Bootstrap base packages (openSUSE) | `./install_suse.sh` |
| Install extra dev tooling | `./install_dev.sh` |
| Apply one dotfile package | `stow <package>` (example: `stow nvim`) |
| Sync Doom config after module/package edits | `doom sync` |

Single-test command: Not applicable (no automated test framework is defined in tracked repo files).

## High-level architecture

- Top-level folders are GNU Stow packages (for example `nvim/`, `emacs/`, `doom/`, `bashrc/`, `git/`). Each package mirrors the destination path in `$HOME` (`.config/...`, `.bashrc`, `.gitconfig`, etc.).
- OS bootstrap scripts (`install_debian.sh`, `install_fedora.sh`, `install_suse.sh`) install system dependencies; the openSUSE script also applies Stow packages directly.
- Neovim config lives in `nvim/.config/nvim` and is split into:
  - `init.lua` entrypoint (`require("kickstart.defaults")` + `require("config.lazy")`)
  - `lua/config/lazy.lua` for `lazy.nvim` bootstrap and plugin import
  - `lua/plugins/*.lua` for active plugin specs
- Emacs is maintained in two styles:
  - Vanilla Emacs config in `emacs/.emacs.d` (straight.el + use-package)
  - Doom Emacs config in `doom/.config/doom` (`init.el` modules, `packages.el`, `config.el`)
- Helper shell utilities are tracked in `bin/.local/bin` (for example `tmux-project.sh`, `cht.sh`).

## Key conventions

- Preserve the Stow package layout when editing files: change files inside package directories, then apply with `stow <package>`.
- For Neovim plugin work, prefer `nvim/.config/nvim/lua/plugins/*.lua`; these are what `lua/config/lazy.lua` imports.
- Keep Lua formatting at 2-space indentation (see `nvim/.config/nvim/.editorconfig`).
- Cross-editor navigation intentionally remaps movement keys to `j/k/l/;` and uses Space-led command workflows; keep that mapping consistent when modifying keybindings.
- For package management in Emacs:
  - Vanilla profile: add/update `use-package` blocks in `emacs/.emacs.d/init.el` (with `:straight t` where appropriate)
  - Doom profile: add packages in `doom/.config/doom/packages.el` and run `doom sync`
