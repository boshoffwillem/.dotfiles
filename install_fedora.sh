#!/usr/bin/env sh

sudo dnf install -y git
sudo dnf install -y ripgrep
sudo dnf install -y bat
sudo dnf install -y exa
sudo dnf install -y gcc
sudo dnf install -y g++
sudo dnf install -y make
sudo dnf install -y cmake
sudo dnf install -y autoconf
sudo dnf install -y fd-find
sudo dnf install -y nodejs
sudo dnf install -y npm
sudo dnf install -y fontconfig
sudo dnf install -y fzf
sudo dnf install -y tmux
sudo dnf install -y stow
sudo dnf install -y unzip
sudo dnf install -y autoconf
sudo dnf install -y fira-code-fonts
sudo dnf install -y gtk3-devel
sudo dnf install -y texinfo
sudo dnf install -y libtiff-devel
sudo dnf install -y giflib-devel
sudo dnf install -y libgccjit-devel
sudo dnf install -y libxml2-devel
sudo dnf install -y libtree-sitter-devel
sudo dnf install -y libotf-devel
sudo dnf install -y gnutls-devel
sudo dnf install -y jansson-devel
sudo dnf install -y ncurses-devel
sudo dnf install -y gnome-tweaks
sudo dnf install -y wget
sudo dnf install -y dotnet-sdk-8.0
dotnet tool install --global PowerShell

# linters and formatters
sudo npm install -g @bufbuild/buf
sudo npm install -g prettier
sudo dnf install -y shfmt

# use starship shell prompt
# =============================================================================
curl -sS https://starship.rs/install.sh | sh
stow starship
# =============================================================================

# Rust
# =============================================================================
# Install rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# =============================================================================

# nvim
# =============================================================================
sudo dnf install -y neovim
stow nvim
# =============================================================================

# install emacs
# =============================================================================
# mkdir ~/code
# cd ~/code
# git clone git://git.sv.gnu.org/emacs.git -b emacs-29 --depth=1
# cd emacs
# ./autogen.sh
# ./configure --with-native-compilation=aot --with-json --with-tree-sitter --with-pgtk --with-mailutils
# make -j8
# # src/emacs -Q
# sudo make install
# cd ~/.dotfiles
#
# # doom emacs
# git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
# ~/.config/emacs/bin/doom install
# rm -rdf ~/.config/doom
# stow doom.d
# ~/.config/emacs/bin/doom sync
# =============================================================================

stow git
stow bashrc
stow ideavimrc
stow nvim
stow omnisharp
stow starship
