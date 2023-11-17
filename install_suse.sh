#!/usr/bin/env sh

sudo zypper ref -b && sudo zypper dup -y
sudo zypper update -y
sudo zypper install -y git
sudo zypper install -y ripgrep
sudo zypper install -y bat
sudo zypper install -y curl
sudo zypper install -y exa
sudo zypper install -y gcc
sudo zypper install -y gcc-c++
sudo zypper install -y make
sudo zypper install -y cmake
sudo zypper install -y autoconf
sudo zypper install -y fd
sudo zypper install -y nodejs
sudo zypper install -y npm
sudo zypper install -y fontconfig
sudo zypper install -y fzf
sudo zypper install -y tmux
sudo zypper install -y stow
sudo zypper install -y unzip
sudo zypper install -y fira-code-fonts
sudo zypper install -y texinfo
sudo zypper install -y tree-sitter-devel
sudo zypper install -y terraform
sudo zypper install -y golang

sudo zypper install -y libicu
sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
wget https://packages.microsoft.com/config/opensuse/15/prod.repo
sudo mv prod.repo /etc/zypp/repos.d/microsoft-prod.repo
sudo chown root:root /etc/zypp/repos.d/microsoft-prod.repo
sudo zypper install -y dotnet-sdk-7.0

# use starship shell prompt
# =============================================================================
sudo zypper install -y starship
# =============================================================================

# Rust
# =============================================================================
# Install rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# =============================================================================

# nvim
# ============================================================================
sudo zypper install -y neovim
# =============================================================================

# install emacs
# =============================================================================
mkdir ~/code
# sudo zypper install -y emacs-pgtk
sudo zypper install -y -t pattern devel_C_C++
sudo zypper install -y glib2-devel gtk3-devel libgccjit0-devel-gcc13 gnutls-devel mailutils
sudo zypper install -y libjansson4 libjansson-devel
# git clone git://git.sv.gnu.org/emacs.git -b emacs-29 --depth=1 ~/code/emacs
# cd ~/code/emacs
# ./autogen.sh
# ./configure --with-json --with-tree-sitter --with-native-compilation --with-pgtk --with-mailutils CFLAGS='-march=native'
# make -j8
# sudo make install -j8
# =============================================================================

cd ~/.dotfiles
rm ~/.bashrc
stow alacritty
stow bashrc
stow git
stow ideavimrc
stow kitty
stow nvim
stow omnisharp
stow starship

