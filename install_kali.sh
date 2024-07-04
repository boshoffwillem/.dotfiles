#!/usr/bin/env sh

sudo apt-get update -y
sudo apt-get dist-upgrade -y
sudo apt install -y git
sudo apt install -y ripgrep
sudo apt install -y bat
sudo apt install -y curl
sudo apt install -y exa
sudo apt install -y gcc
sudo apt install -y g++
sudo apt install -y make
sudo apt install -y cmake
sudo apt install -y autoconf
sudo apt install -y fd-find
sudo apt install -y nodejs
sudo apt install -y npm
sudo apt install -y fontconfig
sudo apt install -y fzf
sudo apt install -y tmux
sudo apt install -y stow
sudo apt install -y unzip
sudo apt install -y fira-code-fonts
sudo apt install -y texinfo
sudo apt install -y libtree-sitter-dev
sudo apt install terraform -y
sudo apt install luarocks -y
sudo apt install golang -y
sudo apt install net-tools -y
sudo apt install zig -y
sudo apt install hyperfine -y
sudo apt install shfmt -y
python3 -m pip install --upgrade pip setuptools wheel

wget https://packages.microsoft.com/config/debian/12/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb
sudo apt-get update -y
sudo apt-get install -y dotnet-sdk-8.0
sudo apt-get install -y azure-cli
dotnet tool install -g dotnet-grpc
dotnet tool install -g PowerShell
dotnet tool install --global csharpier
dotnet new install Avalonia.Templates
dotnet new install SpecFlow.Templates.DotNet

# linters and formatters
sudo npm install -g @bufbuild/buf
sudo npm install -g prettier

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
# ============================================================================
sudo apt-get install -y neovim
# =============================================================================

# install emacs
# =============================================================================
# sudo apt install build-essential libgtk-3-dev libgnutls28-dev libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev texinfo
# git clone git://git.sv.gnu.org/emacs.git -b emacs-29 --depth=1 ~/code/emacs
# ~/code/emacs/autogen.sh
# ~/code/emacs/configure --with-native-compilation=aot --with-json --with-tree-sitter --with-pgtk --with-mailutils
# make -j8
# src/emacs -Q
# sudo make install
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

# ZSH
# =============================================================================
sudo apt-get install -y zsh
chsh -s $(which zsh)
# =============================================================================

sudo apt-get install -y metasploit-framework
sudo apt install -y kali-win-kex
