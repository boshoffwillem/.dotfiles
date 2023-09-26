#!/usr/bin/env sh

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
sudo apt install -y autoconf
sudo apt install -y fira-code-fonts
sudo apt install -y texinfo
sudo apt install -y libtree-sitter-dev
sudo apt install terraform -y

wget https://packages.microsoft.com/config/debian/12/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb
sudo apt-get update -y
sudo apt-get install -y dotnet-sdk-7.0
dotnet tool install -g dotnet-grpc

# use starship shell prompt
# =============================================================================
sudo apt install -y starship
curl -sS https://starship.rs/install.sh | sh
# =============================================================================

# Rust
# =============================================================================
# Install rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# =============================================================================

# nvim
# =============================================================================
sudo apt install -y neovim
# =============================================================================

# install emacs
# =============================================================================
# mkdir ~/code
# cd ~/code
#sudo apt install build-essential libgtk-3-dev libgnutls28-dev libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev texinfo
# git clone git://git.sv.gnu.org/emacs.git -b emacs-29 --depth=1
# cd emacs
# ./autogen.sh
# ./configure --with-native-compilation=aot --with-json --with-tree-sitter --with-pgtk --with-mailutils
# make -j8
# src/emacs -Q
# sudo make install
# =============================================================================

cd ~/.dotfiles
rm ~/.zshrc
rm ~/.bashrc
stow alacritty
stow bashrc
stow git
stow ideavimrc
stow nvim
stow omnisharp
stow starship

sudo apt install kali-desktop-xfce -y
sudo apt install xrdp -y
sudo service xrdp start
sudo apt install -y kali-win-kex

sudo curl -fsSLo /usr/share/keyrings/brave-browser-beta-archive-keyring.gpg https://brave-browser-apt-beta.s3.brave.com/brave-browser-beta-archive-keyring.gpg
echo "deb [signed-by=/usr/share/keyrings/brave-browser-beta-archive-keyring.gpg] https://brave-browser-apt-beta.s3.brave.com/ stable main"|sudo tee /etc/apt/sources.list.d/brave-browser-beta.list
sudo apt update -y
sudo apt install brave-browser-beta -y
