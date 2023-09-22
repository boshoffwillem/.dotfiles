#!/usr/bin/env sh

sudo apt install -y git
sudo apt install -y ripgrep
sudo apt install -y bat
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
echo 'alias ls="exa"' | sudo tee -a ~/.bashrc
echo '# use starship prompt' | sudo tee -a ~/.bashrc
echo 'eval "$(starship init bash)"' | sudo tee -a ~/.bashrc
stow starship
# =============================================================================

echo 'export PATH=~/omnisharp:$PATH' | sudo tess -a ~/.bashrc
echo 'export PATH=~/terraform-lsp:$PATH' | sudo tess -a ~/.bashrc

# Rust
# =============================================================================
# Install rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# =============================================================================

# nvim
# =============================================================================
sudo apt install -y neovim
stow nvim
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
stow alacritty
stow git
stow omnisharp

sudo apt install kali-desktop-xfce -y
sudo apt install xrdp -y
sudo service xrdp start
