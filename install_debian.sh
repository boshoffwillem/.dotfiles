#!/usr/bin/env sh

sudo apt update -y
sudo apt upgrade -y

sudo apt install -y \
  git \
  stow \
  ripgrep \
  tmux \
  fd-find \
  fzf \
  zsh \
  wget \
  unzip \
  htop \
  elixir \
  erlang \
  make \
  cmake \
  libtool \
  curl \
  xclip \
  libtree-sitter-dev \
  golang-go \
  python3 \
  python3-pip \
  build-essential python3-dev python3-venv

sudo apt-get install libtiff-dev libtiff5-dev libglib2.0-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev libtiff-dev -y
sudo apt-get install texinfo imagemagick -y
sudo apt-get -y install libtree-sitter-dev libgccjit-14-dev -y
sudo apt install sqlite3 -y
sudo apt install libgtk-3-dev libwebkit2gtk-4.0-dev libwebkit2gtk-4.1-dev -y
sudo apt install libc6-dev libjpeg62-dev libpng-dev xaw3dg-dev zlib1g-dev libice-dev libsm-dev libx11-dev libxext-dev -y
sudo apt install libxi-dev libxmu-dev libxmuu-dev libxrandr-dev libxt-dev libxtst-dev libxv-dev -y
sudo apt install libattr1-dev -y

sudo apt install libxml2 librsvg2-dev libgnutls28-dev libsqlite3-dev -y 

sudo apt install texlive-latex-base texlive-latex-recommended texlive-latex-recommended-doc texlive-science texlive-science-doc -y
sudo apt install texlive-fonts-recommended texlive-fonts-recommended-doc texlive-luatex texlive-xetex -y

sudo apt-get install -y clang cmake ninja-build pkg-config liblzma-dev
sudo apt install -y libstdc++-12-dev libstdc++-13-dev libstdc++-14-dev
sudo apt install -y libsecret-1-dev llvm lld

curl -L https://aka.ms/InstallAzureCli | bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/mkasberg/ghostty-ubuntu/HEAD/install.sh)"
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.4/install.sh | bash
curl -fsSL https://bun.sh/install | bash

# Rust
# =============================================================================
# Install rustup
echo "Installing Rust..."
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# =============================================================================

# use starship shell prompt
# =============================================================================
echo "Installing Starship..."
curl -sS https://starship.rs/install.sh | sh
stow starship
# =============================================================================

# use starship shell prompt
# =============================================================================
curl -sS https://starship.rs/install.sh | sh
chsh -s $(which zsh)
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
sudo lchsh $USER
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
# =============================================================================

if [ -d "~/.dotnet" ]; then
    echo "Installing dotnet..."
    wget https://dot.net/v1/dotnet-install.sh -O ~/dotnet-install.sh
else
    echo "Skipping dotnet installation."
fi

mkdir ~/code
mkdir ~/code/work

# emacs
# =============================================================================
# mkdir software
# cd software
# git clone git://git.sv.gnu.org/emacs -b emacs-30 --depth 1
# ./autogen.sh
# ./configure \
#     --with-pgtk \
#     --with-native-compilation=aot \
#     --with-xml2 \
#     --with-gnutls \
#     --with-tree-sitter \
#     --with-rsvg \
#     --with-sqlite3 \
#     CC=gcc-14
# make -j $(nproc)
# sudo make install
# =============================================================================
