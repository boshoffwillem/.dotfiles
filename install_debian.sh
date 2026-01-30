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
  curl \
  golang-go

curl -L https://aka.ms/InstallAzureCli | bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/mkasberg/ghostty-ubuntu/HEAD/install.sh)"
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.4/install.sh | bash

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
