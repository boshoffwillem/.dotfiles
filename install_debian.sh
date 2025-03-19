#!/usr/bin/env sh

sudo apt-get update -y
sudo apt install gpg -y
sudo apt-get dist-upgrade -y
sudo apt install -y git
sudo apt install -y ripgrep
sudo apt install -y bat
sudo apt install -y curl
sudo apt install -y exa
sudo apt install -y gcc
sudo apt install -y g++
sudo apt install -y make
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
sudo apt install -y fonts-firacode
sudo apt install -y texinfo
sudo apt install -y libtree-sitter-dev
sudo apt-get install -y ninja-build gettext cmake curl build-essential

sudo apt install terraform -y
wget -O- https://apt.releases.hashicorp.com/gpg | sudo gpg --dearmor -o /usr/share/keyrings/hashicorp-archive-keyring.gpg
gpg --no-default-keyring --keyring /usr/share/keyrings/hashicorp-archive-keyring.gpg --fingerprint
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/hashicorp-archive-keyring.gpg] https://apt.releases.hashicorp.com jammy main" | sudo tee /etc/apt/sources.list.d/hashicorp.list
sudo apt install terraform-ls -y

sudo apt update -y
grep ^Package: /var/lib/apt/lists/apt.releases.hashicorp.com*Packages | sort -u
sudo apt install luarocks -y
sudo apt install golang -y
sudo apt install net-tools -y
sudo apt install zig -y
sudo apt install hyperfine -y
sudo apt install -y git-delta

# linters and formatters
sudo npm install -g @bufbuild/buf
sudo npm install -g prettier
sudo apt install shfmt -y


# Docker
# =============================================================================
# Add Docker's official GPG key:
sudo apt-get install -y ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

# Add the repository to Apt sources:
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "${UBUNTU_CODENAME:-$VERSION_CODENAME}") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
sudo apt install -y gnome-terminal
wget https://desktop.docker.com/linux/main/amd64/docker-desktop-amd64.deb
sudo apt-get install ./docker-desktop-amd64.deb
sudo groupadd docker
sudo usermod -aG docker $USER
# =============================================================================

# Rust
# =============================================================================
# Install rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# =============================================================================

# use starship shell prompt
# =============================================================================
curl -sS https://starship.rs/install.sh | sh
stow starship
sudo apt install -y zsh
chsh -s $(which zsh)
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
sudo lchsh $USER
# =============================================================================

# nvim
# =============================================================================
mkdir ~/code
mkdir ~/code/work
cd ~/code
git clone https://github.com/neovim/neovim.git
cd neovim
make CMAKE_BUILD_TYPE=Release
sudo make install
cd ~
# =============================================================================

sudo apt-get install wget gpg
wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
sudo install -D -o root -g root -m 644 packages.microsoft.gpg /etc/apt/keyrings/packages.microsoft.gpg
echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/keyrings/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" |sudo tee /etc/apt/sources.list.d/vscode.list > /dev/null
rm -f packages.microsoft.gpg
sudo apt install -y apt-transport-https
sudo apt update
sudo apt install -y code # or code-insiders

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
rm ~/.zshrc
stow zshrc
