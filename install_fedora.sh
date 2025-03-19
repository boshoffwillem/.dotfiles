#!/usr/bin/env sh

sudo dnf install -y git
sudo dnf -y install ninja-build cmake gcc make gettext curl glibc-gconv-extra
sudo dnf install -y htop
sudo dnf install -y ripgrep
sudo dnf install -y bat
sudo dnf install -y exa
sudo dnf install -y autoconf
sudo dnf install -y automake
sudo dnf install -y fd-find
sudo dnf install -y nodejs
sudo dnf install -y npm
sudo dnf install -y fontconfig-devel
sudo dnf install -y fzf
sudo dnf install -y tmux
sudo dnf install -y stow
sudo dnf install -y unzip
sudo dnf install -y autoconf
sudo dnf install -y fira-code-fonts
sudo dnf install -y wget
sudo dnf install -y zig
sudo dnf install -y hyperfine
sudo dnf install -y snapd
sudo dnf install -y git-delta

# Dotnet
# =============================================================================
sudo dnf install -y dotnet-sdk-9.0
# =============================================================================

# Docker
# =============================================================================
sudo dnf install -y gnome-terminal
sudo dnf -y install dnf-plugins-core
sudo dnf-3 config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
sudo dnf install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
sudo systemctl enable --now docker
wget https://desktop.docker.com/linux/main/amd64/docker-desktop-x86_64.rpm
sudo dnf install ./docker-desktop-x86_64.rpm
# =============================================================================

# Rust
# =============================================================================
# Install rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# =============================================================================

# linters and formatters
sudo npm install -g @bufbuild/buf
sudo npm install -g prettier
sudo dnf install -y shfmt

# use starship shell prompt
# =============================================================================
curl -sS https://starship.rs/install.sh | sh
stow starship
sudo dnf install -y zsh
chsh -s $(which zsh)
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
sudo lchsh $USER
# =============================================================================

# nvim
# =============================================================================
# mkdir ~/code
# mkdir ~/code/work
# cd ~/code
# git clone https://github.com/neovim/neovim.git
# cd neovim
# make CMAKE_BUILD_TYPE=Release
# sudo make install
# cd ~
# =============================================================================

sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\nautorefresh=1\ntype=rpm-md\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" | sudo tee /etc/yum.repos.d/vscode.repo > /dev/null
dnf check-update
sudo dnf install -y code # or code-insiders

sudo dnf copr enable pgdev/ghostty
sudo dnf install -y ghostty
sudo snap install insomnia

stow git
rm ~/.bashrc
stow bashrc
rm ~/.zshrc
stow zshrc
stow ideavimrc
stow nvim
stow omnisharp
stow starship
