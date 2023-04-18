#!/usr/bin/env sh

sudo dnf install -y git
sudo dnf install -y ripgrep
sudo dnf install -y bat
sudo dnf install -y exa
sudo dnf install -y gcc
sudo dnf install -y g++
sudo dnf install -y make
sudo dnf install -y cmake
sudo dnf install -y fd-find
sudo dnf install -y starship
sudo dnf install -y nodejs
sudo dnf install -y npm
sudo dnf install -y fontconfig
sudo dnf install -y emacs
sudo dnf install -y fzf
sudo dnf install -y neovim
sudo dnf install -y stow
sudo dnf install -y unzip

sudo dnf install -y dotnet-sdk-7.0
dotnet tool install -g dotnet-grpc

git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install

# use starship shell prompt
curl -sS https://starship.rs/install.sh | sh
echo 'alias ls="exa"' | sudo tee -a ~/.bashrc
echo 'alias cat="bat"' | sudo tee -a ~/.bashrc
echo '# use starship prompt' | sudo tee -a ~/.bashrc
echo 'eval "$(starship init bash)"' | sudo tee -a ~/.bashrc

# Install rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

stow starship
stow git

rm -rdf ~/.config/doom
stow doom.d
~/.config/emacs/bin/doom sync

stow nvim
