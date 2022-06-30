# install nix
sh <(curl -L https://nixos.org/nix/install) --no-daemon

# source nix
. ~/.nix-profile/etc/profile.d/nix.sh

# install packages

nix-env -iA \
	nixpkgs.zsh \
	nixpkgs.nushell \
	nixpkgs.starship \
	nixpkgs.git \
	nixpkgs.delta \
	nixpkgs.rustup \
	nixpkgs.cargo \
	nixpkgs.gcc \
	nixpkgs.nodejs \
    nixpkgs.nodePackages.cspell \
    nixpkgs.dotnet-sdk \
	nixpkgs.neovim \
	nixpkgs.emacs \
	nixpkgs.ispell \
	nixpkgs.tmux \
	nixpkgs.ripgrep \
	nixpkgs.exa \
	nixpkgs.bat

# add nushell to login shells
command -v nu | sudo tee -a /etc/shells

# add zsh to login shells
command -v zsh | sudo tee -a /etc/shells

# set nushell as default shell
#sudo chsh -s $(which nu) $USER

# set zsh as default shell
sudo chsh -s $(which zsh) $USER

# use starship shell prompt
echo '' | sudo tee -a ~/.zshrc
echo '# use starship prompt' | sudo tee -a ~/.zshrc
echo 'eval "$(starship init zsh)"' | sudo tee -a ~/.zshrc

# install packer for nvim
git clone --depth 1 https://github.com/wbthomason/packer.nvim\
 ~/.local/share/nvim/site/pack/packer/start/packer.nvim

# install doom emacs
rm ~/.emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
rm -rf ~/.doom.d
git clone https://github.com/boshoffwillem/.doom.d.git ~/.doom.d
~/.emacs.d/bin/doom sync

# some extra packges for null-ls in nvim
# spell checker
sudo npm install -g cspell

# markdown linter
sudo npm install -g markdownlint

# rust setup
rustup install stable
rustup default stable

# git setup
# set up delta for better git diffs
touch ~/.gitignore
echo "
    [user]\
    \n  name = Willem Boshoff\
    \n  email = boshoffwillem@protonmail.com\
    \n[core]\
    \n  pager = delta\
    \n  editor = nvim\
    \n\
    \n[interactive]\
    \n  diffFilter = delta --color-only\
    \n\
    \n[delta]\
    \n  navigate = true # use n and N to move between diff scenarios\
    \n  light = false # true if light background\
    \n\
    \n[merge]\
    \n  conflictstyle = diff3\
    \n\
    \n[diff]\
    \n  colorMoved = default" | sudo tee -a ~/.gitignore

