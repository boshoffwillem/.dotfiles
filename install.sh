# install nix
sh <(curl -L https://nixos.org/nix/install) --no-daemon

# source nix
. ~/.nix-profile/etc/profile.d/nix.sh

# install packages

nix-env -iA \
  nixpkgs.starship \
  nixpkgs.git \
  nixpkgs.delta \
  nixpkgs.rustup \
  nixpkgs.gcc \
  nixpkgs.gnumake \
  nixpkgs.stow \
  nixpkgs.nodejs \
  nixpkgs.nodePackages.cspell \
  nixpkgs.dotnet-sdk \
  nixpkgs.neovim \
  nixpkgs.ispell \
  nixpkgs.tmux \
  nixpkgs.ripgrep \
  nixpkgs.exa \
  nixpkgs.bat \
  nixpkgs.powershell \
  nixpkgs.tree
  
# use starship shell prompt
echo '' | sudo tee -a ~/.profile
echo 'alias ls="exa"' | sudo tee -a ~/.profile
echo 'alias cat="bat"' | sudo tee -a ~/.profile
echo '# use starship prompt' | sudo tee -a ~/.profile
echo 'eval "$(starship init bash)"' | sudo tee -a ~/.profile

# nvim config
git clone git@github.com:boshoffwillem/nvim.git \
  ~/.config/nvim
