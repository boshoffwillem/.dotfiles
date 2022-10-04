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
  nixpkgs.emacs
