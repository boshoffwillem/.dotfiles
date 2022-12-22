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
  nixpkgs.tree

#sudo zypper install -y starship
#sudo zypper install -y delta
#sudo zypper install -y gcc
#sudo zypper install -y gcc-c++
#sudo zypper install -y clang
#sudo zypper install -y stow
#sudo zypper install -y neovim
#sudo zypper install -y emacs
#sudo zypper install -y ripgrep
#sudo zypper install -y exa
#sudo zypper install -y bat
#sudo zypper install -y tree
#sudo zypper install -y tmux
#sudo zypper install -y nodejs
#sudo zypper install -y npm

#sudo zypper install -y libicu
#sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
#wget https://packages.microsoft.com/config/opensuse/15/prod.repo
#sudo mv prod.repo /etc/zypp/repos.d/microsoft-prod.repo
#sudo chown root:root /etc/zypp/repos.d/microsoft-prod.repo
#sudo zypper install -y dotnet-sdk-6.0

#sudo dotnet tool install --global powershell
