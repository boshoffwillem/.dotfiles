
# install packages

sudo apt install git \
    gcc \
    starship \
    stow \
    delta \
    neovim \
    bat \
    exa \
    tree \
    -y
    
# use starship shell prompt
echo 'alias ls="exa"' | sudo tee -a ~/.bashrc
echo 'alias cat="bat"' | sudo tee -a ~/.bashrc
echo '# use starship prompt' | sudo tee -a ~/.bashrc
echo 'eval "$(starship init bash)"' | sudo tee -a ~/.bashrc

#nix-env -iA \
#  nixpkgs.starship \
#  nixpkgs.git \
#  nixpkgs.delta \
#  nixpkgs.rustup \
#  nixpkgs.gcc \
#  nixpkgs.gnumake \
#  nixpkgs.stow \
#  nixpkgs.nodejs \
#  nixpkgs.nodePackages.cspell \
#  nixpkgs.dotnet-sdk \
#  nixpkgs.neovim \
#  nixpkgs.ispell \
#  nixpkgs.tmux \
#  nixpkgs.ripgrep \
#  nixpkgs.exa \
#  nixpkgs.bat \
#  nixpkgs.powershell \
#  nixpkgs.tree
