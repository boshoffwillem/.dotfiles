
# install packages

sudo apt install git -y
sudo apt install gcc -y
sudo apt install make -y
sudo apt install stow -y
sudo apt install delta -y
sudo apt install neovim -y
sudo apt install bat -y
sudo apt install exa -y
sudo apt install ripgrep -y
sudo apt install tree -y
sudo apt install nodejs -y
sudo apt install fzf -y

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    
# use starship shell prompt
curl -sS https://starship.rs/install.sh | sh
echo 'alias ls="exa"' | sudo tee -a ~/.bashrc
echo 'alias cat="bat"' | sudo tee -a ~/.bashrc
echo '# use starship prompt' | sudo tee -a ~/.bashrc
echo 'eval "$(starship init bash)"' | sudo tee -a ~/.bashrc

stow starship
stow nvim
stow git

#nix-env -iA \
#  nixpkgs.starship \
#  nixpkgs.rustup \
#  nixpkgs.gnumake \
#  nixpkgs.nodejs \
#  nixpkgs.nodePackages.cspell \
#  nixpkgs.dotnet-sdk \
#  nixpkgs.ispell \
#  nixpkgs.tmux \
