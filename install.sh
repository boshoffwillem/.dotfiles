# install packages
sudo apt install git -y
sudo apt install gcc -y
sudo apt install g++ -y
sudo apt install make -y
sudo apt install cmake -y
sudo apt install unzip -y
sudo apt install gettext -y
sudo apt install stow -y
sudo apt install delta -y
sudo apt install bat -y
sudo apt install exa -y
sudo apt install ripgrep -y
sudo apt install tree -y
sudo apt install nodejs -y
sudo apt install fzf -y
sudo apt-get install ninja-build gettext libtool libtool-bin autoconf automake cmake g++ pkg-config unzip curl doxygen -y

# Install neovim
mkdir ~/code
cd ~/code
git clone https://github.com/neovim/neovim.git --depth=1
make CMAKE_BUILD_TYPE=RelWithDebInfo
sudo make install
cd ~

# Install dotnet SDK
declare repo_version=$(if command -v lsb_release &> /dev/null; then lsb_release -r -s; else grep -oP '(?<=^VERSION_ID=).+' /etc/os-release | tr -d '"'; fi)
wget https://packages.microsoft.com/config/ubuntu/$repo_version/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb
sudo apt update
sudo apt install dotnet-sdk-7.0 -y

# Install rustup
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
