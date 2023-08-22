#!/usr/bin/env sh

sudo zypper install -y git
sudo zypper install -y ripgrep
sudo zypper install -y bat
sudo zypper install -y exa
sudo zypper install -y gcc
sudo zypper install -y gcc-c++
sudo zypper install -y make
sudo zypper install -y cmake
sudo zypper install -y fd-find
sudo zypper install -y starship
sudo zypper install -y nodejs
sudo zypper install -y npm
sudo zypper install -y fontconfig
sudo zypper install -y fzf
sudo zypper install -y tmux
sudo zypper install -y neovim
sudo zypper install -y stow
sudo zypper install -y unzip
sudo zypper install -y openssl

# install dotnet
sudo zypper install -y libicu
sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
wget https://packages.microsoft.com/config/opensuse/15/prod.repo
sudo mv prod.repo /etc/zypp/repos.d/microsoft-prod.repo
sudo chown root:root /etc/zypp/repos.d/microsoft-prod.repo
sudo zypper install -y dotnet-sdk-7.0
dotnet tool install -g dotnet-grpc

# use starship shell prompt
curl -sS https://starship.rs/install.sh | sh
echo 'alias ls="exa"' | sudo tee -a ~/.bashrc
echo 'alias cat="bat"' | sudo tee -a ~/.bashrc
echo '# use starship prompt' | sudo tee -a ~/.bashrc
echo 'eval "$(starship init bash)"' | sudo tee -a ~/.bashrc

# Install rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source "$HOME/.cargo/env"
rustup component add rust-analyzer

# install emacs from source
mkdir ~/code
cd ~/code
git clone git://git.sv.gnu.org/emacs.git
cd emacs
sudo zypper install -y autoconf
./autogen.sh
sudo zypper install -y -t pattern devel_basis
sudo zypper install -y gtk3-devel
sudo zypper install -y libgccjit0-devel-gcc12
sudo zypper install -y libxml2-devel
sudo zypper install -y libjansson-devel
sudo zypper install -y giflib-devel
sudo zypper install -y libtiff-devel
sudo zypper install -y gnome-tweaks
sudo zypper install -y texinfo
sudo zypper install -y libotf-devel
sudo zypper install -y fira-code-fonts
sudo zypper install -y tree-sitter-devel
echo 'LIBOTF_CFLAGS="libotf-config --cflags"' | sudo tee -a ~/.bashrc
echo 'LIBOTF_LIBS="libotf-config --libs"' | sudo tee -a ~/.bashrc
./configure --with-native-compilation=aot --with-json --with-tree-sitter --with-mailutils --with-pgtk
make -j8
sudo make install

cd ~/.dotfiles
stow starship
stow alacritty
stow git
stow nvim
stow emacs
