#!/usr/bin/env sh

sudo apt update -y && sudo upgrade -y
sudo apt install -y git
sudo apt install -y autoconf automake bsd-mailx build-essential \
    dbus-x11 debhelper dpkg-dev emacs-bin-common emacs-common g++-10 gawk \
    gcc-10 git gvfs ibus-gtk3 language-pack-en-base libacl1-dev libasound2 \
    libasound2-dev libaspell15 libasyncns0 libatk1.0-0 libatk-bridge2.0-0 \
    libatspi2.0-0 libbrotli1 libc6 libc6 libc6-dev libc6-dev libcairo2 \
    libcairo2-dev libcairo-gobject2 libcanberra0 libcanberra-gtk3-0 \
    libcanberra-gtk3-module libdatrie1 libdb5.3 libdbus-1-3 libdbus-1-dev \
    libdrm2 libegl1 libenchant-2-dev libepoxy0 libflac8 libfontconfig1 \
    libfontconfig1-dev libfreetype6 libfreetype6-dev libgbm1 libgccjit0 \
    libgccjit-10-dev libgcc-s1 libgdk-pixbuf2.0-0 libgif7 libgif-dev \
    libgl1 libglib2.0-0 libglvnd0 libglx0 libgmp10 libgnutls28-dev \
    libgnutls30 libgpm2 libgpm2 libgpm-dev libgraphite2-3 \
    libgstreamer1.0-0 libgstreamer-gl1.0-0 libgstreamer-plugins-base1.0-0 \
    libgtk-3-0 libgtk-3-dev libgudev-1.0-0 libharfbuzz0b libharfbuzz0b \
    libharfbuzz-icu0 libhyphen0 libibus-1.0-5 libice6 libicu70 libisl23 \
    libjansson4 libjansson-dev libjbig0 libjpeg8-dev libjpeg-dev \
    libjpeg-turbo8 liblcms2-2 liblcms2-dev liblockfile1 liblockfile-dev \
    libltdl7 libm17n-0 libm17n-dev libmpc3 libmpfr6 libncurses5-dev \
    libnotify4 libnss-mdns libnss-myhostname libnss-sss libnss-systemd \
    libogg0 liborc-0.4-0 liboss4-salsa2 libotf1 libotf-dev libpango-1.0-0 \
    libpangocairo-1.0-0 libpangoft2-1.0-0 libpixman-1-0 libpng16-16 \
    libpng-dev libpulse0 librsvg2-2 librsvg2-dev libsasl2-2 libsecret-1-0 \
    libselinux1-dev libsm6 libsndfile1 libsoup2.4-1 libsqlite3-0 \
    libsqlite3-dev libssl3 libsss-nss-idmap0 libstdc++6 libsystemd-dev \
    libtdb1 libthai0 libtiff5 libtiff-dev libtinfo-dev libtree-sitter0 \
    libtree-sitter-dev libvorbis0a libvorbisenc2 libvorbisfile3 \
    libwayland-client0 libwayland-cursor0 libwayland-egl1 \
    libwayland-server0 libwebkit2gtk-4.0-dev libwebp7 libwebpdemux2 \
    libwebp-dev libwoff1 libx11-6 libx11-xcb1 libxau6 libxcb1 \
    libxcb-render0 libxcb-shm0 libxcomposite1 libxcursor1 libxdamage1 \
    libxdmcp6 libxext6 libxfixes3 libxfixes-dev libxi6 libxi-dev \
    libxinerama1 libxkbcommon0 libxml2 libxml2-dev libxpm4 libxpm-dev \
    libxrandr2 libxrender1 libxrender-dev libxslt1.1 libxt-dev libyajl2 \
    mailutils procps quilt sharutils texinfo zlib1g-dev

sudo apt install -y ripgrep
sudo apt install -y bat
sudo apt install -y exa
sudo apt install -y gcc
sudo apt install -y g++
sudo apt install -y make
sudo apt install -y cmake
sudo apt install -y fd-find
sudo apt install -y nodejs
sudo apt install -y npm
sudo apt install -y fzf
sudo apt install -y tmux
sudo apt install -y stow
sudo apt install -y unzip
sudo apt install -y fira-code-fonts
sudo apt install -y gnome-tweaks
sudo apt install -y wget
sudo apt install -y zig
sudo apt install zsh
chsh -s $(which zsh)
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
sudo apt install -y hyperfine
sudo apt install -y dotnet-sdk-8.0
dotnet tool install --global PowerShell

# linters and formatters
sudo npm install -g @bufbuild/buf
sudo npm install -g prettier
sudo apt install -y shfmt

# use starship shell prompt
# =============================================================================
curl -sS https://starship.rs/install.sh | sh
stow starship
# =============================================================================

# Rust
# =============================================================================
# Install rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# =============================================================================

# nvim
# =============================================================================
sudo apt install -y neovim
stow nvim
# =============================================================================

# install emacs
# =============================================================================
mkdir ~/code
cd ~/code
git clone git://git.sv.gnu.org/emacs.git -b emacs-29 --depth=1
cd emacs
export CC="gcc-10" CXX="gcc-10"
./autogen.sh
./configure --with-native-compilation=aot --with-json --with-tree-sitter --with-pgtk --with-mailutils
make -j$(nproc)
sudo make -j$(nproc) install
cd ~/.dotfiles
#
# # doom emacs
# git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
# ~/.config/emacs/bin/doom install
# rm -rdf ~/.config/doom
# stow doom.d
# ~/.config/emacs/bin/doom sync
# =============================================================================

stow git
stow bashrc
stow ideavimrc
stow nvim
stow omnisharp
stow starship
