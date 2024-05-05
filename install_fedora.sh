#!/usr/bin/env sh

sudo dnf install -y git
sudo dnf install -y go
#sudo dnf install -y bsd-mailx build-essential language-pack-en-base
sudo dnf install -y dbus-x11 debhelper dpkg-dev emacs-common gawk libsqlite3x-devel \
  gvfs ibus-gtk3 libacl-devel libsoundio libsoundio-devel \
  aspell libasyncns atk at-spi2-atk \
  libbrotli glibc-devel cairo cairo-devel cairo-gobject \
  libcanberra libcanberra-devel libcanberra-gtk3 \
  libdatrie libdb dbus dbus-devel
  libwayland-client libwayland-cursor libwayland-egl \
  libwayland-server webkit2gtk4.0-devel libwebp-devel \
  woff woff2 libX11-devel libX11-xcb libXau libxcb \
  libdrm libwayland-egl enchant2-devel libepoxy flac \
  freetype freetype-devel egl-gbm gdk-pixbuf2 \
  libgle glib libglvnd libglvnd-glx gmp

# sudo dnf install -y libgpm2 libgpm2 libgpm-dev libgraphite2-3 \
#   libgstreamer1.0-0 libgstreamer-gl1.0-0 libgstreamer-plugins-base1.0-0 \
#   libgudev-1.0-0 libharfbuzz0b libharfbuzz0b \
#   libharfbuzz-icu0 libhyphen0 libibus-1.0-5 libice6 libicu70 libisl23 \
#   libjbig0 libjpeg8-dev libjpeg-dev \
#   libjpeg-turbo8 liblcms2-2 liblcms2-dev liblockfile1 liblockfile-dev \
#   libltdl7 libm17n-0 libm17n-dev libmpc3 libmpfr6 libncurses5-dev \
#   libnotify4 libnss-mdns libnss-myhostname libnss-sss libnss-systemd \
#   libogg0 liborc-0.4-0 liboss4-salsa2 libpango-1.0-0 \
#   libpangocairo-1.0-0 libpangoft2-1.0-0 libpixman-1-0 libpng16-16 \
#   libpng-dev libpulse0 librsvg2-2 librsvg2-dev libsasl2-2 libsecret-1-0 \
#   libselinux1-dev libsm6 libsndfile1 libsoup2.4-1 \
#   libssl3 libsss-nss-idmap0 libstdc++6 libsystemd-dev \
#   libtdb1 libthai0 libtinfo-dev \
#   libvorbis0a libvorbisenc2 libvorbisfile3 \
#   libxcb-render0 libxcb-shm0 libxcomposite1 libxcursor1 libxdamage1 \
#   libxdmcp6 libxext6 libxfixes3 libxfixes-dev libxi6 libxi-dev \
#   libxinerama1 libxkbcommon0 libxpm4 libxpm-dev \
#   libxrandr2 libxrender1 libxrender-dev libxslt1.1 libxt-dev libyajl2 \
#   mailutils procps quilt sharutils zlib1g-dev
sudo dnf install -y ripgrep
sudo dnf install -y bat
sudo dnf install -y exa
sudo dnf install -y gcc
sudo dnf install -y g++
sudo dnf install -y make
sudo dnf install -y cmake
sudo dnf install -y autoconf
sudo dnf install -y automake
sudo dnf install -y fd-find
sudo dnf install -y nodejs
sudo dnf install -y npm
sudo dnf install -y fontconfig-devel
sudo dnf install -y fzf
sudo dnf install -y tmux
sudo dnf install -y stow
sudo dnf install -y unzip
sudo dnf install -y autoconf
sudo dnf install -y fira-code-fonts
sudo dnf install -y gtk3-devel
sudo dnf install -y texinfo
sudo dnf install -y libtiff-devel
sudo dnf install -y giflib-devel
sudo dnf install -y libgccjit-devel
sudo dnf install -y libxml2-devel
sudo dnf install -y libtree-sitter-devel
sudo dnf install -y libotf-devel
sudo dnf install -y gnutls-devel
sudo dnf install -y jansson-devel
sudo dnf install -y ncurses-devel
sudo dnf install -y gnome-tweaks
sudo dnf install -y wget
sudo dnf install -y zig
sudo apt install zsh
chsh -s $(which zsh)
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
sudo dnf install -y hyperfine
sudo dnf install -y dotnet-sdk-8.0
dotnet tool install --global PowerShell

# linters and formatters
sudo npm install -g @bufbuild/buf
sudo npm install -g prettier
sudo dnf install -y shfmt

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
sudo dnf install -y neovim
stow nvim
# =============================================================================

# install emacs
# =============================================================================
# mkdir ~/code
# cd ~/code
# git clone git://git.sv.gnu.org/emacs.git -b emacs-29 --depth=1
# cd emacs
# ./autogen.sh
# ./configure --with-native-compilation=aot --with-json --with-tree-sitter --with-pgtk --with-mailutils
# make -j$(nproc)
# # src/emacs -Q
# sudo make -j$(nproc) install
# cd ~/.dotfiles
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
