  #!/usr/bin/env sh
  
  sudo zypper install -y git
  sudo zypper install -y ripgrep
  sudo zypper install -y bat
  sudo zypper install -y exa
  sudo zypper install -y gcc
  sudo zypper install -y make
  sudo zypper install -y cmake
  sudo zypper install -y fd
  sudo zypper install -y starship
  sudo zypper install -y nodejs
  sudo zypper install -y npm
  sudo zypper install -y fontconfig
  sudo zypper install -y emacs
  sudo zypper install -y fzf
  sudo zypper install -y neovim
  sudo zypper install -y stow
  
  sudo zypper install libicu
  sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
  wget https://packages.microsoft.com/config/opensuse/15/prod.repo
  sudo mv prod.repo /etc/zypp/repos.d/microsoft-prod.repo
  sudo chown root:root /etc/zypp/repos.d/microsoft-prod.repo
  sudo zypper install -y dotnet-sdk-7.0
  dotnet tool install -g dotnet-grpc
  
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
  ~/.config/emacs/bin/doom install
  
  # use starship shell prompt
  curl -sS https://starship.rs/install.sh | sh
  echo 'alias ls="exa"' | sudo tee -a ~/.bashrc
  echo 'alias cat="bat"' | sudo tee -a ~/.bashrc
  echo '# use starship prompt' | sudo tee -a ~/.bashrc
  echo 'eval "$(starship init bash)"' | sudo tee -a ~/.bashrc
  
  # Install rustup
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
