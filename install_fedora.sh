#!/usr/bin/env sh

Help()
{
   # Display Help
   echo "Please choose what to install"
   echo
   echo "Syntax: scriptTemplate [-all|terra|emacs|code|docker]"
   echo "options:"
   echo "all       All sections."
   echo "terra     Terraform."
   echo "emacs     Emacs."
   echo "code      Visual Studio Code."
   echo "docker    Docker."
   echo "pwsh      PowerShell."
   echo
}

all=''
terra=''
emacs=''
code=''
docker=''
pwsh=''

sudo dnf update -y
sudo dnf upgrade -y
sudo dnf install -y stow

git config --global user.email "boshoffwillem@protonmail.com"
git config --global user.name "Willem Boshoff"

# Rust
# =============================================================================
# Install rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# =============================================================================

# use starship shell prompt
# =============================================================================
curl -sS https://starship.rs/install.sh | sh
stow starship
# =============================================================================

if [ -d "~/jetbrains" ]; then
    echo "Installing jetbrains toolbox..."
    wget https://download.jetbrains.com/toolbox/jetbrains-toolbox-2.7.0.48109.tar.gz
    sudo tar -xvzf jetbrains-toolbox-2.7.0.48109.tar.gz
    sudo mv jetbrains-toolbox-2.7.0.48109 ~/jetbrains
    rm jetbrains-toolbox-2.7.0.48109.tar.gz
else
    echo "Skipping jetbrains toolbox installation."
fi

if [ -d "~/.dotnet" ]; then
    echo "Installing dotnet..."
    wget https://dot.net/v1/dotnet-install.sh -O ~/dotnet-install.sh
    chmod +x ~/dotnet-install.sh
    ~/dotnet-install.sh -c STS
else
    echo "Skipping dotnet installation."
fi

if [ -n "$terra" ] || [ -n "$all" ]; then
    echo "Installing terraform..."
else
    echo "Skipping terraform installation."
fi

rm ~/.bashrc
stow bashrc
source ~/.bashrc

# Get the options
while getopts ":hn:" option; do
   case $option in
      h) # display Help
         Help
         exit;;
      all) # Install all
         all=1;;
      terra) # Install Terraform
         terra=1;;
      emacs) # Install Emacs
         emacs=1;;
      code) # Install Visual Studio Code
          code=1;;
      docker) # Install Docker
          docker=1;;
      pwsh) # Install PowerShell
          pwsh=1;;
     \?) # Invalid option
         echo "Error: Invalid option"
         exit;;
   esac
done

if [ -n "$code" ] || [ -n "$all" ]; then
    echo "Installing terraform..."
else
    echo "Skipping terraform installation."
fi
