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
   echo "docker    Docker."
   echo "pwsh      PowerShell."
   echo
}

all=''
terra=''
emacs=''
docker=''
pwsh=''

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
      docker) # Install Docker
          docker=1;;
      pwsh) # Install PowerShell
          pwsh=1;;
     \?) # Invalid option
         echo "Error: Invalid option"
         exit;;
   esac
done

sudo dnf update -y
sudo dnf upgrade -y
sudo dnf install -y clang cmake ninja-build gtk3-devel
sudo dnf install -y inxi
sudo dnf install -y stow

git config --global user.email "boshoffwillem@protonmail.com"
git config --global user.name "Willem Boshoff"

stow ideavimrc

sudo dnf install -y nodejs
sudo dnf install -y java
sudo dnf install -y java-devel

# Flutter
# =============================================================================
echo "Installing VS Code..."
sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\nautorefresh=1\ntype=rpm-md\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" | sudo tee /etc/yum.repos.d/vscode.repo > /dev/null
dnf check-update
sudo dnf install -y code # or code-insiders
# =============================================================================

# Flutter
# =============================================================================
# Install rustup
echo "Installing Flutter..."
wget "https://storage.googleapis.com/flutter_infra_release/releases/stable/linux/flutter_linux_3.32.7-stable.tar.xz"
mkdir ~/development
tar -xf flutter_linux_3.32.7-stable.tar.xz -C ~/development/
rm flutter_linux_3.32.7-stable.tar.xz
# =============================================================================

# Rust
# =============================================================================
# Install rustup
echo "Installing Rust..."
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# =============================================================================

# use starship shell prompt
# =============================================================================
echo "Installing Starship..."
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
    ~/dotnet-install.sh -c LTS
else
    echo "Skipping dotnet installation."
fi

if [ -n "$terra" ] || [ -n "$all" ]; then
    echo "Installing terraform..."
else
    echo "Skipping terraform installation."
fi

sudo npm install -g @anthropic-ai/claude-code
sudo npm install -g @google/gemini-cli
curl -sSfL https://get.tur.so/install.sh | bash
dotnet tool install --ignore-failed-sources --add-source https://api.nuget.org/v3/index.json --global dotnet-ef
sudo dnf install -y gimp
sudo dnf install -y inkscape
# sudo dnf install -y hyprland

sudo dnf install -y neovim
stow nvim

sudo dnf install -y emacs
stow emacs

mkdir ~/code
mkdir ~/code/work

rm ~/.bashrc
stow bashrc
source ~/.bashrc
