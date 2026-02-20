# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi

alias csharpier="dotnet csharpier"

export PATH
export ANDROID_HOME=$HOME/Library/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools:$ANDROID_HOME/emulator
# export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
export JAVA_HOME=/Library/Java/JavaVirtualMachines/microsoft-17.jdk/Contents/Home
# export JAVA_HOME=/usr/bin/java
export PATH=$JAVA_HOME/bin:$PATH
export PATH=$HOME/tools:$PATH
export PATH="$PATH:$HOME/.dotnet"
export PATH=$HOME/tools/omnisharp:$PATH
export DOTNET_ROOT=$HOME/.dotnet
export DOTNET_ROOT_ARM64=$HOME/.dotnet
export PATH=$PATH:$DOTNET_ROOT:$DOTNET_ROOT/tools
export PATH=$HOME/development/flutter/bin:$PATH
export GEM_HOME=$HOME/.gem
export PATH=$GEM_HOME/bin:$PATH
export PATH="$HOME/.mix/escripts:$PATH"
export PATH="$PATH":"$HOME/.pub-cache/bin"

eval "$(starship init bash)"
. "$HOME/.cargo/env"

# Turso
export PATH="$PATH:/home/boshoffwillem/.turso"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# pnpm
export PNPM_HOME="/home/willem-boshoff/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
export PATH="$PATH:/opt/mssql-tools18/bin"
installs_dir=$HOME/.elixir-install/installs
export PATH=$installs_dir/otp/28.1/bin:$PATH
export PATH=$installs_dir/elixir/1.19.5-otp-28/bin:$PATH

eval "$(/home/willem-boshoff/.local/bin/mise activate bash)" # added by https://mise.run/bash
