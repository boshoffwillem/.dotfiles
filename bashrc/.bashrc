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
export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools:$ANDROID_HOME/emulator
export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
# export JAVA_HOME=/usr/bin/java
export PATH=$JAVA_HOME/bin:$PATH
export DOTNET_ROOT=$HOME/.dotnet
export PATH=$PATH:$DOTNET_ROOT:$DOTNET_ROOT/tools

eval "$(starship init bash)"
. "$HOME/.cargo/env"

# Turso
export PATH="$PATH:/home/boshoffwillem/.turso"
