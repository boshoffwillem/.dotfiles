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
export ANDROID_HOME="~/Android/Sdk"
export VSS_NUGET_EXTERNAL_FEED_ENDPOINTS='{"endpointCredentials": [{"endpoint":"https://psicle.pkgs.visualstudio.com/_packaging/Babel_Production/nuget/v3/index.json", "password":"y2f3dxvhfr3nvd2ugeqo56tyhtz3ssdflmku4y3k5xmaujj36f2a"},{"endpoint":"https://psicle.pkgs.visualstudio.com/_packaging/PsicleWeb_Domain/nuget/v3/index.json", "password":"y2f3dxvhfr3nvd2ugeqo56tyhtz3ssdflmku4y3k5xmaujj36f2a"},{"endpoint":"https://psicle.pkgs.visualstudio.com/_packaging/Gadgets/nuget/v3/index.json", "password":"y2f3dxvhfr3nvd2ugeqo56tyhtz3ssdflmku4y3k5xmaujj36f2a"}]}'
export ARM_CLIENT_ID='dfb44a56-0e26-4430-a9f3-c44e27c7b8f5'
export ARM_CLIENT_SECRET='VjK8Q~XJCMHqZYCt3-AIg55MgMDi55K1_ZkW3b3K'
export ARM_SUBSCRIPTION_ID='865cf421-b3c0-4e25-bbee-aad3c9b00ffa'
export ARM_TENANT_ID='900993f4-cd20-41e9-b024-a200ed7ba3ec'

eval "$(starship init bash)"
. "$HOME/.cargo/env"
