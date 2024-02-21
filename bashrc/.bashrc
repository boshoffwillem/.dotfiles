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
export VSS_NUGET_EXTERNAL_FEED_ENDPOINTS='{"endpointCredentials": [{"endpoint":"https://psicle.pkgs.visualstudio.com/_packaging/Babel_Production/nuget/v3/index.json", "password":"qi3kh6a7tqzbvhxx55wr5kxpbsbgy6mviiradydedzfmvjfdueuq"}, {"endpoint":"https://psicle.pkgs.visualstudio.com/_packaging/PsicleWeb_Domain/nuget/v3/index.json", "password":"qi3kh6a7tqzbvhxx55wr5kxpbsbgy6mviiradydedzfmvjfdueuq"}, {"endpoint":"https://psicle.pkgs.visualstudio.com/_packaging/Gadgets/nuget/v3/index.json", "password":"qi3kh6a7tqzbvhxx55wr5kxpbsbgy6mviiradydedzfmvjfdueuq"}]}'

eval "$(starship init bash)"
. "$HOME/.cargo/env"
