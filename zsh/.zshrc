# source nix packages
if [ -e /home/boshoffwillem/.nix-profile/etc/profile.d/nix.sh ]; then . /home/boshoffwillem/.nix-profile/etc/profile.d/nix.sh; fi

# starship prompt
eval "$(starship init zsh)"


# aliases
alias cat=bat
alias grep=rg
alias ls='exa -al'
alias v='nvim'
