stow git
# stow nvim
stow starship
stow powershell

# use starship shell prompt
echo '' | sudo tee -a ~/.bashrc
echo 'alias ls="exa"' | sudo tee -a ~/.bashrc
echo 'alias cat="bat"' | sudo tee -a ~/.bashrc
echo '# use starship prompt' | sudo tee -a ~/.bashrc
echo 'eval "$(starship init bash)"' | sudo tee -a ~/.bashrc

# install packer for nvim
git clone --depth 1 https://github.com/wbthomason/packer.nvim\
 ~/.local/share/nvim/site/pack/packer/start/packer.nvim

# nvim config
git clone https://github.com/boshoffwillem/nvim\
    ~/.config/nvim

# emacs config
git clone https://github.com/boshoffwillem/.emacs.d ~/.emacs.d

# some extra packges for null-ls in nvim
# spell checker
sudo npm install -g cspell

# markdown linter
sudo npm install -g markdownlint

# rust setup
rustup install stable
rustup default stable

# git setup
# set up delta for better git diffs
touch ~/.gitignore
echo "
    [user]\
    \n  name = Willem Boshoff\
    \n  email = boshoffwillem@protonmail.com\
    \n[core]\
    \n  pager = delta\
    \n  editor = nvim\
    \n\
    \n[interactive]\
    \n  diffFilter = delta --color-only\
    \n\
    \n[delta]\
    \n  navigate = true # use n and N to move between diff scenarios\
    \n  light = false # true if light background\
    \n\
    \n[merge]\
    \n  conflictstyle = diff3\
    \n\
    \n[diff]\
    \n  colorMoved = default" | sudo tee -a ~/.gitignore
