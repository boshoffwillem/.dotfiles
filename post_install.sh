#stow git
# stow nvim
#stow starship
#stow powershell

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
