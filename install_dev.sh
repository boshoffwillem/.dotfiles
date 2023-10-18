#!/usr/bin/env bash

sudo apt install -y yamllint
sudo npm install -g yaml-language-server
sudo npm install -g azure-pipelines-language-server
rustup component add rust-src
rustup component add rust-analyzer

mkdir ~/code
git clone https://github.com/hashicorp/terraform-ls.git --depth=1 ~/code/terraform-ls
cd ~/code/terraform-ls
go install
mv ~/terraform-ls ~/.local/bin
