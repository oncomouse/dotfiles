#!/usr/bin/env bash

curl -sLo ~/.local/nvim-linux64.tar.gz https://github.com/neovim/neovim/releases/download/nightly/nvim-linux64.tar.gz
[[ -d ~/.local/nvim-linux64 ]] && rm -rf ~/.local/nvim-linux64
tar zxvf ~/.local/nvim-linux64.tar.gz --directory ~/.local
rm ~/.local/nvim-linux64.tar.gz
sudo install -d ~/.local/nvim-linux64/ /usr/local
