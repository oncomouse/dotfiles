#!/usr/bin/env bash

curl -sLo ~/.local/nvim-macos.tar.gz https://github.com/neovim/neovim/releases/download/nightly/nvim-macos.tar.gz
owd="$(pwd)"
cd ~/.local || exit
[[ -d nvim-macos ]] && rm -rf nvim-macos
tar zxvf nvim-macos.tar.gz
rm nvim-macos.tar.gz
sudo install -d nvim-macos/ /usr/local
cd "$owd" || exit
