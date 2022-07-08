#!/usr/bin/env bash

curl -sLo ~/.local/nvim-macos.tar.gz https://github.com/neovim/neovim/releases/download/nightly/nvim-macos.tar.gz
owd="$(pwd)"
cd ~/.local || exit
[[ -d nvim-osx64 ]] && rm -rf nvim-osx64
tar zxvf nvim-macos.tar.gz
rm nvim-macos.tar.gz
sudo install -d nvim-osx64/ /usr/local
cd "$owd" || exit
