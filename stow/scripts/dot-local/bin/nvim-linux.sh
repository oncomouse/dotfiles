#!/usr/bin/env bash

curl -sLo ~/.local/nvim-linux64.tar.gz https://github.com/neovim/neovim/releases/download/nightly/nvim-linux64.tar.gz
owd="$(pwd)"
cd ~/.local || exit
[[ -d nvim-linux64 ]] && rm -rf nvim-linux64
tar zxvf nvim-linux64.tar.gz
rm nvim-linux64.tar.gz
cd "$owd" || exit
