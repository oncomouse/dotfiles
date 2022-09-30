#!/usr/bin/env bash

curl -sLo /tmp/nvim-linux64.tar.gz https://github.com/neovim/neovim/releases/download/nightly/nvim-linux64.tar.gz
[[ -d ~/.local/nvim-linux64 ]] && rm -rf ~/.local/nvim-linux64
tar zxvf /tmp/nvim-linux64.tar.gz --directory ~/.local
rm /tmp/nvim-linux64.tar.gz
