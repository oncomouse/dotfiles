#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)

if [ "$os" == "macos" ]; then
  curl -sLo /tmp/nvim-macos.tar.gz https://github.com/neovim/neovim/releases/download/nightly/nvim-macos.tar.gz
  [[ -e /tmp/nvim-macos.tar.gz ]] && [[ -d ~/.local/nvim-macos ]] && rm -rf ~/.local/nvim-macos
  tar zxvf /tmp/nvim-macos.tar.gz --directory ~/.local
  rm /tmp/nvim-macos.tar.gz
else
  curl -sLo /tmp/nvim-linux64.tar.gz https://github.com/neovim/neovim/releases/download/nightly/nvim-linux64.tar.gz
  [[ -e /tmp/nvim-linux64.tar.gz ]] && [[ -d ~/.local/nvim-linux64 ]] && rm -rf ~/.local/nvim-linux64
  tar zxvf /tmp/nvim-linux64.tar.gz --directory ~/.local
  rm /tmp/nvim-linux64.tar.gz
fi
