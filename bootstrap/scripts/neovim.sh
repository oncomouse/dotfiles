#!/usr/bin/env bash
## Setup Vim and NeoVim
if test ! "~/.local/share/nvim/site/autoload/plug.vim"; then
  mkdir -p ~/.local/share/nvim/site/autoload
  curl -fLo ~/.local/share/nvim/site/autoload/plug.vim \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  nvim +PlugInstall +qall
else
  nvim +PlugInstall +PlugClean +qall
fi
fish -c nvim-nightly
launchctl load macos/com.pilsch.nvim-nightly.plist
