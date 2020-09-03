#!/usr/bin/env bash
## Setup Vim and NeoVim
if [ ! -d "$HOME/.vim/autoload/plug.vim" ]; then
  mkdir -p ~/.vim/autoload
  curl -fLo ~/.vim/autoload/plug.vim \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  vim +PlugInstall +qall
  if command -v nvim &> /dev/null; then
    nvim +PlugInstall +qall
  fi
else
  vim +PlugInstall +PlugClean +qall
  if command -v nvim &> /dev/null; then
    nvim +PlugInstall +PlugClean +qall
  fi
fi
