#!/usr/bin/env bash
## Setup Vim and NeoVim
if [ ! -d "$HOME/.vim/autoload/plug.vim" ]; then
  mkdir -p ~/.vim/autoload
  curl -fLo ~/.vim/autoload/plug.vim \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  eval "$(which vim) +PlugInstall +qall"
  if command -v nvim &> /dev/null; then
    eval "$(which nvim) +PlugInstall +qall"
  fi
else
  eval "$(which vim) +PlugInstall +PlugClean +qall"
  if command -v nvim &> /dev/null; then
    eval "$(which nvim) +PlugInstall +PlugClean +qall"
  fi
fi
