#!/usr/bin/env bash
## Setup Vim and NeoVim
if test ! "$HOME/.vim/autoload/plug.vim"; then
  mkdir -p ~/.vim/autoload
  curl -fLo ~/.vim/autoload/plug.vim \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  vim +PlugInstall +qall
else
  vim +PlugInstall +PlugClean +qall
fi
