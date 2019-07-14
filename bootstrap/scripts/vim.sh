#!/usr/bin/env bash
## Setup Vim and NeoVim
if test ! "~/.vim/autoload/plug.vim"; then
  curl -fLo ~/.vim/autoload/plug.vim \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  mkdir -p ~/.local/share/nvim/site/autoload
  cp ~/.vim/autoload/plug.vim ~/.local/share/nvim/site/autoload/plug.vim
  vim +PlugInstall +qall
  nvim +PlugInstall +qall
else
  vim +PlugInstall +PlugClean +qall
  nvim +PlugInstall +PlugClean +qall
fi
