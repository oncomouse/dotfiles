#!/bin/bash

if [[ -d "$HOME/Projects/dwm" ]]; then
  cd "$HOME/Projects/dwm" || exit
  git pull
  make clean
else
  mkdir -p "$HOME/Projects"
  git clone https://github.com/bakkeby/dwm-flexipatch "$HOME/Projects/dwm"
  cd "$HOME/Projects/dwm" || exit
fi
cp "$HOME/dotfiles/conf/dwm/*" "$HOME/Projects/dwm"
make
sudo make install
cd "$HOME/dotfiles" || exit
