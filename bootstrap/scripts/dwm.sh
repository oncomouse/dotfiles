#!/bin/bash

if [[ -d "$HOME/Projects/dwm" ]]; then
  cd "$HOME/Projects/dwm" || exit
  git pull
  make clean
else
  mkdir -p "$HOME/Projects"
  git clone https://github.com/oncomouse/dwm-flexipatch "$HOME/Projects/dwm"
  git checkout statuspadding
  ln -sf "$HOME/dotfiles/conf/dwm/patches.h" "$HOME/Projects/dwm/"
  ln -sf "$HOME/dotfiles/conf/dwm/config.h" "$HOME/Projects/dwm/"
  cd "$HOME/Projects/dwm" || exit
fi
make
sudo make install
if [[ ! -e "/usr/share/xsessions/dwm.desktop" ]]; then
  sudo tee /usr/share/xsessions/dwm.desktop <<- EOF
  [Desktop Entry]
  Encoding=UTF-8
  Name=Dwm
  Comment=the dynamic window manager
  Exec=dwm
  Icon=dwm
  Type=XSession
EOF
fi
cd "$HOME/dotfiles" || exit
