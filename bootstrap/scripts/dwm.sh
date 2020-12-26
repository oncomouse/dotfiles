#!/bin/bash

if [[ -d "$HOME/Projects/dwm" ]]; then
  cd "$HOME/Projects/dwm" || exit
  git pull
  make clean
else
  mkdir -p "$HOME/Projects"
  git clone https://git.suckless.org/dwm "$HOME/Projects/dwm"
  ln -sf "$HOME/dotfiles/conf/dwm/config.h" "$HOME/Projects/dwm/"
  cd "$HOME/Projects/dwm" || exit
fi
git checkout -b build
git apply "$HOME/dotfiles/conf/dwm/combined-dwm.patch"
make
sudo make install
git checkout master
git branch -D build
git checkout -- .
make clean
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
