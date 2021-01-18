#!/bin/bash

oldpwd="$(pwd)"
if [[ ! -d "$HOME/Projects/aslstatus" ]]; then
  mkdir -p ~/Projects/
  git clone https://notabug.org/dm9pZCAq/aslstatus ~/Projects/aslstatus
fi
cd ~/Projects/aslstatus || exit
git pull
cp ~/dotfiles/conf/aslstatus/config.h .
make
sudo make install
make clean
git checkout -- config.h
cd "$oldpwd" || exit
