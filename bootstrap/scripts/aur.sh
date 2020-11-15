#!/usr/bin/env bash

packages=( "nerd-fonts-fira-code" "otf-hasklig" "pandoc-bin" )

mkdir -p ~/aur
for package in "${packages[@]}"; do
  cd ~/aur
  if [ -d "./$package" ]; then
    cd $package
    git pull
  else
    git clone "https://aur.archlinux.org/$package"
    cd $package
  fi
  makepkg -si
done
cd ~/dotfiles
