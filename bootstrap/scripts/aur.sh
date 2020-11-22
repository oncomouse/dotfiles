#!/usr/bin/env bash

packages=(
  "lemonbar-xft-git"
  "ncspot"
  "nerd-fonts-fira-code"
  "otf-hasklig"
  "pandoc-bin"
  "vale"
  "visual-studio-code-bin"
  "xtitle"
  "zotero"
)

mkdir -p ~/aur
for package in "${packages[@]}"; do
  cd ~/aur || continue
  if [ -d "./$package" ]; then
    cd "$package" || continue
    git pull
  else
    git clone "https://aur.archlinux.org/$package"
    cd "$package" || continue
  fi
  makepkg -si
done
cd ~/dotfiles || exit
