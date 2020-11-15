#!/usr/bin/env bash

packages=( "nerd-fonts-fira-code" "otf-hasklig" "ncspot" "pandoc-bin" "vale" "visual-studio-code-bin" "zotero" )

mkdir -p ~/aur
for package in "${packages[@]}"; do
  cd ~/aur || continue
  if [ -d "./$package" ]; then
    true
    # cd "$package" || continue
    # git pull
  else
    git clone "https://aur.archlinux.org/$package"
    cd "$package" || continue
    makepkg -si
  fi
done
cd ~/dotfiles || exit
