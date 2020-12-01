#!/usr/bin/env bash

packages=(
  "i3lock-color-git"
  "lemonbar-xft-git"
  "ncspot"
  "otf-nerd-fonts-fira-code"
  "otf-hasklig"
  "pandoc-bin"
  "perl-term-shellui"
  "rofi-wifi-menu-git"
  "shellcheck-bin"
  "tllocalmgr-git"
  "https://github.com/oncomouse/vale-bin"
  "visual-studio-code-bin"
  "xtitle"
  "zotero"
)

mkdir -p ~/aur
for package in "${packages[@]}"; do
  cd ~/aur || continue
  regex='^http'
  if [[ $package =~ $regex ]]; then
    url=$package
    [[ $url =~ \/([^/]*)$ ]]
    package=${BASH_REMATCH[1]}
  else
    url="https://aur.archlinux.org/$package"
  fi
  if [ -d "./$package" ]; then
    cd "$package" || continue
    git pull
  else
    git clone "$url"
    cd "$package" || continue
  fi
  makepkg -si --noconfirm
done
cd ~/dotfiles || exit
