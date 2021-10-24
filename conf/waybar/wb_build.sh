#!/usr/bin/env bash

cwd="$(pwd)"
[[ -d "$HOME/.local/share/Waybar" ]] || git clone https://github.com/Alexays/Waybar "$HOME/.local/share/Waybar"
cd "$HOME/.local/share/Waybar" || exit
git pull
git checkout -b hide-empty
git apply "$HOME/dotfiles/conf/waybar/waybar-hide-empty.diff"
[[ -d "$HOME/.local/share/Waybar/build" ]] || mkdir -p "$HOME/.local/share/Waybar/build"
meson build
ninja -C build
sudo ninja -C build install
git checkout -- .
git checkout master
git branch -D hide-empty
cd "$cwd" || exit
