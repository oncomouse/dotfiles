#!/bin/bash

# Create XDG directory for Spicetify:
if [[ ! -d ~/.config/spicetify ]]; then
  spicetify
fi

# Create theme repo:
if [[ ! -d ~/Projects/spicetify-themes/ ]]; then
  git clone https://github.com/morpheusthewhite/spicetify-themes/ ~/Projects/spicetify-themes
fi

# Link themes and configure Dribblish:
if [[ ! -d ~/.config/spicetify/Themes/Dribbblish ]]; then
  for dir in $(fd -t d -d 1); do
    ln -s "$(pwd)/$dir" "$HOME/.config/spicetify/Themes/"
  done
  cp ~/.config/spicetify/Themes/Dribblish/dribblish.js ~/.config/spicetify/Extensions
  spicetify config extensions dribbblish.js
  spicetify config inject_css 1 replace_colors 1 overwrite_assets 1
fi

cd ~/Projects/spicetify-themes || exit
git checkout master
git pull

git branch | grep -v "master" | xargs git branch -D
git checkout -b xrdb-dribblish
git apply ~/dotfiles/conf/spicetify/patches/dribblish-xrdb.diff

spicetify config current_theme Dribbblish color_scheme xrdb
spicetify apply

cd ~/dotfiles || exit
