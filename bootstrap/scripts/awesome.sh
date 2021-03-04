#!/usr/bin/env bash

# Clone Rofi Scripts for Powermenu:
if [ -d ~/.local/share/rofi/rofi-scripts ]; then
  lpwd="$(pwd)"
  cd ~/.local/share/rofi/rofi-scripts || exit
  git pull
  cd "$lpwd" || exit
else
  mkdir -p ~/.local/share/rofi/rofi-scripts
  git clone https://github.com/adi1090x/rofi ~/.local/share/rofi/rofi-scripts
fi

# Clone Polybar Scripts for mpris_tail:
if [ -d ~/.local/share/polybar-scripts ]; then
  lpwd="$(pwd)"
  cd ~/.local/share/polybar-scripts || exit
  git pull
  cd "$lpwd" || exit
else
  mkdir -p ~/.local/share/polybar-scripts
  git clone https://github.com/polybar/polybar-scripts/ ~/.local/share/polybar-scripts
fi
