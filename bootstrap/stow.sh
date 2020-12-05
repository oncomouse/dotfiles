#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
# Basic Setup:
mkdir -p ~/.config/fish
stow -d ~/dotfiles/configurations -t ~ fish
if [ -z "$SERVER" ]; then
  stow -d ~/dotfiles/configurations -t ~ kitty
  stow -d ~/dotfiles/configurations -t ~ neovim-desktop
  stow -d ~/dotfiles/configurations -t ~ wal
  stow -d ~/dotfiles/configurations -t ~ haskell
  stow -d ~/dotfiles/configurations -t ~ proselint
  stow -d ~/dotfiles/configurations -t ~ rubocop
  stow -d ~/dotfiles/configurations -t ~ vale
  if [ "$os" == "macos" ]; then
    stow -d ~/dotfiles/configurations -t ~ Brewfile
    stow -d ~/dotfiles/configurations -t ~ hammerspoon
    stow -d ~/dotfiles/configurations -t ~ mackup
  elif [ "$os" == "arch" ]; then
    # Other setup files:
    stow -d ~/dotfiles/configurations -t ~ rofi
    stow -d ~/dotfiles/configurations -t ~ bspwm
    stow -d ~/dotfiles/configurations -t ~ sxhkd
    stow -d ~/dotfiles/configurations -t ~ dunst
    stow -d ~/dotfiles/configurations -t ~ xorg
    stow -d ~/dotfiles/configurations -t ~ gtk-3.0
    stow -d ~/dotfiles/configurations -t ~ gtk-2.0
    stow -d ~/dotfiles/configurations -t ~ redshift
  fi
  ## Configure NCSpot:
  if [ "$os" == "macos" ]; then
    mkdir -p ~/Library/Preferences/org.affekt.ncspot/
    stow -d ~/dotfiles/configurations -t ~ ncspot-macos
  else
    mkdir -p ~/.config/ncspot
    stow -d ~/dotfiles/configurations -t ~ ncspot-linux
  fi
else
  stow -d ~/dotfiles/configurations -t ~ neovim-server
fi
stow -d ~/dotfiles/configurations -t ~ vim
