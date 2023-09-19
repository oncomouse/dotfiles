#!/usr/bin/env bash

# Install Chemacs2:
git clone https://github.com/plexus/chemacs2.git ~/.emacs.d

# Configure Chemacs2:
mkdir -p ~/.emacs.configs

# Install Doom for Chemacs2:
if [ ! -d ~/.emacs.configs/doom ]; then
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.configs/doom
fi

# Install Purcell for Chemacs2:
if [ ! -d ~/.emacs.configs/purcell ]; then
  git clone https://github.com/purcell/emacs.d ~/.emacs.configs/purcell
fi

# Stow our configuration
  /usr/bin/env python "$HOME/dotfiles/scripts/stow.py" -d "$HOME/dotfiles/stow" -t "$HOME" --no-folding --dotfiles --overwrite  -R emacs

# Install doom
~/.emacs.configs/doom/bin/doom sync
~/.emacs.configs/doom/bin/doom env
