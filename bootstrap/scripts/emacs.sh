#!/usr/bin/env bash

/usr/bin/env python "$HOME/dotfiles/scripts/stow.py" -d "$HOME/dotfiles/stow" -t "$HOME" --no-folding --dotfiles --overwrite  -R emacs
# Install Chemacs2:
git clone https://github.com/plexus/chemacs2.git ~/.emacs.d

# Install Doom for Chemacs2:
mkdir -p ~/.emacs.configs
if [ ! -d ~/.emacs.configs/doom ]; then
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.configs/doom
fi
~/.emacs.configs/doom/bin/doom sync
~/.emacs.configs/doom/bindoom env
emacs --batch -f all-the-icons-install-fonts
