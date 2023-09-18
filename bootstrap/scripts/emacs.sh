#!/usr/bin/env bash

/usr/bin/env python "$HOME/dotfiles/scripts/stow.py" -d "$HOME/dotfiles/stow" -t "$HOME" --no-folding --dotfiles --overwrite  -R emacs
# Install Chemacs2:
git clone https://github.com/plexus/chemacs2.git ~/.emacs.d

# Install Doom for Chemacs2:
mkdir -p ~/.emacs.configs
if [ ! -d ~/.emacs.configs/doom ]; then
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.configs/doom
fi
PATH="$HOME/.~/.emacs.configs/doom/bin:$PATH"
doom sync
doom env
emacs --with-profile doom --batch -f all-the-icons-install-fonts
