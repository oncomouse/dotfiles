#!/usr/bin/env bash

/usr/bin/env python "$HOME/dotfiles/scripts/stow.py" -d "$HOME/dotfiles/stow" -t "$HOME" --no-folding --dotfiles --overwrite  -R emacs
if [ ! -d ~/.config/emacs ]; then
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
fi
PATH="$HOME/.config/emacs/bin:$PATH"
doom sync
doom env
emacs --batch -f all-the-icons-install-fonts
