#!/usr/bin/env bash

stow -R emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
PATH="$HOME/.config/emacs/bin:$PATH"
doom sync
doom env
emacs --batch -f all-the-icons-install-fonts
