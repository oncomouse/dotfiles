#!/usr/bin/env bash

if [ ! -f ~/.config/fish/functions/fisher.fish ]; then
  curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
  fish -c "fisher add jethrokuan/fzf fishgretel/fasd excitedleigh/virtualfish wk/plugin-ssh-term-helper"
fi
