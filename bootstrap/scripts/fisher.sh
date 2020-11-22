#!/usr/bin/env bash
# Setup fish's environment:
mkdir -p ~/.config/fish/functions
mkdir -p ~/.config/fish/completions
ln -s ~/dotfiles/fish/config.fish ~/.config/fish/
ln -s ~/dotfiles/fish/fishfile ~/.config/fish/
ln -sf ~/dotfiles/fish/functions/*.fish ~/.config/fish/functions/
ln -sf ~/dotfiles/fish/completions/*.fish ~/.config/fish/completions/
find -L ~/.config/fish/functions -type l -exec rm -- {} +
find -L ~/.config/fish/completions -type l -exec rm -- {} +
script=$(printf "%s;" \
  "if not functions -q fisher" \
  "  set -q XDG_CONFIG_HOME" \
  "  or set XDG_CONFIG_HOME ~/.config" \
  "  curl https://git.io/fisher --create-dirs -sLo \$XDG_CONFIG_HOME/fish/functions/fisher.fish" \
  "end" \
  "fish -c fisher")
fish -c "$script"
# excitedleigh/virtualfish
# laughedelic/pisces
# wk/plugin-ssh-term-helper
# oh-my-fish/plugin-bang-bang
# oh-my-fish/plugin-fasd
# jorgebucaran/fisher
