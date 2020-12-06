#!/usr/bin/env bash

# Setup fish's environment:
/usr/bin/env python "$HOME/dotfiles/scripts/stow.py" -d "$HOME/dotfiles/stow" -t "$HOME" --no-folding --dotfiles -R fish

fisher_plugins=(
  "jorgebucaran/fisher"
  "excitedleigh/virtualfish"
  "laughedelic/pisces"
  "wk/plugin-ssh-term-helper"
  "oh-my-fish/plugin-bang-bang"
  "oh-my-fish/plugin-fasd"
)

# Install fisher:
script=$(printf "%s;" \
  "if not functions -q fisher" \
  "  set -q XDG_CONFIG_HOME" \
  "  or set XDG_CONFIG_HOME ~/.config" \
  "  curl https://git.io/fisher --create-dirs -sLo \$XDG_CONFIG_HOME/fish/functions/fisher.fish" \
  "  source \$XDG_CONFIG_HOME/fish/functions/fisher.fish" \
  "  fisher install $(printf "%s " "${fisher_plugins[@]}")" \
  "else" \
  "  fisher update" \
  "end")
fish -c "$script"
