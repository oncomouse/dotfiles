#!/usr/bin/env bash
## Setup Oh My Fish!
if test ! ~/.local/share/omf ; then
  curl -L https://get.oh-my.fish | fish
  omf install fasd ssh-term-helper fish-spec nodenv virtualfish
else
  # Sometimes conf.d/omf.fish gets deleted by mistake, but OMF is installed:
  if test ! ~/.config/fish/conf.d/omf.fish; then
    curl -sL https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/templates/config.fish -o ~/.config/fish/conf.d/omf.fish
  fi
fi
