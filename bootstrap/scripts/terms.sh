#!/usr/bin/env bash
## Setup $TERM
if test ! "$HOME/.terminfo" ; then
  tic -x -o ~/.terminfo ~/dotfiles/terms/tmux.terminfo
  tic -x -o ~/.terminfo ~/dotfiles/terms/tmux-256color.terminfo
  tic -x -o ~/.terminfo ~/dotfiles/terms/xterm-256color.terminfo
fi
