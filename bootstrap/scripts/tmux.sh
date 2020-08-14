#!/usr/bin/env bash
## Setup Oh My Tmux!
if test ! ~/.tmux ; then
  git clone https://github.com/gpakosz/.tmux ~/.tmux
  ln -s -f ~/.tmux/.tmux.conf ~/.tmux.conf
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi
