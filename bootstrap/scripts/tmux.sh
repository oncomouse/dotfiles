#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
# Deal with terminfo nonesense:
if [ ! -d ~/.tmux/plugins/tpm ]; then
  if [ "$os" = "macos" ]; then
    /opt/homebrew/Cellar/ncurses/6.4/bin/infocmp -x tmux-256color > ~/tmux-256color.src
    /usr/bin/tic -xe tmux-256color ~/tmux-256color.src
  fi
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi
