#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
# Deal with terminfo nonesense:
if [ "$os" = "macos" ]; then
  /usr/local/Cellar/ncurses/6.3/bin/infocmp -x tmux-256color > ~/tmux-256color.src
  /usr/bin/tic -xe tmux-256color ~/tmux-256color.src
fi
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
