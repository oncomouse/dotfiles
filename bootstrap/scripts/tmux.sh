#!/usr/bin/env bash
## Setup Oh My Tmux!
if test ! ~/.tmux ; then
  git clone https://github.com/gpakosz/.tmux ~/.tmux
  ln -s -f ~/.tmux/.tmux.conf ~/.tmux.conf
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi
# Configure Wego (used in tmux)
if test ! "~/go/src/github.com/schachmat/wego" ; then
  go get github.com/schachmat/wego
  ln -s ~/dotfiles/wego/one-liner.go ~/go/src/github.com/schachmat/wego/frontends/
  go install github.com/schachmat/wego
fi
# Configure wttr-safe, the failback client for wttr.in and wego:
if test ! "~/go/src/github.com/oncomouse/wttr-safe" ; then
  go get github.com/oncomouse/wttr-safe
  go install github.com/oncomouse/wttr-safe
fi
