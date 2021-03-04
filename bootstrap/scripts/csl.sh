#!/usr/bin/env bash
## Install CSL support:
if [ ! -d "$HOME/.csl" ]; then
  git clone https://github.com/citation-style-language/styles ~/.csl
else
  lwd="$(pwd)"
  cd ~/.csl || exit
  git pull
  cd "$lwd" || exit
fi
