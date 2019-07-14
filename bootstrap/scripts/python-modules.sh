#!/usr/bin/env bash
## Install Python Modules
if ! pip3 list | ag "neovim" > /dev/null 2>&1; then
  pip3 install mackup neovim virtualfish pylint jedi
fi
