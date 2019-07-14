#!/usr/bin/env bash
## Install Python Modules
if ! pip3 list | ag "neovim" > /dev/null 2>&1; then
  os=`bash ~/dotfiles/bootstrap/scripts/os.sh`
  if os="macos"; then
    pip3 install mackup
  else
    pip3 install thefuck
  fi
  if [ -z $SERVER ];then
    pip3 install neovim pylint jedi
  else
    pip3 install pynvim
  fi
  pip3 install virtualfish
fi
