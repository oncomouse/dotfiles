#!/usr/bin/env bash
## Install Node.js Modules
if [ -z $SERVER ]; then
  if ! npm list --depth 1 --global neovim > /dev/null 2>&1; then
    os=`bash ~/dotfiles/bootstrap/scripts/os.sh`
    npm install -g neovim jsonlint
    if [ "$os" != "macos" ]; then
      npm install -g diff-so-fancy
    fi
  fi
fi
