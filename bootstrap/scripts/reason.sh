#!/usr/bin/env bash

os=`bash ~/dotfiles/bootstrap/scripts/os.sh`
if [ $os != 'macos' ]; then
  os='linux'
fi
if ! which reason-language-server > /dev/null 2>&1; then
  curl -sLo ~/dotfiles/rls.zip https://github.com/jaredly/reason-language-server/releases/download/1.7.0/rls-$os.zip

  unzip ~/dotfiles/rls.zip
  mv ~/dotfiles/rls-$os/reason-language-server /usr/local/bin
  rm -r ~/dotfiles/rls-$os
  rm ~/dotfiles/rls.zip
fi

if which npm > /dev/null 2>&1; then
  if ! npm list --depth 1 --global reason-cli > /dev/null 2>&1; then
    npm install -g reason-cli@latest-$os
    npm install -g bs-platform
  fi
fi
