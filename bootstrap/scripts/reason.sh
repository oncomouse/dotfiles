#!/usr/bin/env bash

os=`bash ~/dotfiles/bootstrap/scripts/os.sh`

if ! which reason-language-server > /dev/null 2>&1; then
  if os='macos'; then
    curl -slo ~/dotfiles/rls.zip https://github.com/jaredly/reason-language-server/releases/download/1.7.0/rls-macos.zip
  else
      curl -slo ~/dotfiles/rls.zip https://github.com/jaredly/reason-language-server/releases/download/1.7.0/rls-linux.zip
  fi

  unzip ~/dotfiles/rls.zip -d ~/dotfiles/rls
  mv ~/dotfiles/rls/reason-language-server /usr/local/bin
  rm -r ~/dotfiles/rls
  rm ~/dotfiles/rls.zip
fi

if which npm > /dev/null 2>&1; then
  if ! npm list --depth 1 --global reason-cli > /dev/null 2>&1; then
    if os='macos'; then
      npm install -g reason-cli@latest-macos
    else
      npm install -g reason-cli@latest-linux
    fi
  fi
fi
