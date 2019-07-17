#!/usr/bin/env bash
## Install Node.js Modules
if which npm > /dev/null 2>&1; then
  if ! which mf > /dev/null 2>&1; then
    npm install -g mf-cli
  fi
  if ! which trash > /dev/null 2>&1; then
    npm install -g trash-cli
  fi
  if ! which diff-so-fancy > /dev/null 2>&1; then
    npm install -g diff-so-fancy
  fi
  if [ -z $SERVER ]; then
    if ! npm list --depth 1 --global neovim > /dev/null 2>&1; then
      npm install -g neovim jsonlint
    fi
  fi
fi
