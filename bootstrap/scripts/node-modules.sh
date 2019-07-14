#!/usr/bin/env bash
## Install Node.js Modules
if ! npm list --depth 1 --global neovim > /dev/null 2>&1; then
  npm install -g neovim jsonlint diff-so-fancy
fi
