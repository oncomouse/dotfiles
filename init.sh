#!/usr/bin/env bash

os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)

# Build bombadil:
/usr/bin/env sh ~/dotfiles/bootstrap/bombadil.sh
if [ -n "$SERVER" ]; then
  bombadil install -c ~/dotfiles/bombadil-server.toml
else
  bombadil install -c ~/dotfiles/bombadil.toml
fi
if [ "$os" == "macos" ]; then
  bombadil link -p macos
else
  bombadil link -p linux
fi
