#!/usr/bin/env bash

# Install ghcup:
if test ! "$(which ghcup)"; then
  mkdir -p ~/.local/bin
  os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
  if [ "$os" == 'macos' ]; then
    curl --proto '=https' --tlsv1.2 -s -o ~/.local/bin/ghcup https://downloads.haskell.org/~ghcup/x86_64-apple-darwin-ghcup
  else
    curl --proto '=https' --tlsv1.2 -s -o ~/.local/bin/ghcup https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup
  fi
  chmod +x ~/.local/bin/ghcup
fi

# Source ghcup install location:
export PATH="$HOME/.local/bin:$PATH"
ghcup upgrade
ghcup install ghc recommended
ghcup install cabal recommended
ghcup install hls recommended
ghcup set ghc recommended
ghcup set cabal recommended
ghcup set hls recommended

stack config set system-ghc --global true
stack install stylish-haskell
#stack install hindent
