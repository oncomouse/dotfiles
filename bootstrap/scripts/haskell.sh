#!/usr/bin/env bash

mkdir -p ~/.local/bin
export PATH="$HOME/.local/bin:$PATH"
if test ! "$(which ghcup)"; then
  os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
  if [ "$os" == 'macos' ]; then
    curl --proto '=https' --tlsv1.2 -s -o ~/.local/bin/ghcup https://downloads.haskell.org/~ghcup/x86_64-apple-darwin-ghcup
  else
    curl --proto '=https' --tlsv1.2 -s -o ~/.local/bin/ghcup https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup
  fi
  chmod +x ~/.local/bin/ghcup
else
  ghcup upgrade
fi

ghcup install ghc recommended
ghcup install cabal recommended
ghcup install hls recommended

ghcup set ghc recommended
ghcup set cabal recommended
ghcup set hls recommended

if [ ! -f ~/.stack/config.yaml ]; then
  mkdir -p ~/.stack
  printf "system-ghc: true" > ~/.stack/config.yaml
fi

stack install stylish-haskell
stack install floskell
