#!/usr/bin/env bash


os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)

if [ "$os" == "macos" ]; then
  ## Get ready for xcode/brew:
  if ! xcode-select --version > /dev/null 2>&1; then
    xcode-select --install
  fi
  if test ! "$(which brew)"; then
    echo "Installing homebrew..."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  else
    brew update
  fi
  brew install cargo
else
  curl https://sh.rustup.rs -sSf | sh
fi
cargo install toml-bombadil

if [ -z $SERVER ]; then
  bombadil install -c ~/dotfiles/bombadil.toml
else
  bombadil install -c ~/dotfiles/bombadil-server.toml
fi
