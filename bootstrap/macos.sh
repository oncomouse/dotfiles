#!/usr/bin/env bash

## Get ready for xcode/brew:
if ! xcode-select --version > /dev/null 2>&1; then
  xcode-select --install
fi

PATH="/opt/homebrew:${PATH}"

if test ! "$(which brew)"; then
  echo "Installing homebrew..."
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
else
  brew update
fi

## Install Brew
ln -sf ~/dotfiles/stow/homebrew/dot-Brewfile ~/.Brewfile
brew bundle install --global --no-lock

## GnuPG Setup for Mac:
mkdir -p ~/.gnupg
if [[ -d /opt/homebrew ]]; then
	echo "pinentry-program /opt/homebrew/bin/pinentry-mac" >> ~/.gnupg/gpg-agent.conf
else
	echo "pinentry-program /usr/local/bin/pinentry-mac" >> ~/.gnupg/gpg-agent.conf
fi

## macOS Defaults:

# Turn on font smoothing:
defaults write -g CGFontRenderingFontSmoothingDisabled -bool false
# Use <tab> to select any UI element:
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

## Fix Homebrew's python3 mess:
ln -sf $(brew --prefix)/bin/python3 $(brew --prefix)/bin/python
ln -sf $(brew --prefix)/bin/pip3 $(brew --prefix)/bin/pip

~/dotfiles/bootstrap/scripts/common.sh

## Use Fish
sudo dscl . -create "/Users/$USER" UserShell $(brew --prefix)/bin/fish

echo "In Seafile, sync $(tput bold)Mackup$(tput sgr0) and when $(tput bold)$(tput setaf 5)done syncing$(tput sgr0), run $(tput setaf 6)mackup restore$(tput sgr0) to load configuration files."

echo ""
