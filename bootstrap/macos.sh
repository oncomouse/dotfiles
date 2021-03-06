#!/usr/bin/env bash

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

## Install Brew
brew bundle install --global --no-lock

## GnuPG Setup for Mac:
mkdir -p ~/.gnupg
echo "pinentry-program /usr/local/bin/pinentry-mac" >> ~/.gnupg/gpg-agent.conf

## macOS Defaults:

# Turn on font smoothing:
defaults write -g CGFontRenderingFontSmoothingDisabled -bool false
# Use <tab> to select any UI element:
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

## Fix Homebrew's python3 mess:
if test ! "/usr/local/bin/python"; then
  ln -s "$(which python3)" /usr/local/bin/python
fi
if test ! "/usr/local/bin/pip"; then
  ln -s "$(which pip3)" /usr/local/bin/pip
fi

~/dotfiles/bootstrap/scripts/common.sh

## Use Fish
if ! echo "$SHELL" | ag fish > /dev/null 2>&1; then
  sudo dscl . -create "/Users/$USER" UserShell /usr/local/bin/fish
fi

## Configure Hammerspoon

## Configure Kitty:
mkdir -p ~/.terminfo/78/
ln -sf /Applications/kitty.app/Contents/Resources/terminfo/78/xterm-kitty ~/.terminfo/78

echo ""

echo "Run $(tput bold)$(tput setaf 6)bash ~/dotfiles/dns/bootstrap.sh$(tput sgr0) to install DNS proxy and local dev domains."

echo ""

echo "In Seafile, sync $(tput bold)Mackup$(tput sgr0) and when $(tput bold)$(tput setaf 5)done syncing$(tput sgr0), run $(tput setaf 6)mackup restore$(tput sgr0) to load configuration files."

echo ""
