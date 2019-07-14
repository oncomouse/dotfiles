#!/usr/bin/env bash

bash="$(which bash)"

## Get ready for xcode/brew:
xcode-select --install

if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
else
  brew update
fi

## Install Brew
brew bundle install

## GnuPG Setup for Mac:
mkdir -p ~/.gnupg
echo "pinentry-program /usr/local/bin/pinentry-mac" >> ~/.gnupg/gpg-agent.conf

## Git Keychain:
git config --global credential.helper osxkeychain


## Fix Homebrew's python3 mess:
if test ! "/usr/local/bin/python"; then
  ln -s "$(which python3)" /usr/local/bin/python
fi
if test ! "/usr/local/bin/pip"; then
  ln -s "$(which pip3)" /usr/local/bin/pip
fi

$bash ~/dotfiles/bootstrap/scripts/minimal-config.sh
$bash ~/dotfiles/bootstrap/scripts/mutt.sh
$bash ~/dotfiles/bootstrap/scripts/python-modules.sh
$bash ~/dotfiles/bootstrap/scripts/diff-so-fancy.sh
$bash ~/dotfiles/bootstrap/scripts/node-modules.sh
$bash ~/dotfiles/bootstrap/scripts/rbenv.sh
$bash ~/dotfiles/bootstrap/scripts/oh-my-fish.sh
$bash ~/dotfiles/bootstrap/scripts/tmux.sh
$bash ~/dotfiles/bootstrap/scripts/vim.sh
$bash ~/dotfiles/bootstrap/scripts/neovim.sh
$bash ~/dotfiles/bootstrap/scripts/terms.sh
$bash ~/dotfiles/bootstrap/scripts/fzf-bibtex.sh
$bash ~/dotfiles/bootstrap/scripts/go.sh
$bash ~/dotfiles/bootstrap/scripts/csl.sh

## Use Fish
sudo dscl . -create /Users/$USER UserShell /usr/local/bin/fish

echo ""

echo "Run $(tput bold)$(tput setaf 6)bash ~/dotfiles/dns/bootstrap.sh$(tput sgr0) to install DNS proxy and local dev domains."

echo ""

echo "Run $(tput bold)$(tput setaf 6)fish ~/dotfiles/mutt/make-gpg-keys.fish$(tput sgr0) to create GPG keys for NeoMutt (make sure Firefox & Bitwarden are working first)!"

echo ""

echo "In Seafile, sync $(tput bold)Mackup$(tput sgr0) and when $(tput bold)$(tput setaf 5)done syncing$(tput sgr0), run $(tput setaf 6)mackup restore$(tput sgr0) to load configuration files."

echo ""
