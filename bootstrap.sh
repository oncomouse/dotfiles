#!/usr/bin/env bash

## Get ready for xcode/brew:
xcode-select --install

if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew bundle install

## Diff-so-fancy Git stuff
git config --global color.diff-highlight.oldNormal "red bold"
git config --global color.diff-highlight.oldHighlight "red bold 52"
git config --global color.diff-highlight.newNormal "green bold"
git config --global color.diff-highlight.newHighlight "green bold 22"

## Git Keychain:
git config --global credential.helper osxkeychain 

## Install Python Modules
pip3 install mackup neovim

## Setup Ruby
mkdir -p "$(rbenv root)"/plugins
git clone https://github.com/rbenv/ruby-build.git "$(rbenv root)"/plugins/ruby-build
git clone https://github.com/momo-lab/rbenv-install-latest.git "$(rbenv root)"/plugins/rbenv-install-latest
rbenv install 2.5.1 # Dreamhost Ruby
rbenv install-latest
rbenv global "$(rbenv versions | sed -e '$!d' -e 's/^[ \t]*//')"

## Setup Oh My Tmux! (.tmux.conf.local will come from mackup)
git clone https://github.com/gpakosz/.tmux ~/.tmux
ln -s -f ~/.tmux/.tmux.conf ~/.tmux.conf

## Setup $TERM
tic -x ./tmux-256color.terminfo

## Use Fish
sudo dscl . -create /Users/$USER UserShell /usr/local/bin/fish

echo "When Dropbox is configured and $(tput bold)$(tput setaf 5)done syncing$(tput sgr0), run $(tput setaf 6)mackup restore$(tput sgr0) to load configuration files."
