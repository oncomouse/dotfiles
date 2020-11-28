#!/usr/bin/env bash

bash="$(which bash)"

$bash ~/dotfiles/bootstrap/scripts/minimal-config.sh
$bash ~/dotfiles/bootstrap/scripts/vim.sh
$bash ~/dotfiles/bootstrap/scripts/tmux.sh
$bash ~/dotfiles/bootstrap/scripts/terms.sh
$bash ~/dotfiles/bootstrap/scripts/diff-so-fancy.sh
$bash ~/dotfiles/bootstrap/scripts/node-modules.sh
$bash ~/dotfiles/bootstrap/scripts/python-modules.sh
$bash ~/dotfiles/bootstrap/scripts/ruby-gems.sh
$bash ~/dotfiles/bootstrap/scripts/fisher.sh
$bash ~/dotfiles/bootstrap/scripts/rustup.sh
if [ -z "$SERVER" ]; then
  $bash ~/dotfiles/bootstrap/scripts/vale.sh
  $bash ~/dotfiles/bootstrap/scripts/asdf.sh
  $bash ~/dotfiles/bootstrap/scripts/neovim.sh
  $bash ~/dotfiles/bootstrap/scripts/haskell.sh
  $bash ~/dotfiles/bootstrap/scripts/csl.sh
  $bash ~/dotfiles/bootstrap/scripts/tex.sh
fi
