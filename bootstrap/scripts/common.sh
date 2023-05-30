#!/usr/bin/env bash

os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)

~/dotfiles/bootstrap/scripts/stow.sh
~/dotfiles/bootstrap/scripts/git.sh
~/dotfiles/bootstrap/scripts/diff-so-fancy.sh
~/dotfiles/bootstrap/scripts/luarocks.sh
~/dotfiles/bootstrap/scripts/node-modules.sh
~/dotfiles/bootstrap/scripts/python-modules.sh
~/dotfiles/bootstrap/scripts/fisher.sh
~/dotfiles/bootstrap/scripts/rustup.sh
~/dotfiles/bootstrap/scripts/tmux.sh
~/dotfiles/bootstrap/scripts/neovim.sh
if [ -z "$SERVER" ]; then
	~/dotfiles/bootstrap/scripts/theme.sh
	~/dotfiles/bootstrap/scripts/vale.sh
	~/dotfiles/bootstrap/scripts/firefox.sh
	~/dotfiles/bootstrap/scripts/asdf.sh
	~/dotfiles/bootstrap/scripts/csl.sh
	~/dotfiles/bootstrap/scripts/tex.sh
fi
~/dotfiles/bootstrap/scripts/ruby-gems.sh

if [ "$os" == "arch" ]; then
	if [ -z "$SERVER" ]; then
		~/dotfiles/bootstrap/scripts/compiled.sh
	fi
fi
