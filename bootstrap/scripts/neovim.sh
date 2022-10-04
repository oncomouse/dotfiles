#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
if [ "$os" == "arch" ]; then
	~/dotfiles/stow/scripts/dot-local/bin/nvim-linux.sh
	systemctl --user start neovim-nightly.timer
elif [ "$os" == "macos" ]; then
	~/dotfiles/stow/scripts/dot-local/bin/nvim-macos.sh
fi
