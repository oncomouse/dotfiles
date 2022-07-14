#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
if [ "$os" == "arch" ]; then
	nvim-linux.sh
	systemctl --user start neovim-nightly.timer
elif [ "$os" == "macos" ]; then
	nvim-macos.sh
fi
