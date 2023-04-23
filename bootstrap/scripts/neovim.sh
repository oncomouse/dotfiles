#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
~/dotfiles/stow/scripts/dot-local/bin/nvim-nightly.sh
if [ "$os" == "arch" ]; then
	systemctl --user start neovim-nightly.timer
fi
