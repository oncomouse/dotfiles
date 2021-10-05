#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
dotfiles_stow () {
	if command -v python3&>/dev/null;then
		python=python3
	else
		python=python
	fi
	/usr/bin/env $python "$HOME/dotfiles/scripts/stow.py" -d "$HOME/dotfiles/stow" -t "$HOME" --no-folding --dotfiles --overwrite "$1"
}
# Basic Setup:
mkdir -p ~/.config/fish
dotfiles_stow fish
dotfiles_stow bash
dotfiles_stow wal
if [ -z "$SERVER" ]; then
	dotfiles_stow kitty
	dotfiles_stow neovim
	dotfiles_stow proselint
	dotfiles_stow rubocop
	dotfiles_stow vale
	dotfiles_stow mpd
	if [ "$os" == "macos" ]; then
		dotfiles_stow homebrew
		dotfiles_stow mackup
		dotfiles_stow karabiner
	elif [ "$os" == "arch" ]; then
		# Other setup files:
		dotfiles_stow dwm
		dotfiles_stow picom
		dotfiles_stow xdg
		dotfiles_stow rofi
		dotfiles_stow xorg
		dotfiles_stow gtk-3.0
		dotfiles_stow gtk-2.0
		dotfiles_stow redshift
		dotfiles_stow xscreensaver
		dotfiles_stow systemd
		dotfiles_stow unclutter
		dotfiles_stow mpDris2
		dotfiles_stow mpv
		dotfiles_stow networkmanager-dmenu
		dotfiles_stow pulse
	fi
	## Configure NCSpot:
	if [ "$os" == "macos" ]; then
		mkdir -p ~/Library/Preferences/org.affekt.ncspot/
		dotfiles_stow ncspot-macos
	elif [ -z "$SERVER" ]; then
		mkdir -p ~/.config/ncspot
		dotfiles_stow ncspot-linux
	fi
else
	dotfiles_stow neovim-server
fi
dotfiles_stow vim
dotfiles_stow tmux
dotfiles_stow ripgrep
