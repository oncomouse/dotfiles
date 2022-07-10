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
dotfiles_stow neovim
dotfiles_stow vim
dotfiles_stow tmux
dotfiles_stow ripgrep
if [ -z "$SERVER" ]; then
	dotfiles_stow wal
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
		dotfiles_stow dunst
		dotfiles_stow sdorfehs
		dotfiles_stow xdg
		dotfiles_stow rofi
		dotfiles_stow xorg
		dotfiles_stow gtk
		dotfiles_stow redshift
		dotfiles_stow xscreensaver
		dotfiles_stow systemd
		dotfiles_stow unclutter
		dotfiles_stow mpv
		dotfiles_stow mpd
		dotfiles_stow ncmpcpp
		dotfiles_stow pulse
	fi
else
	dotfiles_stow neovim-server
fi
