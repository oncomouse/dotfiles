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
dotfiles_stow bash
dotfiles_stow fish
dotfiles_stow git
dotfiles_stow ripgrep
dotfiles_stow scripts
dotfiles_stow starship
dotfiles_stow neovim-server
if [ "$os" == "arch" ]; then
	dotfiles_stow systemd
fi
if [ -z "$SERVER" ]; then
	dotfiles_stow doom
	dotfiles_stow mopidy
	dotfiles_stow mpd
	dotfiles_stow ncmpcpp
	dotfiles_stow pandoc
	dotfiles_stow rubocop
	dotfiles_stow starship
	dotfiles_stow selene
	if [ "$DOTFILES_TARGET" = "desktop" ]; then
		dotfiles_stow tmux
	else
		dotfiles_stow tmux-server
	fi
	dotfiles_stow tridactyl
	dotfiles_stow firefox
	dotfiles_stow wal
	if [ "$os" == "macos" ]; then
		dotfiles_stow homebrew
		dotfiles_stow karabiner
		dotfiles_stow mackup
		dotfiles_stow wezterm
	elif [ "$os" == "arch" ]; then
		# Other setup files:
		dotfiles_stow fontconfig
		dotfiles_stow gtk
		dotfiles_stow mpd
		dotfiles_stow no-xfce4
		dotfiles_stow mpv
		dotfiles_stow ncmpcpp
		dotfiles_stow picom
		dotfiles_stow polybar
		dotfiles_stow pulse
		dotfiles_stow rofi
		if [ "$DOTFILES_TARGET" = "settop" ]; then
			dotfiles_stow settop
		else
			dotfiles_stow dunst
			dotfiles_stow redshift
			dotfiles_stow sdorfehs
			dotfiles_stow xscreensaver
		fi
		dotfiles_stow st
		dotfiles_stow xorg
		dotfiles_stow zathura
	fi
else
	dotfiles_stow tmux-server
fi
