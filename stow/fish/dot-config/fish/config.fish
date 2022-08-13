if status is-interactive
	# Setup Pywal colors:
	if test -d $HOME/.cache/wal
		source ~/.cache/wal/colors.fish
	end

	# EDITOR
	set -gx EDITOR (which nvim)

	# Tell sxhkd to not use fish:
	set -gx SXHKD_SHELL sh

	# NNN Theme
	set -gx NNN_FCOLORS "0603040200050E070D09abc4"
	if test -e $HOME/.ow_credentials.json
		set -gx OW_KEY (cat ~/.ow_credentials.json | jq -r .key)
		set -gx OW_LAT (cat ~/.ow_credentials.json | jq .coordinates[0])
		set -gx OW_LONG (cat ~/.ow_credentials.json | jq .coordinates[1])
	end

	# DOTFILES_TARGET
	if test -e $HOME/.local/share/dotfiles/target
		set -gx DOTFILES_TARGET (/bin/cat $HOME/.local/share/dotfiles/target)
	end

	# Setup FZF themes:
	set -gx FZF_DEFAULT_OPTS "--ansi --bind='ctrl-o:execute(open {})+abort'"
	set -gx FZF_DEFAULT_COMMAND "fd -t f --follow --hidden"
	set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
	set -gx FZF_ALT_C_COMMAND "fd --type d --follow"
	if test -d "$HOME/.cache/wal"
		set -gx FZF_DEFAULT_OPTS "$FZF_DEFAULT_OPTS --color=bg+:$color8,bg:$background,spinner:$color14,hl:$color12"\
		"--color=fg:$foreground,header:$color12,info:$background,pointer:$color14"\
		"--color=marker:$color14,fg+:$foreground,prompt:$color3,hl+:$color12"
	end
	set -gx FZF_CTRL_T_OPTS "--preview-window 'right:60%' --preview 'bat --theme=wal --color=always --style=header,grid --line-range :300 {}'"
	set -gx FZF_ALT_C_OPTS "--preview 'ls --color=always {} | head -200'"

	# XDG Stuff:
	set -gx XDG_DATA_DIRS ~/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:/usr/local/share:/usr/share

	# Configure ASDF:
	# if not contains $HOME/.asdf/shims $fish_user_paths
	#		and test (uname) = "Darwin"
	#		echo "Loading ASDF"
	#		set -Ux ASDF_DIR (brew --prefix asdf)
	#		set -l asdf_data_dir (
	#			if test -n "$ASDF_DATA_DIR"; echo $ASDF_DATA_DIR;
	#			else; echo $HOME/.asdf; end)

	#		# Add asdf to PATH
	#		set -l asdf_bin_dirs $ASDF_DIR/bin $ASDF_DIR/shims $asdf_data_dir/shims
	#		for x in $asdf_bin_dirs
	#			if test -d $x
	#				and not contains $x $fish_user_paths
	#				fish_add_path $x
	#			end
	#		end
	# end

	# Load the asdf wrapper function
	source $ASDF_DIR/lib/asdf.fish

	# Configure Pisces (fish pairing):
	set -gx pisces_only_insert_at_eol 1
	#
	# Setup Kitty:
	if command -sq kitty
		kitty + complete setup fish | source
	end

	# Colors:
	set fish_color_cwd cyan
end
