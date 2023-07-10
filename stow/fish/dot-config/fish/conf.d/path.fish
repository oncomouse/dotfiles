if status --is-login
	fish_add_path --prepend /opt/homebrew/bin
	# Custom paths:
	fish_add_path ~/bin
	fish_add_path ~/.local/bin
	# Set up Cargo:
	fish_add_path ~/.cargo/bin
	# Set up FZF (if local):
	fish_add_path ~/.fzf/bin
	# Set up Poetry:
	fish_add_path ~/.poetry/bin
	# Set up ASDF:
	fish_add_path --prepend ~/.asdf/shims
	fish_add_path --prepend /usr/local/opt/asdf/bin
	fish_add_path --prepend /opt/homebrew/opt/asdf/bin
	fish_add_path --prepend ~/.asdf/bin
	# Sbin:
	fish_add_path /usr/local/sbin
	fish_add_path /opt/homebrew/sbin
	fish_add_path ~/.ghcup/bin
	# NPM Local:
	fish_add_path ~/.npm-packages/bin
	# Luarocks:
	fish_add_path ~/.luarocks/bin
	# Gems:
	which ruby > /dev/null && fish_add_path --append (ruby -e 'puts Gem.user_dir')/bin
	which gem > /dev/null && fish_add_path --append (gem environment gemdir)/bin
	# Lua5.3 for macOS:
	fish_add_path /usr/local/opt/lua@5.3/bin/
	fish_add_path /usr/local/opt/grep/libexec/gnubin/
	fish_add_path --prepend ~/.local/nvim-macos/bin/
	fish_add_path --prepend ~/.local/nvim-linux64/bin/
	fish_add_path --append /usr/local/bin
	# Homebrew's Annoying Ruby Install:
	fish_add_path --append /usr/local/opt/ruby/bin
	fish_add_path --append /opt/homebrew/opt/ruby/bin
	# Local luarocks
	fish_add_path --prepend ~/.luarocks/bin
	# Doom Emacs installer:
	fish_add_path --append ~/.config/emacs/bin


	# NPM Local manpath:
	set -q MANPATH || set MANPATH ''
	set -gx MANPATH $MANPATH ~/.npm-packages/share/man

	# Set Python stub location:
	set -gx MYPYPATH ~/dotfiles/conf/python-stubs
	# Set Luarocks PATH
	# if which lua5.3 > /dev/null
	#		for i in (luarocks --lua-version 5.3 path | awk '{sub(/PATH=/, "PATH ", $2); print "set -gx "$2}'); eval $i; end
	# end
end

