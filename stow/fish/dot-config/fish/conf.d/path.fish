if status --is-login
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
	fish_add_path ~/.asdf/shims
	fish_add_path /usr/local/opt/asdf/bin
	fish_add_path ~/.asdf/bin
	# Sbin:
	fish_add_path /usr/local/sbin
	fish_add_path ~/.ghcup/bin
	# NPM Local:
	fish_add_path ~/.npm-packages/bin
	# Luarocks:
	fish_add_path ~/.luarocks/bin
	# Gems:
	fish_add_path (ruby -e 'puts Gem.user_dir')/bin

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

