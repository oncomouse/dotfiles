#!/usr/bin/env bash
## Install Lua Rocks
lualist=$(luarocks list --porcelain | cut -f 1)
# os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
function luainstall() {
	local repo=$1
	local version=5.4
	if ! [[ $lualist =~ $repo ]]; then 
		sudo luarocks --lua-version "$version" install "$repo"
	fi
}
# Install lpeg-bibtex for Neovim:
luarocks install --dev --lua-version=5.1 lpeg-bibtex --local
# if [ -z "$SERVER" ];then
	# luainstall 'luacheck'
	# if [[ $os == "arch" ]]; then
	# 	# Awesome:
	# 	sudo luarocks --lua-version 5.3 install "lgi"
	# 	sudo luarocks --lua-version 5.3 install "penlight"
	# 	sudo luarocks --lua-version 5.3 install "ldoc"
	# fi
# fi

