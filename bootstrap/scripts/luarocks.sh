#!/usr/bin/env bash
## Install Lua Rocks
lualist=$(luarocks list --porcelain | cut -f 1)
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
function luainstall() {
	local repo=$1
	local version=$2-5.4
	if ! [[ $lualist =~ $repo ]]; then 
		sudo luarocks --lua-version "$version" install "$repo"
	fi
}
if [ -z "$SERVER" ];then
	luainstall 'luacheck'
	if [[ $os == "arch" ]]; then
		# Awesome:
		luainstall 'lgi' '5.3'
		luainstall 'penlight' '5.3'
		luainstall 'ldoc' '5.3'
	fi
fi

