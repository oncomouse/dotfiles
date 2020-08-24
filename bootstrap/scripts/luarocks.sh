#!/usr/bin/env bash
## Install Lua Rocks
lualist=$(luarocks list --porcelain | cut -f 1)
# os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
function luainstall() {
  local repo=$1
  if ! [[ $lualist =~ $repo ]]; then 
    luarocks install $repo
  fi
}
if [ -z "$SERVER" ];then
  luainstall 'luacheck'
fi
