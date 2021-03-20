#!/usr/bin/env bash
## Install Lua Rocks
lualist=$(luarocks list --porcelain | cut -f 1)
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
function luainstall() {
  local repo=$1
  # local version="${2:5.4}"
  local version="5.4"
  if ! [[ $lualist =~ $repo ]]; then 
    sudo luarocks --lua-version "$version" install "$repo"
  fi
}
if [ -z "$SERVER" ];then
  luainstall 'luacheck'
  # if [[ $os == "arch" ]]; then
  #   luainstall 'lgi' '5.3'
  #   luainstall 'penlight' '5.3'
  #   luainstall 'ldoc' '5.3'
  # fi
  # Install sumneko lua-server
  platform="Linux"
  if [[ $os == "mac" ]]; then
    platform="macOS"
  fi
  version="1.12.1"
  url="https://github.com/sumneko/vscode-lua/releases/download/v$version/lua-$version.vsix"
  asset="vscode-lua.vsix"

  dir=~/.local/share/sumneko-lua-language-server-"$version"
  mkdir -p "$dir"
  curl -L "$url" -o "$dir/$asset"
  unzip "$dir/$asset" -d "$dir"
  rm "$dir/$asset"
  chmod +x "$dir/extension/server/bin/$platform/lua-language-server"
  mkdir -p ~/.local/bin
  cat <<EOF > ~/.local/bin/sumneko-lua-language-server
#!/usr/bin/env bash
DIR="$dir/extension/server"
\$DIR/bin/$platform/lua-language-server -E -e LANG=en \$DIR/main.lua \$*
# vim:ft=bash
EOF
fi

