#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
# Install VSCode LSPs:
platform="linux-x64"
if [[ $os == "mac" ]]; then
  platform="darwin-universal"
fi
curl -sLo vscode.tar.gz https://update.code.visualstudio.com/latest/"$platform"/stable
rm -rf vscode
mkdir vscode
tar -xzf vscode.tar.gz -C vscode --strip-components 1
rm vscode.tar.gz
dir=~/.local/share/vscode-ls
[ -d $dir ] && rm -rf $dir
mkdir -p $dir
mv vscode/resources/app/extensions/node_modules "$dir"
mv vscode/resources/app/extensions/html-language-features "$dir"
mv vscode/resources/app/extensions/css-language-features "$dir"
mv vscode/resources/app/extensions/json-language-features "$dir"
rm -rf vscode
mkdir -p ~/.local/bin
cat <<EOF > ~/.local/bin/json-languageserver
#!/usr/bin/env bash
node $dir/json-language-features/server/dist/node/jsonServerMain.js \$*
EOF
chmod +x ~/.local/bin/json-languageserver
cat <<EOF > ~/.local/bin/html-languageserver
#!/usr/bin/env bash
node $dir/html-language-features/server/dist/node/htmlServerMain.js \$*
EOF
chmod +x ~/.local/bin/html-languageserver
cat <<EOF > ~/.local/bin/css-languageserver
#!/usr/bin/env bash
node $dir/css-language-features/server/dist/node/cssServerMain.js \$*
EOF
chmod +x ~/.local/bin/css-languageserver

# Install Sumneko LSP:
platform="Linux"
if [[ $os == "mac" ]]; then
  platform="macOS"
fi
version="1.19.0"
url="https://github.com/sumneko/vscode-lua/releases/download/v$version/lua-$version.vsix"
asset="vscode-lua.vsix"

dir=~/.local/share/sumneko-lua-language-server-"$version"
if [[ ! -d $dir ]]; then
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
  chmod +x ~/.local/bin/sumneko-lua-language-server
fi
