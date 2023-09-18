#!/usr/bin/env bash
## Install Node.js Modules
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
# Setup a local to user global npm path:
function npminstall() {
	for repo in "$@"; do
		if ! [[ $npmlist =~ $repo ]]; then
			npm install -g "$repo"
		fi
	done
}

mkdir -p "${HOME}/.npm-packages/lib"
npm config set prefix "${HOME}/.npm-packages"
if which npm >/dev/null 2>&1; then
	if [ -z "$SERVER" ]; then
		npmlist=$(npm list -g)
		if [ "$os" != "macos" ] && [ "$os" != "arch" ]; then
			npminstall jsonlint prettier vscode-langservers-extracted standard diff-so-fancy trash-cli
		fi
		if [ "$os" != "arch" ]; then
			npminstall vim-language-server
		fi
		npminstall mf-cli empty-trash-cli  neovim 
		if [ "$os" == "macos" ]; then
			npminstall eslint_d htmlhint
		fi
	fi
	if [ "$os" != "macos" ] && [ "$os" != "arch" ]; then
		npminstall '@bitwarden/cli'
	fi
fi
