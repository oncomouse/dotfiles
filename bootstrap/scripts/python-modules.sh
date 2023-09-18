#!/usr/bin/env bash
## Install Python Modules
pip3list=$(pip3 list | cut -d " " -f 1 | sed 1,2d)
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)

function pip3install() {
	for repo in "$@"
	do
		if ! [[ $pip3list =~ $repo ]]; then
			pip3 install "$repo"
		fi
	done
}

if [ "$os" != "macos" ] && [ "$os" != "arch" ]; then
	if [ -z "$SERVER" ]; then
		pip3install vim-vint flake8 black yamllint reorder-python-imports
	fi
	# Install poetry
	curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python3
	pip3install virtualfish
fi

if [ -z "$SERVER" ]; then
	if [ "$os" != "arch" ]; then
		pip3install colorama pywal
	fi
	pip3install bibparse citation-langserver
fi
if [ "$os" != "arch" ]; then
	pip3install pynvim
fi
