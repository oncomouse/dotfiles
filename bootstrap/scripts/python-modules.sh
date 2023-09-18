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

# Use package manager wherever possible:
if [ "$os" == 'macos' ]; then
	if [ -z "$SERVER" ]; then
		brew install flake8 black vint yamllint mackup reorder-python-imports
	fi
	brew install poetry virtualfish
elif [ "$os" == "arch" ]; then
	if [ -z "$SERVER" ]; then
		sudo pacman -S --noconfirm vint flake8 python-black yamllint python-pywal python-colorama
		paru -S --noconfirm python-reorder-python-imports
	fi
	sudo pacman -S --noconfirm python-poetry python-pynvim
	paru -S --noconfirm virtualfish
else
	if [ -z "$SERVER" ]; then
		pip3install vim-vint flake8 black yamllint reorder-python-imports
	fi
	# Install poetry
	curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python3
	pip3install virtualfish
fi

# General installers
if [ -z "$SERVER" ]; then
	if [ "$os" != "arch" ]; then
		pip3install colorama pywal
	fi
	pip3install bibparse citation-langserver
fi
if [ "$os" != "arch" ]; then
	pip3install pynvim
fi
