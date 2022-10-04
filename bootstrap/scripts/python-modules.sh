#!/usr/bin/env bash
## Install Python Modules
pip3list=$(pip3 list | cut -d " " -f 1 | sed 1,2d)
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
function pip3install() {
	local repo=$1
	if ! [[ $pip3list =~ $repo ]]; then 
		pip3 install "$1"
	fi
}
if [ "$os" == 'macos' ]; then
	pip3install 'mackup'
elif [ "$os" == "arch" ]; then
	if [ -z "$SERVER" ];then
		sudo pacman -S --noconfirm vint flake8 python-black yamllint python-pywal
	fi
	sudo pacman -S --noconfirm python-poetry python-pynvim
else
	if [ -z "$SERVER" ];then
		# Vim Linters:
		pip3install 'vim-vint'
		# Python Linters:
		pip3install 'flake8'
		pip3install 'black'
		# Yaml Linters:
		pip3install 'yamllint'
		# Pywal:
		pip3install 'pywal'
	fi
	# Install poetry
	curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python3
	pip3install 'pynvim'
fi
if [ -z "$SERVER" ]; then
	pip3install 'colorama'
	pip3install 'neovim'
	pip3install 'reorder-python-imports'
	pip3install 'bibparse'
	# LSP
	pip3install 'citation-langserver'
	pip3install 'pywalfox'
fi
pip3install 'virtualfish'
# pip3install 'ranger-fm'
# pip3install 'jedi'
# pip3install 'mypy'
# pip3install 'bandit'
# pip3install 'autopep8'
# Markdown Linters:
# pip3install 'proselint'
