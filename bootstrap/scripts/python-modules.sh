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
fi
if [ -z "$SERVER" ];then
  pip3install 'neovim'
  # Vim Linters:
  pip3install 'vim-vint'
  # Python Linters:
  pip3install 'jedi'
  pip3install 'pylint'
  pip3install 'bandit'
  pip3install 'autopep8'
  pip3install 'reorder-python-imports'
  pip3install 'mypy'
  pip3install 'black'
  # Yaml Linters:
  pip3install 'yamllint'
  # Markdown Linters:
  pip3install 'proselint'
  ln -sf ~/dotfiles/proselint ~/.config/
  # BibTeX
  # pip3install 'bibtexparser'
  pip3install 'bibparse'
  # LSP
  pip3install 'jedi-language-server'
  pip3install 'citation-langserver'
  # Image support for Ranger:
  pip3install 'pillow'
  # Pywal:
  pip3install 'pywal'
fi
pip3install 'pynvim'
pip3install 'virtualfish'
pip3install 'ranger-fm'
# Install poetry
curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python3
