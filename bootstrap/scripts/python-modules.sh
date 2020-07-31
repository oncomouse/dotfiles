#!/usr/bin/env bash
## Install Python Modules
pip3list=`pip3 list | cut -d " " -f 1 | sed 1,2d`
os=`bash ~/dotfiles/bootstrap/scripts/os.sh`
function pip3install() {
  local repo=$1
  if ! [[ $pip3list =~ $repo ]]; then 
    pip3 install $1
  fi
}
if [ $os == 'macos' ]; then
  pip3install 'mackup'
else
  pip3install 'thefuck'
fi
if [ -z $SERVER ];then
  pip3install 'neovim'
  # Vim Linters:
  pip3install 'vim-vint'
  # Python Linters:
  pip3install 'jedi'
  pip3install 'pylint'
  pip3install 'bandit'
  # Yaml Linters:
  pip3install 'yamllint'
  # Markdown Linters:
  pip3install 'proselint'
  ln -sf ~/dotfiles/proselint ~/.config/
fi
pip3install 'pynvim'
pip3install 'virtualfish'
