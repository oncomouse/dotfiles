#!/usr/bin/env bash
## Install Node.js Modules
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
# Setup a local to user global npm path:
function npminstall() {
  local repo=$1
  if ! [[ $npmlist =~ $repo ]]; then 
    npm install -g "$1"
  fi
}
mkdir -p "${HOME}/.npm-packages/lib"
npm config set prefix "${HOME}/.npm-packages"
if which npm > /dev/null 2>&1; then
  if [ -z "$SERVER" ]; then 
    npmlist=$(npm list -g)
    npminstall 'mf-cli' 
    npminstall 'trash-cli' 
    npminstall 'empty-trash-cli'
    npminstall 'diff-so-fancy' 
    npminstall 'neovim' 
    npminstall 'jsonlint' 
    npminstall 'prettier'
    npminstall 'vim-language-server'
    npminstall 'vscode-langservers-extracted'
    if [ "$os" = "macos" ]; then
      npminstall 'typescript-language-server'
      npminstall 'htmlhint'
    fi
  fi
  npminstall '@bitwarden/cli'
fi
