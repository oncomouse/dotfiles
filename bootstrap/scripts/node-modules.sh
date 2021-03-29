#!/usr/bin/env bash
## Install Node.js Modules
# Setup a local to user global npm path:
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
mkdir -p "${HOME}/.npm-packages/lib"
npm config set prefix "${HOME}/.npm-packages"
if which npm > /dev/null 2>&1; then
  if [ -z "$SERVER" ]; then 
    npmlist=$(npm list -g)
    function npminstall() {
      local repo=$1
      if ! [[ $npmlist =~ $repo ]]; then 
        npm install -g "$1"
      fi
    }
    npminstall 'mf-cli' 
    npminstall 'trash-cli' 
    npminstall 'empty-trash-cli'
    npminstall 'diff-so-fancy' 
    npminstall 'neovim' 
    npminstall 'jsonlint' 
    npminstall 'prettier'
    npminstall 'prettier-semi-cli'
    npminstall 'js-beautify'
    npminstall 'htmlhint'
    # Required by typescript-language-server:
    npminstall 'typescript'
    npminstall 'typescript-language-server'
    npminstall 'vim-language-server'
    npminstall 'bash-language-server'
  fi
  npminstall '@bitwarden/cli'
fi
