#!/usr/bin/env bash
## Install Node.js Modules
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
    npminstall 'semistandard'
    npminstall 'snazzy'
    npminstall 'prettier'
    npminstall 'js-beautify'
    npminstall 'htmlhint'
    npminstall 'vscode-langservers-extracted'
    npminstall 'typescript-language-server'
    npminstall 'vim-language-server'
    npminstall '@bitwarden/cli'
  fi
fi
