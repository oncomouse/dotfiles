#!/usr/bin/env bash
## Install Ruby Modules
if which gem > /dev/null 2>&1; then
  gemlist=$(gem list | cut -d " " -f 1)
  # os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
  function geminstall() {
    if ! [[ $gemlist =~ $1 ]]; then 
      gem install "$1"
    fi
  }
  if [ -z "$SERVER" ];then
    geminstall 'neovim'
    geminstall 'rubocop'
    geminstall 'rufo'
    geminstall 'standard'
  fi
  geminstall 'bundler'
  # Set gem path:
  bundle config set --local path "/home/andrew/.gem"
fi
