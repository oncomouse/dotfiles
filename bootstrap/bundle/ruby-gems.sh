#!/usr/bin/env bash

if which gem > /dev/null 2>&1; then
  gemlist=`gem list | cut -d " " -f 1`
  os=`bash ~/dotfiles/bootstrap/scripts/os.sh`
  function geminstall() {
    local repo=$1
    if ! [[ $gemlist =~ $repo ]]; then 
      gem install $repo
    fi
  }
  if [ -z $SERVER ];then
    geminstall 'solargraph'
    geminstall 'rubocop'
  fi
  geminstall 'bundler'
fi
