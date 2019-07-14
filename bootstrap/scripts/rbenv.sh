#!/usr/bin/env bash
## Setup Ruby
if test ! "$(rbenv root)/plugins" ; then
  mkdir -p "$(rbenv root)"/plugins
  git clone https://github.com/rbenv/ruby-build.git "$(rbenv root)"/plugins/ruby-build
  git clone https://github.com/momo-lab/rbenv-install-latest.git "$(rbenv root)"/plugins/rbenv-install-latest
  rbenv install -s 2.5.1 # Dreamhost Ruby
  rbenv install-latest
  rbenv global "$(rbenv versions | sed -e '$!d' -e 's/^[ \t]*//')"
fi
