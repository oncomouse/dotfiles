#!/usr/bin/env bash
## Install CSL support:
if test ! "~/.csl" ; then
  git clone https://github.com/citation-style-language/styles ~/.csl
fi
