#!/usr/bin/env bash
ocd="$(pwd)"
cd ~/dotfiles || exit
make st
cd "$ocd" || exit
