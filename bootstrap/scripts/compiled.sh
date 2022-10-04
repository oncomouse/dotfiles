#!/usr/bin/env bash
ocd="$(pwd)"
cd ~/dotfiles || exit
make st sdorfehs
cd "$ocd" || exit
