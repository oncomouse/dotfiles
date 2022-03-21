#!/usr/bin/env bash

case $BUTTON in
	1) dotfiles-media play ;;
	2) dotfiles-media prev ;;
	3) dotfiles-media next ;;
	*) dotfiles-media status
esac
