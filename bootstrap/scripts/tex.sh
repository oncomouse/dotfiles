#!/usr/bin/env bash
# Install the TeX plugins needed to compile the CV:
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
packages=(
	titling
	titlesec
	enumitem
	ifmtarg
	xifthen
	varwidth
	tabu
	libertine
	sectsty
)
if [ "$os" = "arch" ];then
  tllocalmgr install "${packages[@]}"
  sudo texhash
else
  sudo tlmgr install "${packages[@]}"
fi
