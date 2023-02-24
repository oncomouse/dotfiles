#!/usr/bin/env bash
# Install the TeX plugins needed to compile the CV:
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
packages=(
	enumitem
	ifmtarg
	libertine
	ltablex
	sectsty
	soul
	titlesec
	titling
	varwidth
	xifthen
)
if [ "$os" = "arch" ];then
  tllocalmgr install "${packages[@]}"
  sudo texhash
else
  sudo tlmgr install "${packages[@]}"
fi
