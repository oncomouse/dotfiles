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
if [ "$os" != "arch" ];then
  sudo chown -R "$(whoami)" "$(brew --prefix)"/texlive
  sudo tlmgr install "${packages[@]}"
fi
