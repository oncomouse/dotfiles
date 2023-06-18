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
if [ "$os" = "macos" ];then
  sudo chown -R "$(whoami)" "$(brew --prefix)"/texlive
elif [ "$os" = "arch" ]; then
  sudo mkdir -p /usr/share/tlpkg/backups
  sudo mkdir -p /usr/share/tlpkg/tlpobj
  sudo tlmgr option repository http://mirrors.rit.edu/CTAN/systems/texlive/tlnet
fi
sudo tlmgr install "${packages[@]}"
