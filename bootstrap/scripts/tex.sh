#!/usr/bin/env bash
# Install the TeX plugins needed to compile the CV:
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
if [ "$os" = "arch" ];then
  tllocalmgr install titling titlesec enumitem ifmtarg xifthen varwidth tabu
  texhash
else
  sudo tlmgr install titling titlesec enumitem ifmtarg xifthen varwidth tabu
fi
