#!/bin/bash
# Source: https://raw.githubusercontent.com/kbrgl/dotfiles/master/bin/network_name.sh
nname=$(iwgetid -r)
echo -en "%{A:connman-gtk:}%{F#7d8fa3}\uf1eb%{F-} "
if [[ $nname != "" ]]; then
  echo -n "$nname"
else
  echo -n "Not connected"
fi
echo "%{A}"
