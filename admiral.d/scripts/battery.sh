#!/bin/bash
#Source: https://raw.githubusercontent.com/kbrgl/dotfiles/master/bin/battery.sh
status="$(cat /sys/class/power_supply/BAT0/status)"
capacity="$(cat /sys/class/power_supply/BAT0/capacity)"
color=false
echo -n "%{F#e5c078}"
if [[ $status == "Charging" ]]; then
    echo -en "\uf0e7"
elif [[ $capacity -ge  "85" ]]; then
    echo -en "\uf240"
elif [[ $capacity -ge "65" ]]; then
    echo -en "\uf241"
elif [[ $capacity -ge "45" ]]; then
    echo -en "\uf242"
elif [[ $capacity -ge "15" ]]; then
    echo -en "\uf243"
elif [[ $capacity -ge "0" ]]; then
    color=true
    echo -en "%{F#cc6666}\uf244"
fi
echo -n "%{F-}"

echo -n " $capacity%"

if [[ color ]]; then
    echo "%{F-}"
else
    echo
fi

