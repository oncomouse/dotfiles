#!/usr/bin/env sh
pkill stalonetray
sleep 0.1 && sdorfehs -c "gravity ne" &
exec stalonetray -bg "$(jq -r ".colors.color8" ~/.cache/wal/colors.json)" -geometry "4x1+0+20" --grow-gravity w
