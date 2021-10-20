#!/usr/bin/env bash
cmd=$1

if [ $cmd == "up" ]
then
    playerctl -p spotify previous
else
    playerctl -p spotify next
fi
