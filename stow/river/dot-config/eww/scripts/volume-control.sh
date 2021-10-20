#!/usr/bin/env bash

cmd=$1

if [ $cmd == "up" ]
then
	pamixer -i "2"
else
	pamixer -d "2"
fi

