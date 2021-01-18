#!/bin/bash
dex -a
xsetroot -bg "#$(xrdb -query | grep color7 | head -1 | cut -d "#" -f2)"
aslstatus | xsetroot &
