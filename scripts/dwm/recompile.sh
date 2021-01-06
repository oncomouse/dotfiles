#!/bin/bash

oldwd=$(pwd)
cd ~/Projects/dwm || exit
git checkout master --force
git checkout build; make; sudo make install; make clean; rm dwm-msg.o; git checkout master
cd "$oldwd" || exit
