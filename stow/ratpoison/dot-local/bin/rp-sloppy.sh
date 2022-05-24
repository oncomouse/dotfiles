#!/usr/bin/env bash

if [ ! -d "$HOME/.local/bin" ]; then
	mkdir -p "$HOME/.local/bin"
fi

if [ ! -f "$HOME/.local/bin/sloppy" ]; then
	gcc -o "$HOME/.local/bin/sloppy" /usr/share/ratpoison/sloppy.c -lX11
fi

exec sloppy
