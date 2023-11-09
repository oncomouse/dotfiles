#!/usr/bin/env bash
if [ ! -d ~/.config/emacs ]; then
	git clone https://github.com/hlissner/doom-emacs ~/.config/emacs
fi
~/.config/emacs/bin/doom sync
~/.config/emacs/bin/doom env
emacs --batch -f nerd-icons-install-fonts
