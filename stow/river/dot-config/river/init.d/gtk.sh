#!/bin/bash
gnome_schema="org.gnome.desktop.interface"

gsettings set "$gnome_schema" gtk-theme "oomox-xresources-reverse"
gsettings set "$gnome_schema" icon-theme "oomox-xresources-reverse"
gsettings set "$gnome_schema" cursor-theme "Adwaita"
gsettings set "$gnome_schema" font-name "Liberation Sans 10"
