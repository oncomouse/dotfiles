#!/bin/bash
gnome_schema="org.gnome.desktop.interface"

gsettings set "$gnome_schema" gtk-theme "Matcha-dark-pueril"
gsettings set "$gnome_schema" icon-theme "ePapirus-Dark"
gsettings set "$gnome_schema" cursor-theme "Adwaita"
gsettings set "$gnome_schema" font-name "LiterationMono Nerd Font 10"
