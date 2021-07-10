#!/usr/bin/env bash

mkdir -p ~/Music
mkdir -p ~/.local/share/mpd
mkdir -p ~/.local/share/mpd/playlists
systemctl --user enable mpd.service

ln -s /usr/share/applications/mpdris2.desktop ~/.config/autostart/
