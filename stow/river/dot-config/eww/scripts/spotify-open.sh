#!/usr/bin/env bash
if pgrep -x spotify; then
  # running
  exit 0
else
  # open it
  flatpak run com.spotify.Client & disown
fi
