#!/usr/bin/env bash
rsync -av -e ssh --progress "andrew@192.168.1.48:/mnt/Media/Devices" "$HOME/Music"
