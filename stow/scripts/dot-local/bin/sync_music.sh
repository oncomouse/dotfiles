#!/usr/bin/env bash
rsync -akv -e ssh --progress "andrew@192.168.1.48:/mnt/Media/Devices" "$HOME/Music"
