#!/usr/bin/env bash
# Sloppy Focus
riverctl focus-follows-cursor normal

# Borders
source ~/.cache/wal/colors.sh
riverctl border-color-focused "${color6//#/0x}"
riverctl border-color-unfocused "${color0//#/0x}"
riverctl border-width 1
riverctl background-color "${color0//#/0x}"
