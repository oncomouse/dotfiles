#!/usr/bin/env bash

# Set app-ids of views which should float
riverctl float-filter-add app-id "float"
riverctl float-filter-add app-id "popup"
riverctl float-filter-add app-id "pop-up"
riverctl float-filter-add app-id "mpv"

# Set app-ids of views which should use client side decorations
# riverctl csd-filter-add "gedit"
