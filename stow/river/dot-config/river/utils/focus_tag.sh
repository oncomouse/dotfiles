#!/usr/bin/env bash
tags=$1
riverctl set-focused-tags "$tags"
echo "[$tags]" > /tmp/river_tag.json
killall -35 waybar
