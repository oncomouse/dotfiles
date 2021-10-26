#!/usr/bin/env bash

layout=$1
tag="$(cat /tmp/river_tag.json | jq ".[0]")"

inplace="$(mktemp)"
cat /tmp/river_layout.json | jq ".[$tag]=\"$layout\"" > "$inplace"; mv "$inplace" /tmp/river_layout.json
killall -35 waybar
