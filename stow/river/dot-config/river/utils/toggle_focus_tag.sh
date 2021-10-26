#!/usr/bin/env bash
tags=$1
riverctl toggle-focused-tags "$tags";
inplace="$(mktemp)"
cat /tmp/river_tag.json | jq 'if contains([$tags]) and length > 1 then . - [$tags] else . + [$tags] end | unique' > "$inplace"
mv "$inplace" /tmp/river_tag.json
killall -35 waybar
