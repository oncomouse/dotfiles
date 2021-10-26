#!/usr/bin/env bash

declare -A layoutSymbols
layoutSymbols["tile"]="[]="
layoutSymbols["rtile"]="=[]"
layoutSymbols["monocle"]="[M]"
layoutSymbols["centered_monocle"]="{C}"
layoutSymbols["float"]="><>"
layout=$1
tag="$(cat /tmp/river_tag.json | jq "add")"

inplace="$(mktemp)"
cat /tmp/river_layout.json | jq ".[$tag]=\"${layoutSymbols[$layout]}\"" > "$inplace"; mv "$inplace" /tmp/river_layout.json
killall -35 waybar
exec "$HOME/.config/river/$layout.sh"
