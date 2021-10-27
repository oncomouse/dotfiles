#!/usr/bin/env bash

function kile_layout {
	local layout
	layout="$1"
	if [[ "$(pgrep -c kile)" == 0 ]]; then
		riverctl spawn "kile -l '$layout'" && riverctl default-layout kile
	else
		riverctl send-layout-cmd kile "focused $layout"
	fi
}
