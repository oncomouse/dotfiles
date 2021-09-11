#!/usr/bin/env bash

STATUS="/tmp/dwmblocks.mpris"

case $BUTTON in
	1) liskin-media play ;;
	2) liskin-media prev ;;
	3) liskin-media next ;;
	*) liskin-media status
esac
