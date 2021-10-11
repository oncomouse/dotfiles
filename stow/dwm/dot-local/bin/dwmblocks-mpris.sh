#!/usr/bin/env bash

case $BUTTON in
	1) liskin-media play ;;
	2) liskin-media prev ;;
	3) liskin-media next ;;
	*) liskin-media status
esac
