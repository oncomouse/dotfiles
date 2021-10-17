#!/usr/bin/env bash

case $BUTTON in
	1) liskin-media play && liskin-media status ;;
	2) liskin-media prev && liskin-media status ;;
	3) liskin-media next && liskin-media status ;;
	*) liskin-media status
esac
