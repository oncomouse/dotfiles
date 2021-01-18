#!/bin/sh

case $BUTTON in
  1) playerctl play-pause ;;
  2) playerctl previous ;;
  3) playerctl next ;;
esac
