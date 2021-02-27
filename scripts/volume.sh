#!/bin/bash

vol_change=${2:-5%}
case "$1" in
  "up")
    if ponymix is-muted; then
      ponymix unmute
    else
      ponymix increase "$vol_change"
    fi
    ;;
  "down")
    if ponymix is-muted; then
      ponymix unmute
    else
      ponymix decrease "$vol_change"
    fi
    ;;
  "mute")
    ponymix toggle > /dev/null
    if ponymix is-muted; then
      echo "x"
    else
      echo "$(ponymix get-volume)"
    fi
    ;;
  *)
    if ponymix is-muted; then
      echo "x"
    else
      ponymix get-volume
    fi
    ;;
esac
