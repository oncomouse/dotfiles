#!/bin/bash

case "$1" in
  "up")
    if ponymix is-muted; then
      ponymix unmute
    else
      ponymix increase 5%
    fi
    ;;
  "down")
    if ponymix is-muted; then
      ponymix unmute
    else
      ponymix decrease 5%
    fi
    ;;
  "mute")
    ponymix toggle
    ;;
esac
