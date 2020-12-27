#!/bin/sh
dwmstatus() {
  while true; do
    xsetroot -name "$(date +"%a %l:%M %p")"
    sleep 2
  done
}
pkill -f "dwmstatus"
dwmstatus
