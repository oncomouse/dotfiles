#!/bin/sh
dwmstatus() {
  while true; do
    xsetroot -name "^C0^^B4^$(date +"%a %l:%M %p")"
    sleep 2
  done
}
pkill -f "dwmstatus"
dwmstatus
