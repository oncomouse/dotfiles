#!/bin/bash
# XDG Autostart:
dex -a

# Terminate already running status instances
killall -q aslstatus
# Wait until the processes have been shut down
while pgrep -u $UID -x aslstatus >/dev/null; do sleep 1; done
aslstatus&
