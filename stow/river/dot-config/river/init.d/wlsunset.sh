#!/bin/sh

killall -q wlsunset
while pgrep -x wlsunset >/dev/null; do sleep 1; done
exec wlsunset -l 30.6 -L -96.3
