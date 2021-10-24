#!/bin/sh

killall -q wlsunset
while pgrep -x wlsunset >/dev/null; do sleep 1; done
exec wlsunset -l 8.17 -L 123.80
