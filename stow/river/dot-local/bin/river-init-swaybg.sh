#!/usr/bin/env bash

killall -q swaybg
while pgrep -x swaybg >/dev/null; do sleep 1; done
convert ~/.cache/wal/background.svg ~/.cache/wal/background.png
exec swaybg -m tile -i ~/.cache/wal/background.png
