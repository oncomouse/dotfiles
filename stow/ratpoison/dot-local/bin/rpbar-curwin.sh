#!/usr/bin/env bash
echo "[$(ratpoison -c windows | grep "[0-9]\+\*" | cut -d "*" -f 2)]"
