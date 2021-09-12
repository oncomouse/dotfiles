#!/bin/sh

while true; do
    echo "updates|string|Querying updates..."
    echo ""
    update=$(zypper lu | tail -n +4 | wc -l)
    echo "updates|string|Updates:$update"
    echo ""
    sleep 2s
    echo ""
    sleep "${1}h"
done
