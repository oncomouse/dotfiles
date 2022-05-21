#!/usr/bin/env bash
workspaces="$1"
rpws init "$workspaces" -a
for i in $(seq 1 "$workspaces"); do
	ratpoison -c "definekey top C-M-$i exec rpws $i"
	ratpoison -c "definekey top C-M-S-$i exec rpwsm$i"
done
