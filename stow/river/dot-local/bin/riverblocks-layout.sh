#!/usr/bin/env bash
tag="$(cat /tmp/river_tag.json | jq ".[0]")"
cat /tmp/river_layout.json | jq -r ".[$tag] // \"[]=\""
