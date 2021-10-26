#!/usr/bin/env bash

pkill -f liskin-media
exec liskin-media mpris-daemon
