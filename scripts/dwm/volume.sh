#!/bin/bash

if ponymix is-muted; then
  echo "x"
else
  echo "$(ponymix get-volume)%"
fi
