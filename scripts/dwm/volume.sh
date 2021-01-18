#!/bin/bash

if ponymix is-muted; then
  echo "x"
else
  ponymix get-volume
fi
