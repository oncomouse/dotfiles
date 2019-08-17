#!/usr/bin/env bash

if ! which golint > /dev/null 2>&1; then
  go get -u golang.org/x/lint/golint
fi
if ! which goimports > /dev/null 2>&1; then
  go get golang.org/x/tools/cmd/goimports
fi
