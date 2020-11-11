#!/usr/bin/env bash
os="unknown"
case $(uname) in
  Darwin)
    os="macos"
    ;;
  Linux)
    os=$(grep "^NAME" < /etc/os-release | tr -d '"' | tr '[:upper:]' '[:lower:]' | sed -e "s/ linux//" | sed -e "s/name=//")
    ;;
esac
echo "$os"
