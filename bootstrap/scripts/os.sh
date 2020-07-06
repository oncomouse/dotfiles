#!/usr/bin/env bash
os="unknown"
case `uname` in
  Darwin)
    os="macos"
    ;;
  Linux)
    os=$(grep "^NAME" < /etc/os-release | tr -d "NAME=" | tr -d '"' | tr '[:upper:]' '[:lower:]' | sed -e "s/ linux//")
    ;;
esac
echo $os
