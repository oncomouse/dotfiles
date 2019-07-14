#!/usr/bin/env bash
os="unknown"
case `uname` in
  Darwin)
    os="macos"
    ;;
  Linux)
    os=`cat /etc/os-release | ag "^NAME" | tr -d "NAME=" | tr -d '"' | tr '[:upper:]' '[:lower:]'`
    ;;
esac
echo $os
