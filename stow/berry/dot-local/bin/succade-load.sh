#!/usr/bin/env bash
echo -n " "
uptime | sed -e "s/^.*load average: //"
