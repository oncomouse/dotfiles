#!/bin/sh
update=$(zypper lu | tail -n +5 | wc -l)
#result=$(echo "$update / $total_installed_packages * 100" | bc -l)
echo "${update}"
#if [ $update == "0" ]; then
    #echo "Up-to-date"
#else
    #echo "Updates:$update"
#fi

