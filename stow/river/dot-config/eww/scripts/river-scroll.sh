#!/bin/sh
 
cmd=$1
curtag=$(echo "$2 - 1" | bc -l)
list_tags=()

for i in $(seq 1 11)
do
    tags=$((1 << ($i - 1)))
    list_tags+=( $tags )

done

#echo "${list_tags[@]}"

nextag=$(echo "$curtag + 1" | bc -l)
prevtag=$(echo "$curtag - 1" | bc -l)

if [ $cmd == "up" ]
then
    if [[ $nextag -gt 10 ]]
    then
        riverctl set-focused-tags 1
    else
        riverctl set-focused-tags ${list_tags[$nextag]}
    fi
elif [ $cmd == "down" ]
then
    if [[ $curtag -lt 0 ]]
    then
        riverctl set-focused-tags 1024
    else
        riverctl set-focused-tags ${list_tags[$prevtag]}
    fi
fi

#echo $cmd$prevtag${list_tags[$prevtag]}$curtag

