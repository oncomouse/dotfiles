#!/usr/bin/env bash

if [ -e ~/.duckdns/duck.json ]; then
	while read -r dns; do
		name="$(echo "$dns" | cut -d "," -f 1)"
		token="$(echo "$dns" | cut -d "," -f 2)"
		echo url="$(printf "https://www.duckdns.org/update?domains=%s&token=%s&ip=" "$name" "$token")" | curl -k -so ~/.duckdns/duck.log -K -
	done <<< "$(jq -r '.[] | [."dns", ."token"] | join(",")' ~/.duckdns/duck.json)"
fi

# DuckDNS Service: https://www.duckdns.org
