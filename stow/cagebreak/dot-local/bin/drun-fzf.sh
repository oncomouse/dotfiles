#!/usr/bin/env bash

cachedir="${XDG_CACHE_HOME:-"$HOME/.cache"}"
cache="$cachedir/drun_fzf"

format_xdg_data_dirs() {
    printf "%s" "$XDG_DATA_DIRS" | xargs -d : -I'{}' printf "%s/applications/*.desktop\\n" "{}"
}

drun () {
    [ -z "$XDG_DATA_DIRS" ] && XDG_DATA_DIRS="/usr/share:$HOME/.local/share"
    # shellcheck disable=SC2046
    rg --files-without-match ^NoDisplay=true $(format_xdg_data_dirs) 2> /dev/null | xargs -I {} bash -c "printf '{},%s\n' \"\$(rg Name= {} | head -1 | sed -e s/Name=//)\"" | fzf --reverse -1 --with-nth=2.. --delimiter="," | cut -d "," -f 1 |  while read -r command
	do
		[ -z "$command" ] && continue

		# shellcheck disable=SC2091
		$(grep -E '^Exec' "$command" | tail -1 | sed -e 's/^Exec=//' -e 's/%.//'  -e 's/^"//g' -e 's/" *$//g') > /dev/null 2>&1 &
	done
}

drun
