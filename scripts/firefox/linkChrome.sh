#!/usr/bin/env bash

os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)

if [ "$os" == "macos" ]; then
	profile_path="$HOME/Library/Application Support/Firefox/Profiles"
else
	profile_path="$HOME/.mozilla/firefox"
fi

profile="$(fd --max-results 1 default-release "$profile_path")"
mkdir -p "$profile/chrome"
if [ ! -h "$profile/chrome/userChrome.css" ]; then
	ln -sf ~/dotfiles/conf/firefox/userChrome.css "$profile/chrome"
fi
