#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
if [ "$os" = "macos" ]; then
	moz_dir="$HOME/Library/Application Support/Firefox"
else
	moz_dir="$HOME/.mozilla/firefox/"
fi
dir="$(find "$moz_dir" -maxdepth 1 -iname "*default-release*")"
if [ -d "$dir" ]; then
	mkdir -p "$dir/chrome"
	curl -so "$dir/chrome/userChrome.css" https://raw.githubusercontent.com/migueravila/SimplerentFox/master/Linux/userChrome__OneLine.css
	curl -so "$dir/chrome/userContent.css" https://raw.githubusercontent.com/migueravila/SimplerentFox/master/Linux/userContent.css
	cat << EOF > "$dir/user.js"
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("browser.compactmode.show", true);
user_pref("svg.context-properties.content.enabled", true);
EOF
else
	echo "No profile found. Perhaps you've not yet run Firefox?"
fi
