#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
update_chrome() {
	local target="$1"
	if [ "$target" = "firefox" ]; then
		if [ "$os" = "macos" ]; then
			moz_dir="$HOME/Library/Application Support/Firefox/Profiles"
		else
			moz_dir="$HOME/.mozilla/firefox/"
		fi
	elif [ "$target" = "librewolf" ]; then
		moz_dir="$HOME/.librewolf"
	else
		echo "Target ($target) not found."
		return 1
	fi
	dir="$(find "$moz_dir" -maxdepth 1 -iname "*default-release*")"
	if [ -d "$dir" ]; then
		mkdir -p "$dir/chrome"
		ln -sf ~/dotfiles/conf/firefox/userChrome.css "$dir/chrome"
		curl -so "$dir/chrome/userContent.css" https://raw.githubusercontent.com/migueravila/SimplerentFox/master/Linux/userContent.css
		cat << EOF > "$dir/user.js"
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("browser.compactmode.show", true);
user_pref("svg.context-properties.content.enabled", true);
EOF
	else
		echo "No profile found. Perhaps you've not yet run Firefox?"
	fi
}

update_chrome "firefox"
update_chrome "librewolf"
