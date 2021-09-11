#!/bin/bash

# Get information:
project=${1:-dwm}
conf_file="config.h"
if [ "$project" = "dwmblocks" ]; then
	conf_file="blocks.h"
fi

# Read in a list of patches:
if [ -e "conf/$project/patches.json" ]; then
	mapfile -t patches < <(jq -r .[] "conf/$project/patches.json")
else
	patches=()
fi

branch_name() {
	echo "$1" | awk -F"/" '{print $NF}' | sed -e "s/\.diff//" | cut -d "-" -f2
}

# So far, all the merge coflicts this build generates are situations where we
# want to keep everything, so we do this to "fix" merge conflicts
merge_conflict() {
	echo "Merge Conflict in $1."
	echo ""
	for file in $(git ls-files -u | cut -f 2 | sort -u); do
		local tmpfile

		echo "Fixing $file."
		PS3='Please enter your choice: '
		select opt in "Accept Both" "Accept Patch" "Edit ${file}"; do
			case $opt in
				"Accept Both")
					tmpfile=$(mktemp)
					grep -v -e'^<<<<<<<' -e '^>>>>>>>' -e'=======' "$file" > "$tmpfile"
					mv "$tmpfile" "$file"
					break
					;;
				"Accept Patch")
					tmpfile=$(mktemp)
					sed -e '/^<<<<<<</,/^=======/d' -e '/^>>>>>>>/d' "$file" > "$tmpfile"
					mv "$tmpfile" "$file"
					break
					;;
				"Accept Master")
					tmpfile=$(mktemp)
					sed -e '/^=======/,/^>>>>>>>/d' -e '/^<<<<<<</d' "$file" > "$tmpfile"
					mv "$tmpfile" "$file"
					break
					;;
				"Edit ${file}")
					eval "${EDITOR:-nvim} $file"
					break
					;;
				"*")
					echo "Invalid Reply"
					;;
			esac
		done
	done
	git add .
	git commit -m "merged $1"
}

# Appending "rebuild" as a second argument will recompile the project without
# patching everything again. Use this for updating config files, etc.
rebuild() {
	owd="$(pwd)"
	cd "$HOME/.local/share/dwm-config/$project" || exit
	git checkout build
	make
	sudo make install
	git checkout master
	cd "$owd" || exit
}
if [ "$2" = "rebuild" ]; then
	rebuild
	exit
fi

# Clone the project repository if it doesn't exist:
if [[ ! -d "$HOME/.local/share/dwm-config/$project" ]]; then
	mkdir -p "$HOME/.local/share/dwm-config"
	project_repo="git://git.suckless.org/$project"

	# Non-suckless projects that have different Git URLs:
	if [ "$project" = "dwmblocks" ]; then
		project_repo=https://github.com/torrinfail/dwmblocks
	elif [ "$project" = "aslstatus" ]; then
		project_repo=https://notabug.org/dm9pZCAq/aslstatus
	fi

	git clone "$HOME/.local/share/dwm-config/$project"
fi

cd "$HOME/.local/share/dwm-config/$project" || exit
# Reset project to default state, so we can pull changes into the repo:
git checkout master --force
# Clean previous build stuff:
git branch | grep -v "master" | xargs git branch -D --force
# Update Git repo:
git pull
# Clean:
make clean

# Generate our separate build branch:
git checkout -b build
# aslstatus doesn't use the config.def.h convention, so delete the default:
if [ "$project" = "aslstatus" ]; then
	rm "$HOME/Projects/dwm-config/conf/$project/$conf_file"
fi
# Link the configuration file from our repository:
ln -sf "$HOME/Projects/dwm-config/conf/$project/$conf_file" "$HOME/.local/share/dwm-config/$project"

# Check if there is a target file:
TARGET="${DOTFILES_TARGET:-laptop}"
if [ -e "$HOME/Projects/dwm-config/conf/$project/targets/$TARGET/target.h" ]; then
	ln -sf "$HOME/Projects/dwm-config/conf/$project/targets/$TARGET/target.h" "$HOME/.local/share/dwm-config/$project"
fi

# Apply any patches:
for patch in "${patches[@]}"; do
	branch=$(branch_name "$patch")
	git checkout -b "$branch"
	curl -so "$branch.diff" "$patch"
	git apply ./*.diff
	rm ./*.diff
	git add .
	git commit -m "Applied $branch"
	git checkout build
done

# Merge each patch back into the build repository:
set -e
for patch in "${patches[@]}"; do
	branch=$(branch_name "$patch")
	git merge -m "merging $branch" "$branch" || merge_conflict "$branch"
done
set +e

# Build and install the software:
make
sudo make install

# Reset to the default state:
make clean
git checkout master
