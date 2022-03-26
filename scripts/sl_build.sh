#!/usr/bin/env bash

# Where are all the configuration and target files stored:
ROOT_CONFIGURATION_PATH="$HOME/dotfiles/conf"
# Directory structure will be:
# - Main configuration file at:
#   - $ROOT_CONFIGURATION_PATH/$project/config.h (or blocks.h for dwmblocks)
# - Optional list of patches as a JSON array (requires jq):
#   - $ROOT_CONFIGURATION_PATH/$project/patches.json
# - Optional target files for different configurations per machine:
#   - $ROOT_CONFIGURATION_PATH/$project/targets/$TARGET/target.h ($TARGET should be set in shell by user)

# We default to ~/.local/share/dwm-config for building the projects, but set
# to whatever you would like here:
BUILD_LOCATION=${XDG_DATA_HOME:-$HOME/.local/share}/dwm-config

# Determine which Suckless project is being used:
project=${1:-dwm}

# Get the name of the configuration file, if it is different:
conf_file="config.h"
if [ "$project" = "dwmblocks" ]; then
	conf_file="blocks.h"
elif [ "$project" = "neatvi" ] || [ "$project" = "nextvi" ]; then
	conf_file="conf.c"
fi

main_branch=master
if [ "$project" == "dwl" ]; then
	main_branch=main
fi

# Read in a list of patches to apply:
if [ -e "conf/$project/patches.json" ]; then
	mapfile -t patches < <(jq -r .[] "conf/$project/patches.json")
else
	patches=()
fi

# Determine branch name from patch file name:
branch_name() {
	echo "$1" | awk -F"/" '{print $NF}' | sed -e "s/\.\(diff\|patch\)//" | cut -d "-" -f2 | cut -d ":" -f2
}

# Resolve merge conflicts. Either, accept both the patch and head, accept the
# patch, accept head, or open an editor to combine patch and head.
merge_conflict() {
	echo "Merge Conflict in $1."
	echo ""
	for file in $(git ls-files -u | cut -f 2 | sort -u); do
		local tmpfile

		echo "Fixing $file."
		PS3='Please enter your choice: '
		select opt in "Accept Both" "Accept Patch" "Accept Head" "Edit ${file}"; do
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
				"Accept Head")
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

install_software() {
	if [ "$project" = "neatvi" ]; then
		sudo cp vi /usr/local/bin
	else
		sudo make install
	fi
}

# Appending "rebuild" as a second argument will recompile the project without
# patching everything again. Use this for updating config files, etc.
rebuild() {
	owd="$(pwd)"
	cd "$BUILD_LOCATION/$project" || exit
	git checkout build
	make
	install_software
	git checkout "$main_branch"
	cd "$owd" || exit
}
if [ "$2" = "rebuild" ]; then
	rebuild
	exit
fi

# Clone the project repository if it doesn't exist:
if [[ ! -d "$BUILD_LOCATION/$project" ]]; then
	mkdir -p "$BUILD_LOCATION"
	project_repo="git://git.suckless.org/$project"

	# Non-suckless projects that have different Git URLs:
	if [ "$project" = "dwmblocks" ]; then
		project_repo=https://github.com/torrinfail/dwmblocks
	elif [ "$project" = "dwl" ]; then
		project_repo=https://github.com/djpohly/dwl
	elif [ "$project" = "aslstatus" ]; then
		project_repo=https://notabug.org/dm9pZCAq/aslstatus
	elif [ "$project" = "neatvi" ]; then
		project_repo=https://github.com/aligrudi/neatvi
	elif [ "$project" = "nextvi" ]; then
		project_repo=https://github.com/kyx0r/nextvi
	elif [ "$project" = "2bwm" ]; then
		project_repo=https://github.com/venam/2bwm
	elif [ "$project" = "shod" ]; then
		project_repo=https://github.com/phillbush/shod
	elif [ "$project" = "berry" ]; then
		project_repo=https://github.com/JLErvin/berry
	elif [ "$project" = "lemonaid" ]; then
		project_repo=https://github.com/Murtaza-Udaipurwala/lemonaid
fi

	git clone "$project_repo" "$BUILD_LOCATION/$project"
fi

cd "$BUILD_LOCATION/$project" || exit
# Reset project to default state, so we can pull changes into the repo:
git checkout "$main_branch" --force
# Clean previous build stuff:
git branch | grep -v "$main_branch" | xargs git branch -D --force
# Update Git repo:
git pull
# Clean:
make clean

# Generate our separate build branch:
git checkout -b build
# aslstatus doesn't use the config.def.h convention, so delete the default:
if [ "$project" = "2bwm" ] || [ "$project" = "lemonaid" ] || [ "$project" = "shod" ] || [ "$project" = "aslstatus" ] || [ "$project" = "neatvi" ] || [ "$project" = "nextvi" ]; then
	rm "$conf_file"
fi
# Link the configuration file from our repository:
if [ -f "$ROOT_CONFIGURATION_PATH/$project/$conf_file" ]; then
	ln -sf "$ROOT_CONFIGURATION_PATH/$project/$conf_file" "$BUILD_LOCATION/$project"
fi

# Check if there is a target file:
TARGET="${DOTFILES_TARGET:-laptop}"
if [ -e "$ROOT_CONFIGURATION_PATH/$project/targets/$TARGET/target.h" ]; then
	ln -sf "$ROOT_CONFIGURATION_PATH/$project/targets/$TARGET/target.h" "$BUILD_LOCATION/$project"
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
if [ "$project" == "berry" ];then
	./configure
fi
make
install_software

# Reset to the default state:
make clean
git checkout "$main_branch"
