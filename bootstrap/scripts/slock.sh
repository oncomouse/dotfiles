#!/bin/bash
project=slock
patches=(
  "https://tools.suckless.org/slock/patches/blur-pixelated-screen/slock-blur_pixelated_screen-1.4.diff"
  "https://raw.githubusercontent.com/oncomouse/dotfiles/master/conf/slock/patches/capscolor.diff"
  "https://tools.suckless.org/slock/patches/terminalkeys/slock-terminalkeys-1.4.diff"
  "https://tools.suckless.org/slock/patches/xresources/slock-xresources-20191126-53e56c7.diff"
)

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
    select opt in "Accept Both" "Edit ${file}"; do
      case $opt in
        "Accept Both")
          tmpfile=$(mktemp)
          grep -v -e'^<<<<<<<' -e '^>>>>>>>' -e'=======' "$file" > "$tmpfile"
          mv "$tmpfile" "$file"
          break
          ;;
        "Edit ${file}")
          vim "$file"
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

if [[ ! -d "$HOME/Projects/$project" ]]; then
  mkdir -p "$HOME/Projects"
  git clone https://git.suckless.org/$project "$HOME/Projects/$project"
fi
cd "$HOME/Projects/$project" || exit
# Setup the most recent DWM source:
git checkout master --force
# Clean previous build stuff:
git branch | grep -v "master" | xargs git branch -D --force
git pull
make clean
git checkout -b build
ln -sf "$HOME/dotfiles/conf/$project/config.h" "$HOME/Projects/$project"
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
set -e
for patch in "${patches[@]}"; do
  branch=$(branch_name "$patch")
  git merge -m "merging $branch" "$branch" || merge_conflict "$branch"
done
set +e
make
sudo make install
make clean
git checkout master
