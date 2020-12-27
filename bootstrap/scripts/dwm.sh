#!/bin/bash

patches=(
  "https://dwm.suckless.org/patches/vanitygaps/dwm-vanitygaps-20200610-f09418b.diff"
  "https://dwm.suckless.org/patches/attachbottom/dwm-attachbottom-6.2.diff"
  "https://dwm.suckless.org/patches/xrdb/dwm-xrdb-6.2.diff"
  "https://dwm.suckless.org/patches/anybar/dwm-anybar-20200810-bb2e722.diff"
  "https://dwm.suckless.org/patches/ipc/dwm-ipc-20201106-f04cac6.diff"
)

branch_name() {
  echo "$1" | awk -F"/" '{print $NF}' | sed -e "s/\.diff//" | cut -d "-" -f2
}

# So far, all the merge coflicts this build generates are situations where we
# want to keep everything, so we do this to "fix" merge conflicts
merge_conflict() {
  for file in $(git ls-files -u | cut -f 2 | sort -u); do
    local tmpfile
    tmpfile=$(mktemp)
    grep -v -e'^<<<<<<<' -e '^>>>>>>>' -e'=======' "$file" > "$tmpfile"
    mv "$tmpfile" "$file"
  done
  git add .
  git commit -m "merged $1"
}

if [[ -d "$HOME/Projects/dwm" ]]; then
  cd "$HOME/Projects/dwm" || exit
  git pull
  make clean
else
  mkdir -p "$HOME/Projects"
  git clone https://git.suckless.org/dwm "$HOME/Projects/dwm"
  cd "$HOME/Projects/dwm" || exit
fi
# Clean previous build stuff:
git checkout master --force
git branch | grep -v "master" | xargs git branch -D --force
git checkout -b build
ln -sf "$HOME/dotfiles/conf/dwm/config.h" "$HOME/Projects/dwm"
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
