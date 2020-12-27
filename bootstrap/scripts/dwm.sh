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
git checkout master
git branch | grep -v "master" | xargs git branch -D
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
merging=1
for patch in "${patches[@]}"; do
  branch=$(branch_name "$patch")
  if test "$merging" -eq 1; then
    git merge -m "merging $branch" "$branch"
  else
    echo "Merge $branch later."
  fi
  if test $? -ne 0; then
    merging=1
  fi
done
