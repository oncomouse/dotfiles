#!/bin/bash
project=dwm
patches=(
  "https://dwm.suckless.org/patches/attachbottom/dwm-attachbottom-6.2.diff"
  "https://raw.githubusercontent.com/oncomouse/dwm-patches/master/dwm-xrdb_colorbar-20210117-61bb8b2.diff"
  "https://dwm.suckless.org/patches/ipc/dwm-ipc-20201106-f04cac6.diff"
  "https://dwm.suckless.org/patches/autostart/dwm-autostart-20200610-cb3f58a.diff"
  "https://dwm.suckless.org/patches/ewmhtags/dwm-ewmhtags-6.2.diff"
  "https://dwm.suckless.org/patches/focusonnetactive/dwm-focusonnetactive-6.2.diff"
  "https://dwm.suckless.org/patches/pertag/dwm-pertag-20200914-61bb8b2.diff"
  "https://raw.githubusercontent.com/oncomouse/dwm-patches/master/dwm-restartsig-20210106-61bb8b2.diff"
  "https://raw.githubusercontent.com/oncomouse/dwm-patches/master/dwm-centeredmonocle_staticicon-20210119-61bb8b2.diff"
  "https://raw.githubusercontent.com/oncomouse/dwm-patches/master/dwm-statuscmd-20210118-61bb8b2.diff"
  "https://dwm.suckless.org/patches/cfacts/dwm-cfacts-20200913-61bb8b2.diff"
  "https://raw.githubusercontent.com/oncomouse/dwm-patches/master/dwm-noborder-centeredmonocle-20210119-61bb8b2.diff"
  "https://raw.githubusercontent.com/oncomouse/dwm-patches/master/dwm-activetagindicatorbottombar-20210120-61bb8b2.diff"
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
          "${EDITOR:-vi} $file"
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
rm dwm-msg.o
git checkout master
if [[ ! -e /usr/share/xesssions/dwm.desktop ]]; then
  cat << EOF | sudo tee "/usr/share/xsessions/dwm.desktop" > /dev/null
  [Desktop Entry]
  Encoding=UTF-8
  Name=DWM
  Comment=The Dynamic Window Manager
  Exec=dwm
  Icon=dwm
  Type=XSession
EOF
fi
