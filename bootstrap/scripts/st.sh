#!/bin/bash
project=st
patches=(
  "https://st.suckless.org/patches/anysize/st-anysize-20201003-407a3d0.diff"
  "https://st.suckless.org/patches/scrollback/st-scrollback-0.8.4.diff"
  "https://st.suckless.org/patches/clipboard/st-clipboard-0.8.3.diff"
  "https://raw.githubusercontent.com/oncomouse/dwm-patches/master/st-ligatures-scrollback-20210114-4ef0cbd.diff"
  "https://st.suckless.org/patches/font2/st-font2-20190416-ba72400.diff"
  "https://st.suckless.org/patches/xresources/st-xresources-20200604-9ba7ecf.diff"
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
# Setup the most recent source:
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
# Add to application launchers:
if [[ ! -e "$HOME/.local/share/applications/st.desktop" ]]; then
  cat << EOF > "$HOME/.local/share/applications/st.desktop"
[Desktop Entry]
Version=0.8.4
Type=Application
Name=st
GenericName=Terminal emulator
Comment=The Suckless Terminal
TryExec=st
Exec=st
Icon=st
Categories=System;TerminalEmulator;
EOF
fi
