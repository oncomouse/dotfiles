#!/bin/bash

patches=(
  "https://dwm.suckless.org/patches/activetagindicatorbar/dwm-activetagindicatorbar-6.2.diff"
  "https://dwm.suckless.org/patches/alpha/dwm-alpha-20201019-61bb8b2.diff"
  "https://dwm.suckless.org/patches/alttagsdecoration/dwm-alttagsdecoration-2020010304-cb3f58a.diff"
  "https://dwm.suckless.org/patches/bar_height/dwm-bar-height-6.2.diff"
  "https://dwm.suckless.org/patches/barpadding/dwm-barpadding-20200720-bb2e722.diff"
  "https://dwm.suckless.org/patches/centeredwindowname/dwm-centeredwindowname-20200723-f035e1e.diff"
  "https://dwm.suckless.org/patches/colorbar/dwm-colorbar-6.2.diff"
  "https://dwm.suckless.org/patches/decoration_hints/dwm-decorhints-6.2.diff"
  "https://dwm.suckless.org/patches/defaulttransparency/dwm-defaulttransparency-r1521.diff"
  "https://dwm.suckless.org/patches/dwmc/dwm-dwmc-6.2.diff"
  "https://dwm.suckless.org/patches/ewmhtags/dwm-ewmhtags-6.2.diff"
  "https://dwm.suckless.org/patches/pango/dwm-pango-20201020-519f869.diff"
  "https://dwm.suckless.org/patches/statuspadding/dwm-statuspadding-20150524-c8e9479.diff"
)

branch_name() {
  echo "$1" | awk -F"/" '{print $NF}' | sed -e "s/\.diff//" | sed -e "s/dwm-//"
}

mkdir -p "$HOME/Projects"
cd "$HOME/Projects" || exit
if [[ -d dwm ]]; then
  cd dwm || exit
  git pull
  git branch | grep -v "master" | xargs git branch -D
else
  git clone https://git.suckless.org/dwm
  cd dwm || exit
fi
if [[ -e config.h ]]; then
  ln -sf "$HOME/dotfiles/conf/dwm/config.h" .
fi
git checkout -b build
for patch in "${patches[@]}"; do
  branch=$(branch_name "$patch")
  git checkout -b $branch
  curl -so "$branch.diff" $patch
  git apply "$branch.diff"
  git add .
  git commit -m "$branch"
  git checkout build
  git merge $(git log -1 "$branch" | head -n 1 | cut -d " " -f2)
done
make
sudo make install
git checkout master
cd "$HOME/dotfiles" || exit
