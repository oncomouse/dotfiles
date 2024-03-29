#!/usr/bin/env bash

packages=(
  "paru-bin"
)

mkdir -p ~/aur
for package in "${packages[@]}"; do
  cd ~/aur || continue
  regex='^http'
  if [[ $package =~ $regex ]]; then
    url=$package
    [[ $url =~ \/([^/]*)$ ]]
    package=${BASH_REMATCH[1]}
  else
    url="https://aur.archlinux.org/$package"
  fi
  build=1
  if [ -d "./$package" ]; then
    cd "$package" || continue
    git remote update >> /dev/null
    local_rev=$(git rev-parse master)
    remote_rev=$(git rev-parse origin/master)
    if [[ $local_rev != "$remote_rev" ]];then
      git pull
    else
      build=0
    fi
  else
    git clone "$url"
    cd "$package" || continue
  fi
  if [[ $build == 1 ]]; then
    makepkg -si --noconfirm
  fi
done
cd ~/dotfiles || exit
