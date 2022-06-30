#!/usr/bin/env bash

# Seadrive install for Arch

oldpwd=$(pwd)
mkdir -p "$HOME/Projects"
cd "$HOME/Projects" || exit

gpg --recv-key 910397D88D29319A --keyserver hkp://keyserver.ubuntu.com:80
paru --skipreview --noconfirm -S libevent-compat libsearpc

git clone https://github.com/haiwen/seadrive-fuse
cd seadrive-fuse || exit
./autogen.sh
./configure
make
sudo make install
systemctl enable --user seadrive
cd "$oldpwd" || exit

mkdir -p ~/.config/seadrive
mkdir -p ~/.local/share/seadrive/data
mkdir -p ~/.local/share/seadrive/logs
cp "$HOME/dotfiles/conf/seadrive/seadrive.conf.template" ~/.config/seadrive/seadrive.conf

echo 'Run curl -d "username=<USERNAME>" -d "password=<PASSWORD>" -H "X-SEAFILE-OTP: <CURRENT 2FA_KEY>" https://SEAFILE_SERVER/api2/auth-token/'
echo "And paste token into ~/.config/seafile/seafile.conf"
echo " (In seafile.conf, server needs protocol)"
echo "Then run systemctl --user enable --now seadrive"
