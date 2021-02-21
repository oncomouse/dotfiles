#!/usr/bin/env bash

# Seadrive install for Arch

oldpwd=$(pwd)
mkdir -p "$HOME/Projects"
cd "$HOME/Projects" || exit

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
mkdir -p ~/.local/share/seadrive/Seafile
cp "$HOME/dotfiles/conf/seadrive/seadrive.conf.template" ~/.config/seadrive/seadrive.conf

echo 'Run curl -d "username=USERNAME" -d "password=PASSWORD" -H "X-SEAFILE-OTP: 2FA_KEY" https://SEAFILE_SERVER/api2/auth-token/'
echo "And paste token into ~/.config/seafile/seafile.conf"
echo "Then run systemctl start --user seadrive"
