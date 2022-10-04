#!/usr/bin/env bash

# Seadrive install for Arch

# Install prerequisites
paru --skipreview --noconfirm -S libsearpc --mflags "--skippgpcheck"

# Install from git:
mkdir -p ~/Projects
git clone https://github.com/haiwen/seadrive-fuse ~/Projects/seadrive-fuse
oldpwd=$(pwd)
cd ~/Projects/seadrive-fuse || exit
./autogen.sh
./configure --prefix=/usr
make
sudo make install
cd "$oldpwd" || exit

# Install the fuse client daemon
mkdir -p ~/.config/seadrive
mkdir -p ~/.local/share/seadrive/data
mkdir -p ~/.local/share/seadrive/logs
cp ~/dotfiles/conf/seadrive/seadrive.conf.template ~/.config/seadrive/seadrive.conf

# Print instructions
echo 'Run curl -d "username=<USERNAME>" -d "password=<PASSWORD>" -H "X-SEAFILE-OTP: <CURRENT 2FA_KEY>" https://SEAFILE_SERVER/api2/auth-token/'
echo "And paste token into ~/.config/seafile/seafile.conf"
echo " (In seafile.conf, server needs protocol)"
echo "Then run systemctl --user enable --now seadrive"
