#!/usr/bin/env bash

# Seadrive install for Arch

# Install prerequisites
gpg --recv-key 910397D88D29319A --keyserver hkp://keyserver.ubuntu.com:80
paru --skipreview --noconfirm -S libevent-compat libsearpc

# Install the fuse client daemon
oldpwd=$(pwd)
mkdir -p ~/aur
https://github.com/oncomouse/seadrive-daemon-git ~/aur/seadrive-daemon-git
cd ~/aur/seadrive-daemon-git || exit
makepkg -si
cd "$oldpwd" || exit

# Make directories
mkdir -p ~/.config/seadrive
mkdir -p ~/.local/share/seadrive/data
mkdir -p ~/.local/share/seadrive/logs
cp ~/dotfiles/conf/seadrive/seadrive.conf.template ~/.config/seadrive/seadrive.conf

# Print instructions
echo 'Run curl -d "username=<USERNAME>" -d "password=<PASSWORD>" -H "X-SEAFILE-OTP: <CURRENT 2FA_KEY>" https://SEAFILE_SERVER/api2/auth-token/'
echo "And paste token into ~/.config/seafile/seafile.conf"
echo " (In seafile.conf, server needs protocol)"
echo "Then run systemctl --user enable --now seadrive"
