#!/usr/bin/env bash

bash="$(which bash)"

$bash ~/dotfiles/bootstrap/bundle/apt.sh

# Setup Git:
git config --global user.name "oncomouse"
git config --global user.email "oncomouse@gmail.com"

$bash ~/dotfiles/bootstrap/scripts/common.sh

# Setup Firewall:
sudo ufw allow OpenSSH
sudo ufw enable

# install fail2ban:
sudo apt-get -y install fail2ban
sudo cp /etc/fail2ban/jail.conf /etc/fail2ban/jail.local

# Restrict su
sudo groupadd admin
sudo usermod -a -G admin andrew
sudo dpkg-statoverride --update --add root admin 4750 /bin/su
sudo passwd -l root

# Secure /tmp
sudo fallocate -l 1G /tmpdisk
sudo mkfs.ext4 /tmpdisk
sudo chmod 0600 /tmpdisk
sudo mount -o loop,noexec,nosuid,rw /tmpdisk /tmp
sudo chmod 1777 /tmp
sudo fish -c "echo \"/tmpdisk/tmp ext4 loop,nosuid,noexec,rw 0 0\" >> /etc/fstab"
sudo mv /var/tmp /var/tmpold
sudo ln -s /tmp /var/tmp
sudo cp -prf /var/tmpold/* /tmp/
sudo rm -rf /var/tmpold/
sudo fish -c "echo \"tmpfs /run/shm tmpfs ro,noexec,nosuid 0 0\" >> /etc/fstab"

if ! echo $SHELL | ag fish > /dev/null 2>&1; then
  sudo chsh -s $(which fish) $USER
fi

# Clean up
sudo apt -y autoremove

# IP Sec features:
echo "Copy your SSH key(s) and turn off Root/Password login. Add IP Hardening"
echo ""
echo "    $(tput setaf 6)https://dennisnotes.com/note/20180627-ubuntu-18.04-server-setup/$(tput sgr0)"
