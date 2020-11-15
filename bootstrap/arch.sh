#!/usr/bin/env bash
bash=$(which bash)

# Assume "pacman -S grub base-devel vi git curl fish" run during install:
sudo pacman -S - < ~/dotfiles/pacman-pkg.txt
$bash ~/dotfiles/bootstrap/scripts/aur.conf

# Setup Git:
git config --global user.name "oncomouse"
git config --global user.email "oncomouse@gmail.com"

$bash ~/dotfiles/bootstrap/scripts/common.sh
$bash ~/dotfiles/bootstrap/scripts/ncspot.sh
$bash ~/dotfiles/bootstrap/scripts/spectrwm.conf
$bash ~/dotfiles/bootstrap/scripts/rofi.conf

# Enable OpenSSH:
systemctl enable sshd.service
systemctl start sshd.service

sudo pacman -S ufw
sudo systemctl enable ufw.service
sudo systemctl start ufw.service
sudo ufw allow OpenSSH
sudo ufw enable

# Restrict su
sudo groupadd admin
sudo usermod -a -G admin andrew
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

# Set Shell to Fish:
if ! echo "$SHELL" | grep fish > /dev/null 2>&1; then
  sudo chsh -s "$(which fish)" "$USER"
fi
