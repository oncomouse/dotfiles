#!/usr/bin/env bash
bash=$(which bash)

# Assume "pacman -S grub base-devel vi git curl fish" run during install:
sudo pacman -S --noconfirm - < ~/dotfiles/pacman-pkg.txt

$bash ~/dotfiles/bootstrap/scripts/common.sh
$bash ~/dotfiles/bootstrap/scripts/aur.sh

# Other setup files:
ln -sf ~/dotfiles/rofi/ ~/.config/
ln -sf ~/dotfiles/bspwm ~/.config/
ln -sf ~/dotfiles/sxhkd ~/.config/
ln -sf ~/dotfiles/dunst ~/.config/
ln -sf ~/dotfiles/xorg/xprofile ~/.xprofile
ln -sf ~/dotfiles/xorg/Xresources ~/.Xresources
ln -sf ~/dotfiles/gtk-3.0 ~/.config/
ln -sf ~/dotfiles/gtk-2.0/gtkrc-2.0 ~/.gtkrc-2.0

# Enable LightDM:
sudo systemctl enable lightdm

arch# Enable OpenSSH:
sudo systemctl enable sshd.service
sudo systemctl start sshd.service

sudo systemctl enable ufw.service
sudo systemctl start ufw.service
sudo ufw allow SSH
sudo ufw enable

# Restrict su
sudo passwd -l root
sudo chgrp -R wheel /usr/local

# Secure /tmp
# sudo fallocate -l 1G /tmpdisk
# sudo mkfs.ext4 /tmpdisk
# sudo chmod 0600 /tmpdisk
# sudo mount -o loop,noexec,nosuid,rw /tmpdisk /tmp
# sudo chmod 1777 /tmp
# sudo fish -c "echo \"/tmpdisk/tmp ext4 loop,nosuid,noexec,rw 0 0\" >> /etc/fstab"
# sudo mv /var/tmp /var/tmpold
# sudo ln -s /tmp /var/tmp
# sudo cp -prf /var/tmpold/* /tmp/
# sudo rm -rf /var/tmpold/
# sudo fish -c "echo \"tmpfs /run/shm tmpfs ro,noexec,nosuid 0 0\" >> /etc/fstab"

# Set Shell to Fish:
if ! echo "$SHELL" | grep fish > /dev/null 2>&1; then
  sudo chsh -s "$(which fish)" "$USER"
fi

# Configure xdg-utils
xdg-settings set default-web-browser firefox.desktop
xdg-mime default org.pwmt.zathura.desktop application/pdf

# Fonts
mkdir -p ~/Downloads
curl -o "$HOME/Downloads/calvin-font.zip" "https://dl.dafont.com/dl/?f=calvin_and_hobbes"
unzip -d ~/Downloads ~/Downloads/calvin-font.zip
sudo mkdir -p /usr/share/fonts/misc/
sudo mv ~/Downloads/*.TTF /usr/share/fonts/misc/
rm ~/Downloads/calvin-font.zip
fc-cache -r
