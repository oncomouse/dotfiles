#!/usr/bin/env bash
bash=$(which bash)

# Assume "pacman -S grub base-devel vi git curl fish" run during install:
sudo pacman -S --noconfirm - < ~/dotfiles/conf/pacman/packages.txt

$bash ~/dotfiles/bootstrap/scripts/common.sh
# Install DWM:
$bash ~/dotfiles/bootstrap/scripts/dwm.sh
if [ -z "$SERVER" ]; then
  $bash ~/dotfiles/bootstrap/scripts/aur.sh
  # Enable Redshift:
  systemctl --user enable redshift
  # Enable LightDM:
  sudo systemctl enable lightdm
fi

# Use Rofi for dmenu:
sudo ln -sf "$(which rofi)" /usr/bin/dmenu

# Enable OpenSSH:
sudo systemctl enable sshd.service
sudo systemctl start sshd.service

sudo systemctl enable ufw.service
sudo systemctl start ufw.service
sudo ufw allow SSH
sudo ufw enable

# Restrict su
sudo passwd -l root
sudo chgrp -R wheel /usr/local

# Set Shell to Fish:
if ! echo "$SHELL" | grep fish > /dev/null 2>&1; then
  sudo chsh -s "$(which fish)" "$USER"
fi

if [ -z "$SERVER" ]; then
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
fi
