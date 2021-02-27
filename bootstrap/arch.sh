#!/usr/bin/env bash

# Assume, at minium, "pacman -S base-devel vim git curl fish" run during install:
sudo pacman -S --noconfirm --needed - < "$HOME/dotfiles/conf/arch-packages/pacman.txt"

# Install Yay:
url_makepkg() {
  local pkg
  pkg=$(basename "$1")
  mkdir -p "$HOME/aur"
  git clone "$1" "$HOME/aur/$pkg"
  cd "$HOME/aur/$pkg" || exit
  makepkg -si --noconfirm
  cd "$HOME/dotfiles" || exit
}
url_makepkg "https://aur.archlinux.org/yay"

# Install AUR using Yay:
grep -v -e "^#" < "$HOME"/dotfiles/conf/arch-packages/aur.txt | sed -e "s/\s*#.*\$//g" | yay -S --noconfirm -
# Setup flatpak:
flatpak --user remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
# Install Flatpaks:
grep -v -e "^#" < "$HOME"/dotfiles/conf/arch-packages/flatpak.txt | sed -e "s/\s*#.*\$//g" | flatpak --user install -

# Install non-AUR PKGBUILD stuff:
~/dotfiles/bootstrap/scripts/aur.sh

~/dotfiles/bootstrap/scripts/common.sh
if [ -z "$SERVER" ]; then
  ## User systemd services
  systemctl --user enable pipewire-pulse
  systemctl --user enable seadrive

  # Use Rofi for dmenu:
  sudo ln -sf "$(which rofi)" /usr/bin/dmenu

  # Configure spicetify:
  ~/dotfiles/bootstrap/scripts/spicetify.sh
fi

# Enable OpenSSH:
sudo systemctl enable sshd.service
sudo systemctl start sshd.service

# Enable UFW:
sudo systemctl enable ufw.service
sudo systemctl start ufw.service
sudo ufw deny in
sudo ufw allow SSH
sudo ufw allow out
sudo ufw enable

# Set kernel flags:
sudo sed -i ' 1 s/"$/ l1tf=full,force spec_store_bypass_disable=on spectre_v2=on lsm=lockdown,yama,apparmor lockdown=confidentiality init_on_alloc=1 init_on_free=1 page_alloc.shuffle=1 slab_nomerge vsyscall=none"/' /boot/refind_linux.conf

# Restrict su
sudo passwd -l root
sudo chgrp -R wheel /usr/local

# Set Shell to Fish:
if ! echo "$SHELL" | grep fish > /dev/null 2>&1; then
  sudo chsh -s "$(which fish)" "$USER"
fi

# Configure Firejail:
# ~/dotfiles/bootstrap/scripts/firejail.sh

if [ -z "$SERVER" ]; then
  # Configure xdg-utils
  xdg-settings set default-web-browser firefox.desktop
  xdg-mime default org.pwmt.zathura.desktop application/pdf

  # Get spotify to work with firejail and spotifywm:
  #if [[ -e /usr/local/bin/spotify ]]; then
  #  sudo rm /usr/local/bin/spotify
  #fi
  #cat << EOF | sudo tee /usr/local/bin/spotify > /dev/null
##!/bin/sh
#firejail --env="LD_PRELOAD=/usr/lib/spotifywm.so" spotify "$@"
#EOF
  #sudo chmod +x "/usr/local/bin/spotify"

  # Configure Seadrive:
  ~/dotfiles/bootstrap/scripts/seadrive.sh
fi
