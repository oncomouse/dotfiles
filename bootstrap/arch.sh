#!/usr/bin/env bash

# Assume, at minimum, "pacman -S base-devel vim git curl fish" run during install:
sudo pacman -S --noconfirm --needed - < "$HOME/dotfiles/conf/arch-packages/pacman.txt"

~/dotfiles/bootstrap/scripts/common.sh

mkdir -p "$HOME/aur"
# Install some AUR packages (including paru):
~/dotfiles/bootstrap/scripts/aur.sh

# Install AUR using Paru:
 for pkg in "$(grep -v -e "^#" < "$HOME"/dotfiles/conf/arch-packages/aur.txt | sed -e "s/\s*#.*\$//g")"; do
	paru --needed -S --skipreview $pkg
 done
# Setup flatpak:
flatpak --user remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
# Install Flatpaks:
grep -v -e "^#" < "$HOME"/dotfiles/conf/arch-packages/flatpak.txt | sed -e "s/\s*#.*\$//g" | flatpak --user install -

if [ -z "$SERVER" ]; then
	## User systemd services
	systemctl --user enable pipewire-pulse
	systemctl --user enable seadrive.service
	systemctl --user enable kmonad.service
	systemctl --user enable mpd.service
	systemctl --user enable mpDris2.service
	systemctl --user enable tmux.service
	systemctl --user enable redshift.service

	# Use Rofi for dmenu:
	sudo ln -sf "$(which rofi)" /usr/bin/dmenu
fi
sudo systemctl enable NetworkManager.service
sudo systemctl start NetworkManager.service

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

if [ -z "$SERVER" ]; then
	# Configure xdg-utils
	xdg-settings set default-web-browser firefox.desktop
	xdg-mime default org.pwmt.zathura.desktop application/pdf

	# Configure Seadrive:
	~/dotfiles/bootstrap/scripts/seadrive.sh
fi
