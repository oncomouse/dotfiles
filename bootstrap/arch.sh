#!/usr/bin/env bash

# sudo needs to be configured

# Assume, at minimum, "pacman -S base-devel neovim git curl fish sudo" run during install:
sudo cat "$HOME/dotfiles/conf/arch-packages/pacman.txt" | sudo pacman -S --noconfirm --needed --assume-installed=ttf-font-nerd -
if [ "$SERVER" = "" ]; then
	sudo cat "$HOME/dotfiles/conf/arch-packages/pacman-desktop.txt" | sudo pacman -S --noconfirm --needed -
	if [ "$DOTFILES_TARGET" = "desktop" ]; then
		sudo cat "$HOME/dotfiles/conf/arch-packages/pacman-dev.txt" | sudo pacman -S --noconfirm --needed -
	fi
	if [ "$DOTFILES_TARGET" = "settop" ]; then
		sudo cat "$HOME/dotfiles/conf/arch-packages/pacman-settop.txt" | sudo pacman -S --noconfirm --needed -
	fi
fi

~/dotfiles/bootstrap/scripts/common.sh

mkdir -p "$HOME/aur"
# Install some AUR packages (including paru):
~/dotfiles/bootstrap/scripts/aur.sh
cat "$HOME"/dotfiles/conf/arch-packages/aur.txt | paru --needed -S --skipreview --noconfirm -

if [ "$SERVER" = "" ]; then
	cat "$HOME"/dotfiles/conf/arch-packages/aur-desktop.txt | paru --needed -S --skipreview --noconfirm -
	if [ "$DOTFILES_TARGET" = "desktop" ]; then
		cat "$HOME"/dotfiles/conf/arch-packages/aur-dev.txt | paru --needed -S --skipreview --noconfirm -
	fi
	if [ "$DOTFILES_TARGET" = "settop" ]; then
		cat "$HOME"/dotfiles/conf/arch-packages/aur-settop.txt | paru --needed -S --skipreview --noconfirm -
		curl -sLO https://github.com/complexlogic/flex-launcher/releases/download/v2.1/flex-launcher-2.1-1-x86_64.pkg.tar.zst
		sudo pacman -U --noconfirm flex-launcher-2.1-1-x86_64.pkg.tar.zst
	fi
	# Setup flatpak:
	flatpak --user remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
	# Install Flatpaks:
	if [ "$DOTFILES_TARGET" = "desktop" ]; then
		grep -v -e "^#" <"$HOME"/dotfiles/conf/arch-packages/flatpak.txt | sed -e "s/\s*#.*\$//g" | flatpak --user install -
	fi
fi

# Link rofi to dmenu:
if [ "$SERVER" = "" ]; then
	sudo ln -sf "$(which rofi)" /usr/local/bin/dmenu
fi

sudo systemctl daemon-reload
systemctl --user daemon-reload

if [ "$SERVER" = "" ]; then
	## User systemd services
	systemctl --user enable --now pipewire-pulse
	systemctl --user enable --now wireplumber.service
	systemctl --user enable --now mpd.service
	systemctl --user enable --now unclutter.service
	if [ "$DOTFILES_TARGET" = "desktop" ]; then
		systemctl --user enable --now seadrive.service
		systemctl --user enable --now redshift.service
	elif [ "$DOTFILES_TARGET" = "settop" ]; then
		systemctl --user enable --now sxhkd.service
	fi
fi

sudo systemctl enable NetworkManager.service
sudo systemctl start NetworkManager.service

# Enable OpenSSH:
sudo systemctl enable sshd.service
sudo systemctl start sshd.service

# Configure ssh-agent autostart:
# systemctl --user enable ssh-agent.service

# Enable UFW:
sudo systemctl enable ufw.service
sudo systemctl start ufw.service
sudo ufw deny in on any
sudo ufw allow SSH
sudo ufw allow out on any
sudo ufw allow from 192.168.1.0/24
sudo ufw enable

# Set kernel flags:
sudo sed -i ' 1 s/"$/ l1tf=full,force spec_store_bypass_disable=on spectre_v2=on lsm=yama init_on_alloc=1 init_on_free=1 page_alloc.shuffle=1 slab_nomerge vsyscall=none"/' /boot/refind_linux.conf

# Set Shell to Fish:
if ! echo "$SHELL" | grep fish >/dev/null 2>&1; then
	sudo chsh -s "$(which fish)" "$USER"
fi

# Configure Root Environment:
sudo mkdir -p /root/.config/nvim
sudo cp ~/dotfiles/conf/vim/init-minimal.lua /root/.config/nvim/init.lua
# sudo nvim +PaqInstall +qall
sudo cp ~/dotfiles/stow/tmux/dot-tmux.conf /root/.tmux.conf
sudo mkdir -p /root/.tmux/plugins
sudo cp -r ~/.tmux/plugins/tpm /root/.tmux/plugins
sudo mkdir -p /root/.config/fish/conf.d
echo "fzf_key_bindings" | sudo tee /root/.config/fish/conf.d/fzf.fish

if [ "$SERVER" = "" ]; then
	# Configure xdg-utils
	xdg-settings set default-web-browser firefox.desktop
	xdg-mime default org.pwmt.zathura.desktop application/pdf
	xdg-mime default feh.desktop image/png
	xdg-mime default feh.desktop image/jpeg

	# Configure Seadrive:
	if [ "$DOTFILES_TARGET" = "desktop" ]; then
		~/dotfiles/bootstrap/scripts/seadrive.sh
	fi
fi
