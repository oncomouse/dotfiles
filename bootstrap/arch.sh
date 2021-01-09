#!/usr/bin/env bash
bash=$(which bash)

# Assume "pacman -S grub base-devel vi git curl fish" run during install:
sudo pacman -S --noconfirm --needed - < "$HOME/dotfiles/conf/pacman/packages.txt"

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
grep -v -e "^#" < "$HOME"/dotfiles/conf/pacman/aur.txt | sed -e "s/\s*#.*\$//g" | yay -S --noconfirm -
# Install Suckless Stuff:
$bash ~/dotfiles/bootstrap/scripts/dwm.sh
$bash ~/dotfiles/bootstrap/scripts/slock.sh

# Install non-AUR PKGBUILD stuff:
$bash ~/dotfiles/bootstrap/scripts/aur.sh

$bash ~/dotfiles/bootstrap/scripts/common.sh
if [ -z "$SERVER" ]; then
  # $bash ~/dotfiles/bootstrap/scripts/aur.sh
  # Enable Redshift:
  systemctl --user enable redshift
  # Enable LightDM:
  sudo systemctl enable lightdm

  # Use Rofi for dmenu:
  sudo ln -sf "$(which rofi)" /usr/bin/dmenu

  # Remove i3 startup scripts:
  sudo rm /usr/share/xsessions/i3*.desktop

  # Configure spicetify:
  $bash ~/dotfiles/bootstrap/scripts/spicetify.sh
fi

# Enable OpenSSH:
sudo systemctl enable sshd.service
sudo systemctl start sshd.service

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

# Enable firejail:
sudo systemctl enable --now apparmor
sudo apparmor_parser -r /etc/apparmor.d/firejail-default
sudo firecfg
sudo echo "$(whoami)" | sudo tee /etc/firejail/firejail.users > /dev/null
$bash ~/dotfiles/bootstrap/scripts/firejail.sh

if [ -z "$SERVER" ]; then
  # Configure xdg-utils
  xdg-settings set default-web-browser firefox.desktop
  xdg-mime default org.pwmt.zathura.desktop application/pdf

  # Get spotify to work with firejail:
  if [[ -e /usr/local/bin/spotify ]]; then
    sudo rm /usr/local/bin/spotify
  fi
  mkdir -p "$HOME/.local/bin/spotify"
  cat << EOF > "$HOME/.local/bin/spotify"
#!/bin/sh
firejail --env="LD_PRELOAD=/usr/lib/spotifywm.so" spotify "$@"
EOF
  chmod +x "$HOME/.local/bin/spotify"

  # Configure Seadrive:
  $bash ~/dotfiles/bootstrap/scripts/seadrive.sh
fi
