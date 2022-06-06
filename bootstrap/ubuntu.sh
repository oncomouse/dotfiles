#!/usr/bin/env bash

sudo apt-get update
sudo apt-get upgrade -y
# Install Necessary Tools:
sudo apt-get install -y vim fish python3-pip htop
sudo apt-get install -y caca-utils highlight atool w3m poppler-utils mediainfo

# # Install Node.js
# if ! which node > /dev/null 2>&1; then
#   sudo apt-get install -y curl python-software-properties
#   curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -
#   curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
#   echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
#   sudo apt-get update
#   sudo apt-get install -y nodejs
#   sudo apt-get install -y yarn
# fi

# Install FZF (configured in fish ctrl+r & ctrl+t):
if test ! ~/.fzf; then
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install
fi

package_version() {
  regex='^http'
  if [[ $1 =~ $regex ]]; then
    url=$1
  else
    url="https://github.com/${1}"
  fi
  pkgver=$(git ls-remote --sort="version:refname" --tags "${url}" | tail -n 1 | cut -d "/" -f3 | sed -e "s/v//" | tr -d '\n')
  echo -n "$pkgver"
}

# Install Bat
if ! which bat > /dev/null 2>&1; then
  pkgver=$(package_version sharkdp/bat)
  curl -sLo ~/dotfiles/bat.deb https://github.com/sharkdp/bat/releases/download/v"${pkgver}"/bat-musl_"${pkgver}"_amd64.deb
  sudo dpkg -i ~/dotfiles/bat.deb
  rm ~/dotfiles/bat.deb
fi

# Install fd
if ! which fd > /dev/null 2>&1; then
  pkgver=$(package_version sharkdp/fd)
  curl -sLo ~/dotfiles/fd.deb https://github.com/sharkdp/fd/releases/download/v"${pkgver}"/fd-musl_"${pkgver}"_amd64.deb
  sudo dpkg -i ~/dotfiles/fd.deb
  rm ~/dotfiles/fd.deb
fi

if ! which exa > /dev/null 2>&1; then
  pkgver=$(package_version ogham/exa)
  curl -sLo ~/dotfiles/exa.zip https://github.com/ogham/exa/releases/download/v"${pkgver}"/exa-linux-x86_64-"${pkgver}".zip
  sudo apt-get install -y unzip
  unzip ~/dotfiles/exa.zip
  sudo mv exa-linux-x86_64 /usr/local/bin/exa
  rm ~/dotfiles/exa.zip
fi

if ! which rg > /dev/null 2>&1; then
  pkgver=$(package_version BurntSushi/ripgrep)
  curl -sLo ~/dotfiles/rg.deb https://github.com/BurntSushi/ripgrep/releases/download/v"${pkgver}"/ripgrep_"${pkgver}"_amd64.deb
  sudo dpkg -i ~/dotfiles/rg.deb
  rm ~/dotfiles/rg.deb
fi

if [ -z "$SERVER" ]; then
  sudo apt-get install -y pandoc pandoc-citeproc diction bibtool
  sudo apt-get install -y firefox

  ## Install Seafile Cilent
  sudo add-apt-repository ppa:seafile/seafile-client
  sudo apt-get update
  sudo apt-get -y install seafile-gui

  ## Install ASDF:
  sudo apt-get -y install autoconf bison build-essential libssl-dev libyaml-dev libreadline6-dev zlib1g-dev libncurses5-dev libffi-dev libgdbm5 libgdbm-dev
  git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.0
  echo ". $HOME/.asdf/asdf.sh" >> ~/.bashrc
  echo ". $HOME/.asdf/completions/asdf.bash" >. ~/.bashrc
  source "$HOME/.bashrc"

  ## Install NeoVim:
  sudo add-apt-repository ppa:neovim-ppa/stable
  sudo apt-get update
  sudo apt-get install -y neovim
fi

~/dotfiles/bootstrap/scripts/common.sh

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

if ! echo "$SHELL" | ag fish > /dev/null 2>&1; then
  sudo chsh -s "$(which fish)" "$USER"
fi

# Clean up
sudo apt -y autoremove

# IP Sec features:
echo "Copy your SSH key(s) and turn off Root/Password login. Add IP Hardening"
echo ""
echo "    $(tput setaf 6)https://dennisnotes.com/note/20180627-ubuntu-18.04-server-setup/$(tput sgr0)"
