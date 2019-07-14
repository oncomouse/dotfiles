#!/usr/bin/env bash

sudo apt-get update
sudo apt-get upgrade -y
# Install Necessary Tools:
sudo apt-get -y install vim fish python3-pip silversearcher-ag

# Install Node.js
sudo apt-get -y curl python-software-properties
curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -
sudo apt-get install -y nodejs

# Install thefuck and virtualfish support:
if ! pip3 list | ag "thefuck" > /dev/null 2>&1; then
  pip3 install thefuck virtualfish
fi

# Install FZF (configured in fish ctrl+r & ctrl+t):
if test ! ~/.fzf; then
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install
fi

# Build current TMUX
if ! which tmux > /dev/null 2>&1; then
  sudo apt -y install automake build-essential pkg-config libevent-dev libncurses5-dev bison
  git clone https://github.com/tmux/tmux.git ~/dotfiles/tmux
  cd ~/dotfiles/tmux
  git checkout tags/2.9a
  ./autogen.sh; ./configure; make
  sudo make install
  sudo apt -y remove automake build-essential pkg-config libevent-dev libncurses5-dev bison
  cd ~/dotfiles
  rm -rf tmux
fi

# Install Fasd
if test ! "/usr/local/bin/fasd"; then
  git clone https://github.com/clvv/fasd
  cd ~/dotfiles/fasd
  sudo make install
  cd ~/dotfiles
  rm -rf fasd
fi

# Install Golang:
sudo add-apt-repository ppa:gophers/archive
sudo apt-get update
sudo apt-get install golang-1.11-go
sudo ln -s /usr/lib/go-1.11/bin/* /usr/bin

# Setup Git:
git config --global user.name "oncomouse"
git config --global user.email "oncomouse@gmail.com"

$bash ~/dotfiles/bootstrap/scripts/vim.sh
$bash ~/dotfiles/bootstrap/scripts/node-modules.sh
$bash ~/dotfiles/bootstrap/scripts/oh-my-fish.sh
$bash ~/dotfiles/bootstrap/scripts/tmux.sh
$bash ~/dotfiles/bootstrap/scripts/terms.sh
$bash ~/dotfiles/bootstrap/scripts/go.sh
$bash ~/dotfiles/bootstrap/scripts/diff-so-fancy.sh

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

# Clean up
sudo apt -y autoremove

# IP Sec features:
echo "Copy your SSH key(s) and turn off Root/Password login. Add IP Hardening"
echo ""
echo "    $(tput setaf 6)https://dennisnotes.com/note/20180627-ubuntu-18.04-server-setup/$(tput sgr0)"
