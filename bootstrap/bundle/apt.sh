#!/usr/bin/env bash

sudo apt-get update
sudo apt-get upgrade -y
# Install Necessary Tools:
sudo apt-get install -y vim fish python3-pip silversearcher-ag htop
sudo apt-get install -y caca-utils highlight atool w3m poppler-utils mediainfo

# Install Golang:
if ! which go > /dev/null 2>&1; then
  sudo add-apt-repository ppa:gophers/archive
  sudo apt-get update
  sudo apt-get install golang-1.11-go
  sudo ln -s /usr/lib/go-1.11/bin/* /usr/local/bin
fi

# Install Node.js
if ! which node > /dev/null 2>&1; then
  sudo apt-get install -y curl python-software-properties
  curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -
  curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
  echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
  sudo apt-get update
  sudo apt-get install -y nodejs
  sudo apt-get install -y yarn
fi

# Install FZF (configured in fish ctrl+r & ctrl+t):
if test ! ~/.fzf; then
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install
fi

# Build and Install Tmux
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
if ! which fasd > /dev/null 2>&1; then
  git clone https://github.com/clvv/fasd
  cd ~/dotfiles/fasd
  sudo make install
  cd ~/dotfiles
  rm -rf fasd
fi

# Install Bat
if ! which bat > /dev/null 2>&1; then
  curl -sLo ~/dotfiles/bat.deb https://github.com/sharkdp/bat/releases/download/v0.11.0/bat-musl_0.11.0_amd64.deb
  sudo dpkg -i ~/dotfiles/bat.deb
  rm ~/dotfiles/bat.deb
fi

# Install fd
if ! which fd > /dev/null 2>&1; then
  curl -sLo ~/dotfiles/fd.deb https://github.com/sharkdp/fd/releases/download/v7.3.0/fd-musl_7.3.0_amd64.deb
  sudo dpkg -i ~/dotfiles/fd.deb
  rm ~/dotfiles/fd.deb
fi

if ! which exa > /dev/null 2>&1; then
  curl -sLo ~/dotfiles/exa.zip https://github.com/ogham/exa/releases/download/v0.9.0/exa-linux-x86_64-0.9.0.zip
  sudo apt-get install -y unzip
  unzip ~/dotfiles/exa.zip
  sudo mv exa-linux-x86_64 /usr/local/bin/exa
  rm ~/dotfiles/exa.zip
fi

if [ -z $SERVER ]; then
  sudo apt-get install -y pandoc pandoc-citeproc diction bibtool
  sudo apt-get install -y firefox

  ## Install Seafile Cilent
  sudo add-apt-repository ppa:seafile/seafile-client
  sudo apt-get update
  sudo apt-get -y install seafile-gui

  ## Install Virtualbox
  wget -q https://www.virtualbox.org/download/oracle_vbox_2016.asc -O- | sudo apt-key add -
  sudo add-apt-repository "deb [arch=amd64] http://download.virtualbox.org/virtualbox/debian $(lsb_release -cs) contrib"
  sudo apt-get update
  sudo apt-get -y install virtualbox-6.0

  ## Install RBEnv:
  sudo apt-get -y install autoconf bison build-essential libssl-dev libyaml-dev libreadline6-dev zlib1g-dev libncurses5-dev libffi-dev libgdbm5 libgdbm-dev
  git clone https://github.com/rbenv/rbenv.git ~/.rbenv
  echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc
  echo 'eval "$(rbenv init -)"' >> ~/.bashrc
  source ~/.bashrc

  ## Install NeoVim:
  sudo add-apt-repository ppa:neovim-ppa/stable
  sudo apt-get update
  sudo apt-get install -y neovim
fi
