#!/usr/bin/env bash

sudo apt-get update
sudo apt-get upgrade -y
# Install Necessary Tools:
sudo apt-get install -y vim fish python3-pip silversearcher-ag htop
sudo apt-get install -y ranger caca-utils highlight atool w3m poppler-utils mediainfo

# Install Golang:
sudo add-apt-repository ppa:gophers/archive
sudo apt-get update
sudo apt-get install golang-1.11-go
sudo ln -s /usr/lib/go-1.11/bin/* /usr/local/bin

# Install Node.js
sudo apt-get install -y curl python-software-properties
curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt-get update
sudo apt-get install -y nodejs
sudo apt-get install -y yarn

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
if test ! "/usr/local/bin/fasd"; then
  git clone https://github.com/clvv/fasd
  cd ~/dotfiles/fasd
  sudo make install
  cd ~/dotfiles
  rm -rf fasd
fi

# Install Bat
if ! which bat > /dev/null 2>&1; then
  curl -so ~/dotfiles/bat.deb https://github.com/sharkdp/bat/releases/download/v0.11.0/bat-musl_0.11.0_amd64.deb
  sudo dpkg -i ~/dotfiles/bat.deb
  rm ~/dotfiles/bat.deb
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

  ## Install Leiningen:
  sudo apt install -y default-jre
  sudo curl -fLo /usr/local/bin/lein \
    https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein

  ## Install Clojure Kondo
  sudo curl -L https://raw.githubusercontent.com/borkdude/clj-kondo/master/script/install-clj-kondo | bash
fi
