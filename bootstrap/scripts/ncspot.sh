#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
if [ "$os" != "arch" ]; then
  git clone https://github.com/hrkfdn/ncspot ~/ncspot
fi
if [ "$os" == "macos" ]; then
  cargo install --path ~/ncspot --no-default-features --features portaudio_backend,cursive/pancurses-backend,mpris,share_clipboard,notify
elif [ "$os" == "ubuntu" ]; then
  sudo apt install libncursesw5-dev libdbus-1-dev libpulse-dev libssl-dev libxcb1-dev libxcb-render0-dev libxcb-shape0-dev libxcb-xfixes0-dev
  cargo install --path ~/ncspot
fi
rm -rf ~/ncspot
