#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
if [ ! -d ~/.cargo ]; then
  rustup-init
fi
git https://github.com/hrkfdn/ncspot ~/ncspot
if [ "$os" == "macos" ]; then
  cargo install --path ~/ncspot --no-default-features --features portaudio_backend,cursive/pancurses-backend,mpris,share_clipboard,notify
else
  sudo apt install libncursesw5-dev libdbus-1-dev libpulse-dev libssl-dev libxcb1-dev libxcb-render0-dev libxcb-shape0-dev libxcb-xfixes0-dev
  cargo install --path ~/ncspot
fi
if [ "$os" == "macos" ]; then
  mkdir -p ~/Library/Preferences/org.affekt.ncspot/
  ln -sf ~/dotfiles/ncspot/config.toml ~/Library/Preferences/org.affekt.ncspot/config.toml
else
  mkdir -p ~/.config/ncspot
  ln -sf ~/dotfiles/ncspot/config.toml ~/.config/ncspot/config.toml
fi
rm -rf ~/ncspot
