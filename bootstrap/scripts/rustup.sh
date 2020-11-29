os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
if [ "$os" = "arch" ]; then
  rustup install stable
  rustup default stable
else
  if rustup -v COMMAND &> /dev/null; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  fi
  if [ ! -d ~/.cargo ]; then
    rustup-init
    source "$HOME/.cargo/env"
  fi
fi

