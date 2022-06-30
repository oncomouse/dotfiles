#!/usr/bin/env bash
set -euxo pipefail

os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
bash=$(which bash)
env=$(which env)
echo -n "Name this target: "
read -r target
mkdir -p ~/.local/share/dotfiles
echo "$target" > ~/.local/share/dotfiles/target
case $os in
  macos)
    $env SERVER="$SERVER" "$bash" "$HOME/dotfiles/bootstrap/macos.sh"
    ;;
  ubuntu)
    $env SERVER="$SERVER" "$bash" "$HOME/dotfiles/bootstrap/ubuntu.sh"
    ;;
  arch)
    $env SERVER="$SERVER" "$bash" "$HOME/dotfiles/bootstrap/arch.sh"
    ;;
  *)
    echo "Cannot recognize your operating system: $os"
    ;;
esac
