#!/usr/bin/env bash
# Create the minimal configuration environment by removing old files and
# sym-linking new ones into their place.
#
# Also build the basic dot directory structures.

# Detect OS (used for Vim bundling):
os="unknown"
case `uname` in
  Darwin)
    os="macos"
    ;;
  Linux)
    os=`cat /etc/os-release | ag "^NAME" | tr -d "NAME=" | tr -d '"' | tr '[:upper:]' '[:lower:]'`
    ;;
esac
echo $os

# Remove any old versions of the files we create:
declare -a created_files=("~/.vimrc" "~/.config/nvim/init.vim" "~/.config/fish/config.fish" "~/.tmux.conf.local" "~/.muttrc" "~/.mackup.cfg" "~/.config/kitty/kitty.conf" "~/.wegorc" "~/.lein/profiles.clj" "~/.agignore")

mkdir -p ~/.backup
for created_file in "${created_files[@]}"
do
  created_file="${created_file/#\~/$HOME}"
  if test $created_file; then
    if ! mv $created_file ~/.backup 2> /dev/null; then
      rm $created_file
    fi
  fi
done
# Delete backup if empty:
rmdir ~/.backup 2> /dev/null

## Link Minimal Configuration Files
mkdir -p ~/.config/fish/functions/
mkdir -p ~/.vim/autoload/
mkdir -p ~/.config/nvim/
mkdir -p ~/.config/kitty/
mkdir -p ~/.lein/
ln -s ~/dotfiles/ag/agignore ~/.agignore
ln -s "~/dotfiles/vim/vimrc-${os}" ~/.vimrc
ln -s "~/dotfiles/vim/vimrc-${os}" ~/.config/nvim/init.vim
ln -s ~/dotfiles/fish/config.fish ~/.config/fish/
ln -sf ~/dotfiles/fish/functions/*.fish ~/.config/fish/functions/
ln -s ~/dotfiles/tmux/tmux.conf.local ~/.tmux.conf.local
ln -s ~/dotfiles/mutt/muttrc ~/.muttrc
ln -s ~/dotfiles/mackup/mackup.cfg ~/.mackup.cfg
ln -s ~/dotfiles/kitty/kitty.conf ~/.config/kitty/
ln -s ~/dotfiles/leinengen/profiles.clj  ~/.lein/
ln -s ~/dotfiles/wego/wegorc ~/.wegorc

