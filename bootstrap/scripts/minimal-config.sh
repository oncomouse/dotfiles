#!/usr/bin/env bash
# Create the minimal configuration environment by removing old files and
# sym-linking new ones into their place.
#
# Also build the basic dot directory structures.

os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
# Remove any old versions of the files we create:
declare -a created_files=("$HOME/.vimrc" "$HOME/.config/nvim/init.vim" "$HOME/.config/fish/config.fish" "$HOME/.config/fish/fishfile" "$HOME/.tmux.conf.local" "$HOME/.mackup.cfg" "$HOME/.config/kitty/kitty.conf" "$HOME/.ignore")

mkdir -p ~/.backup
for created_file in "${created_files[@]}"
do
  created_file="${created_file/#\~/$HOME}"
  if test "$created_file"; then
    if ! mv "$created_file" ~/.backup 2> /dev/null; then
      rm "$created_file" > /dev/null 2>&1
    fi
  fi
done
# Delete backup if empty:
rmdir ~/.backup 2> /dev/null

## Link Minimal Configuration Files
mkdir -p ~/.config/fish/functions/
mkdir -p ~/.config/fish/completions/
mkdir -p ~/.vim/autoload/
mkdir -p ~/.config/nvim/
mkdir -p ~/.config/kitty/
ln -s ~/dotfiles/ag/agignore ~/.ignore
# Always link the server rc file for vim:
ln -s ~/dotfiles/vim/vimrc-server ~/.vimrc
# If Neovim is installed, link the correct rc file:
if command -v nvim &> /dev/null; then
  if [ -z "$SERVER" ]; then
    ln -s ~/dotfiles/vim/vimrc-desktop ~/.config/nvim/init.vim
  else
    ln -s ~/dotfiles/vim/vimrc-server ~/.config/nvim/init.vim
  fi
fi
ln -s ~/dotfiles/kitty/kitty.conf ~/.config/kitty/
if [ "$os" == "macos" ]; then
  ln -s ~/dotfiles/mackup/mackup.cfg ~/.mackup.cfg
fi
ln -sf ~/dotfiles/wal ~/.config/
