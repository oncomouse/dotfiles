#!/usr/bin/env bash

## Get ready for xcode/brew:
xcode-select --install

if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

## Install Brew
brew bundle install

## Link Minimal Configuration Files
mkdir -p ~/.config/fish/functions/
mkdir -p ~/.vim/autoload/
mkdir -p ~/.config/nvim/
ln -s ~/dotfiles/.agignore ~/
ln -s ~/dotfiles/.vimrc ~/
ln -s ~/dotfiles/.config/nvim/init.vim ~/.config/nvim/
ln -s ~/dotfiles/.config/fish/config.fish ~/.config/fish/
ln -s ~/dotfiles/.config/fish/functions/fish_prompt.fish ~/.config/fish/functions/
ln -s ~/dotfiles/.tmux.conf.local ~/
ln -s ~/dotfiles/.mackup.cfg ~/
ln -s ~/dotfiles/.config/kitty/ ~/.config/

## Diff-so-fancy Git stuff
git config --global color.diff-highlight.oldNormal "red bold"
git config --global color.diff-highlight.oldHighlight "red bold 52"
git config --global color.diff-highlight.newNormal "green bold"
git config --global color.diff-highlight.newHighlight "green bold 22"

## Git Keychain:
git config --global credential.helper osxkeychain

## Install Python Modules
pip3 install mackup neovim virtualfish pylint jedi
if [! -f "/usr/local/bin/python"]; then
  ln -s "$(which python3)" /usr/local/bin/python
fi
if [! -f "/usr/local/bin/pip"]; then
  ln -s "$(which pip3)" /usr/local/bin/pip
fi

## Install Node.js Modules
npm install -g neovim jsonlint

## Setup Ruby
mkdir -p "$(rbenv root)"/plugins
git clone https://github.com/rbenv/ruby-build.git "$(rbenv root)"/plugins/ruby-build
git clone https://github.com/momo-lab/rbenv-install-latest.git "$(rbenv root)"/plugins/rbenv-install-latest
rbenv install 2.5.1 # Dreamhost Ruby
rbenv install-latest
rbenv global "$(rbenv versions | sed -e '$!d' -e 's/^[ \t]*//')"

## Setup Oh My Fish!
curl -L https://get.oh-my.fish | fish
omf install z ssh-term-helper fish-spec nodenv virtualfish

## Setup Oh My Tmux!
git clone https://github.com/gpakosz/.tmux ~/.tmux
ln -s -f ~/.tmux/.tmux.conf ~/.tmux.conf
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

## Setup Vim and NeoVim
curl -fLo ~/.vim/autoload/plug.vim \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
mkdir -p ~/.local/share/nvim/site/autoload
cp ~/.vim/autoload/plug.vim ~/.local/share/nvim/site/autoload/plug.vim
vim +PlugInstall +qall
nvim +PlugInstall +qall

## Setup $TERM
tic -x -o ~/.terminfo ~/dotfiles/terms/tmux.terminfo
tic -x -o ~/.terminfo ~/dotfiles/terms/tmux-256color.terminfo
tic -x -o ~/.terminfo ~/dotfiles/terms/xterm-256color.terminfo

## Use Fish
sudo dscl . -create /Users/$USER UserShell /usr/local/bin/fish

# Configure FZF BibTeX
go get github.com/msprev/fzf-bibtex
go install github.com/msprev/fzf-bibtex/cmd/bibtex-ls
go install github.com/msprev/fzf-bibtex/cmd/bibtex-markdown
go install github.com/msprev/fzf-bibtex/cmd/bibtex-cite

# Configure Wego (used in tmux)
go get github.com/schachmat/wego
ln -s ~/dotfiles/wego/one-liner.go ~/go/src/github.com/schachmat/wego/frontends/
ln -s ~/dotfiles/.wegorc ~/
go install github.com/schachmat/wego

# Configure wttr-safe, the failback client for wttr.in and wego:
go get github.com/oncomouse/wttr-safe
go install github.com/oncomouse/wttr-safe

echo "Run $(tput bold)$(tput setaf 6)dns/bootstrap.sh$(tput sgr0) to install DNS proxy and local dev domains."

echo "When Dropbox is configured and $(tput bold)$(tput setaf 5)done syncing$(tput sgr0), run $(tput setaf 6)mackup restore$(tput sgr0) to load configuration files."
