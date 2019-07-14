#!/usr/bin/env bash

## Get ready for xcode/brew:
xcode-select --install

if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
else
  brew update
fi

## Install Brew
brew bundle install

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
ln -s ~/dotfiles/vim/vimrc ~/.vimrc
ln -s ~/dotfiles/vim/vimrc ~/.config/nvim/init.vim
ln -s ~/dotfiles/fish/config.fish ~/.config/fish/
ln -sf ~/dotfiles/fish/functions/*.fish ~/.config/fish/functions/
ln -s ~/dotfiles/tmux/tmux.conf.local ~/.tmux.conf.local
ln -s ~/dotfiles/mutt/muttrc ~/.muttrc
ln -s ~/dotfiles/mackup/mackup.cfg ~/.mackup.cfg
ln -s ~/dotfiles/kitty/kitty.conf ~/.config/kitty/
ln -s ~/dotfiles/leinengen/profiles.clj  ~/.lein/
ln -s ~/dotfiles/wego/wegorc ~/.wegorc

# Mail Setup
mkdir -p ~/.mutt
mkdir -p ~/.passwords

# GnuPG Setup
mkdir -p ~/.gnupg
echo "pinentry-program /usr/local/bin/pinentry-mac" >> ~/.gnupg/gpg-agent.conf

## Diff-so-fancy Git stuff
git config --global color.diff-highlight.oldNormal "red bold"
git config --global color.diff-highlight.oldHighlight "red bold 52"
git config --global color.diff-highlight.newNormal "green bold"
git config --global color.diff-highlight.newHighlight "green bold 22"

## Git Keychain:
git config --global credential.helper osxkeychain

## Install Python Modules
pip3 install mackup neovim virtualfish pylint jedi
if test ! "/usr/local/bin/python"; then
  ln -s "$(which python3)" /usr/local/bin/python
fi
if test ! "/usr/local/bin/pip"; then
  ln -s "$(which pip3)" /usr/local/bin/pip
fi

## Install Node.js Modules
npm install -g neovim jsonlint

## Setup Ruby
if test ! "$(rbenv root)/plugins" ; then
  mkdir -p "$(rbenv root)"/plugins
  git clone https://github.com/rbenv/ruby-build.git "$(rbenv root)"/plugins/ruby-build
  git clone https://github.com/momo-lab/rbenv-install-latest.git "$(rbenv root)"/plugins/rbenv-install-latest
  rbenv install -s 2.5.1 # Dreamhost Ruby
  rbenv install-latest
  rbenv global "$(rbenv versions | sed -e '$!d' -e 's/^[ \t]*//')"
fi

## Setup Oh My Fish!
if test ! ~/.local/share/omf ; then
  curl -L https://get.oh-my.fish | fish
  omf install fasd ssh-term-helper fish-spec nodenv virtualfish
else
  # Sometimes conf.d/omf.fish gets deleted by mistake, but OMF is installed:
  if test ! ~/.config/fish/conf.d/omf.fish; then
    curl -sL https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/templates/config.fish -o ~/.config/fish/conf.d/omf.fish
  fi
fi

## Setup Oh My Tmux!
if test ! ~/.tmux ; then
  git clone https://github.com/gpakosz/.tmux ~/.tmux
  ln -s -f ~/.tmux/.tmux.conf ~/.tmux.conf
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

## Setup Vim and NeoVim
if test ! "~/.vim/autoload/plug.vim"; then
  curl -fLo ~/.vim/autoload/plug.vim \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  mkdir -p ~/.local/share/nvim/site/autoload
  cp ~/.vim/autoload/plug.vim ~/.local/share/nvim/site/autoload/plug.vim
  vim +PlugInstall +qall
  nvim +PlugInstall +qall
else
  vim +PlugInstall +PlugClean +qall
  nvim +PlugInstall +PlugClean +qall
fi

## Setup $TERM
if test ! "~/.terminfo" ; then
  tic -x -o ~/.terminfo ~/dotfiles/terms/tmux.terminfo
  tic -x -o ~/.terminfo ~/dotfiles/terms/tmux-256color.terminfo
  tic -x -o ~/.terminfo ~/dotfiles/terms/xterm-256color.terminfo
fi

## Use Fish
sudo dscl . -create /Users/$USER UserShell /usr/local/bin/fish

# Configure FZF BibTeX
if test ! "~/go/src/github.com/msprev/fzf-bibtex" ; then
  go get github.com/msprev/fzf-bibtex
  go install github.com/msprev/fzf-bibtex/cmd/bibtex-ls
  go install github.com/msprev/fzf-bibtex/cmd/bibtex-markdown
  go install github.com/msprev/fzf-bibtex/cmd/bibtex-cite
fi

# Configure Wego (used in tmux)
if test ! "~/go/src/github.com/schachmat/wego" ; then
  go get github.com/schachmat/wego
  ln -s ~/dotfiles/wego/one-liner.go ~/go/src/github.com/schachmat/wego/frontends/
  go install github.com/schachmat/wego
fi

# Configure wttr-safe, the failback client for wttr.in and wego:
if test ! "~/go/src/github.com/oncomouse/wttr-safe" ; then
  go get github.com/oncomouse/wttr-safe
  go install github.com/oncomouse/wttr-safe
fi

# Configure gocode and gopls
if test ! "~/go/src/github.com/mdempsky/gocode" ; then
  go get -u github.com/mdempsky/gocode
  go install github.com/mdempsky/gocode
  ~/go/bin/gocode
  go get -u golang.org/x/tools/gopls
  go install golang.org/x/tools/gopls
fi

# Install CSL support:
if test ! "~/.csl" ; then
  git clone https://github.com/citation-style-language/styles ~/.csl
fi

echo "Run $(tput bold)$(tput setaf 6)bash ~/dotfiles/dns/bootstrap.sh$(tput sgr0) to install DNS proxy and local dev domains."

echo "Run $(tput bold)$(tput setaf 6)fish ~/dotfiles/mutt/make-gpg-keys.fish$(tput sgr0) to create GPG keys for NeoMutt (make sure Firefox & Bitwarden are working first)!"

echo "In Seafile, sync $(tput bold)Mackup$(tput sgr0) and when $(tput bold)$(tput setaf 5)done syncing$(tput sgr0), run $(tput setaf 6)mackup restore$(tput sgr0) to load configuration files."
