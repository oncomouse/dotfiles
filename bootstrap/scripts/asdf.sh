#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
# Install ASDF for Linux:
if [[ $os != "macos" ]]; then
  git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.0
  . "$HOME/.asdf/asdf.sh"
  mkdir -p "$HOME/.config/fish/completions"
  ln -sf ~/.asdf/completions/asdf.fish ~/.config/fish/completions/
fi
ruby_env=$([ "$os" == "macos" ] && echo "--with-openssl-dir=$(brew --prefix)/opt/openssl@1.1")
asdf plugin add ruby
asdf plugin add nodejs
# asdf plugin add lua https://github.com/Stratus3D/asdf-lua.git
env RUBY_CONFIGURE_OPTS="$ruby_env" asdf install ruby 2.7.2
asdf global ruby system
asdf global nodejs system
# asdf install lua 5.1.5
# asdf install lua 5.3.5
# if [[ $os == "arch" ]]; then
#   asdf global lua 5.3.5
# elif [[ $os == "macos" ]]; then
#   asdf global lua 5.1.5
# fi
# ## Setup Ruby
# if test ! "$(rbenv root)/plugins" ; then
#   mkdir -p "$(rbenv root)"/plugins
#   git clone https://github.com/rbenv/ruby-build.git "$(rbenv root)"/plugins/ruby-build
#   git clone https://github.com/momo-lab/rbenv-install-latest.git "$(rbenv root)"/plugins/rbenv-install-latest
#   rbenv install -s 2.5.1 # Dreamhost Ruby
#   rbenv install-latest
#   rbenv global "$(rbenv versions | sed -e '$!d' -e 's/^[ \t]*//')"
# fi
