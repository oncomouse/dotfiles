#!/usr/bin/env bash
## Install ASDF Plugins
asdf plugin add ruby
asdf plugin add nodejs
asdf install ruby 2.7.1
asdf install ruby 2.5.1 # Dreamhost
asdf global ruby 2.7.1
asdf global nodejs system
