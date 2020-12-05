#!/usr/bin/env bash
# Install NNN Plugins:
if [[ ! -d "$HOME/.config/nnn/plugins" ]];then
  curl -Ls https://raw.githubusercontent.com/jarun/nnn/master/plugins/getplugs | sh
fi
fish -c "set -Ux NNN_PLUG 'p:fzcd;v:imgview'"
