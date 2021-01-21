#!/usr/bin/env bash
# Install NNN Plugins:
curl -Ls https://raw.githubusercontent.com/jarun/nnn/master/plugins/getplugs | sh
fish -c "setuvar NNN_PLUG 'p:fzcd;v:imgview;o:fzopen;z:fzz'"
# Make the fzopen plugin work:
sed -i -e "s/biL/bL/g" ~/.config/nnn/plugins/fzopen
sed -i -e "s/\"\${VISUAL:-\$EDITOR}\"/nvim/g" ~/.config/nnn/plugins/fzopen
