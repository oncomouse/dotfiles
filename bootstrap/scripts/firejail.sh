#!/bin/bash

# Enable firejail:
sudo systemctl enable --now apparmor
sudo apparmor_parser -r /etc/apparmor.d/firejail-default
sudo firecfg
whoami | sudo tee /etc/firejail/firejail.users > /dev/null

# Whitelist dotfiles:
cat << EOF | sudo tee /etc/firejail/globals.local > /dev/null
whitelist ${HOME}/dotfiles
EOF

# Whitelist Firefox app defaults:
cat << EOF | sudo tee /etc/firejail/firefox.local > /dev/null
whitelist ${HOME}/.local/share/applications
EOF
