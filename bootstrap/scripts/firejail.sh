#!/bin/bash

cat << EOF | sudo tee /etc/firejail/globals.local > /dev/null
whitelist ${HOME}/dotfiles
EOF

# Set Firefox as default browser:
cat << EOF | sudo tee /etc/firejail/firefox.local > /dev/null
whitelist ${HOME}/.local/share/applications
EOF
