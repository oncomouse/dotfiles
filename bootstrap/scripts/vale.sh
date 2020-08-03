#!/usr/bin/env bash
ln -sf ~/dotfiles/vale/vale.ini ~/.vale.ini
mkdir -p ~/.config/vale/styles
git clone https://github.com/errata-ai/proselint ~/.config/vale/styles
git clone https://github.com/errata-ai/write-good ~/.config/vale/styles
