#!/usr/bin/env bash

# https://github.com/wez/wezterm/releases/download/nightly/WezTerm-macos-nightly.zip

curl -sLo ~/.local/wezterm-nightly.zip https://github.com/wez/wezterm/releases/download/nightly/WezTerm-macos-nightly.zip
owd="$(pwd)"
cd ~/.local || exit
rm -rf WezTerm*
unzip wezterm-nightly.zip
rm -rf /Applications/WezTerm.app
mv WezTer*/WezTerm.app /Applications
cd "$owd" || exit
