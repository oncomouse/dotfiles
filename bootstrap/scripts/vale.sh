#!/usr/bin/env bash
mkdir -p ~/.config/vale/styles
git clone https://github.com/errata-ai/proselint proselint-git
git clone https://github.com/errata-ai/write-good write-good-git
mv proselint-git/proselint ~/.config/vale/styles/
mv write-good-git/write-good ~/.config/vale/styles/
rm -rf proselint-git
rm -rf write-good-git
