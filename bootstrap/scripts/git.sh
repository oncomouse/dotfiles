#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
git config --global user.name "oncomouse"
git config --global user.email "oncomouse@gmail.com"
if [ "$os" == "macos" ]; then
  git config --global credential.helper osxkeychain
else
  git config --global credential.helper /usr/lib/git-core/git-credential-libsecret
fi
git config --global pull.rebase false
git config --global init.defaultBranch master
if [ -z "$SERVER" ]; then
  git config --global core.editor "nvim"
else
  git config --global core.editor "vim"
fi
