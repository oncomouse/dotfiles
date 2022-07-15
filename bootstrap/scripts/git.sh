#!/usr/bin/env bash
git config --global user.name "oncomouse"
git config --global user.email "oncomouse@gmail.com"
# Encrypted netrc credential helper
if ! command -v git-credential-netrc > /dev/null; then
	owd="$(pwd)"
	git clone https://github.com/git/git ~/.cache/git-project
	cd ~/.cache/git-project/contrib/credential/netrc || exit
	make
	cp git-credential-netrc ~/.local/bin
	cd "$owd" || exit
	rm -rf ~/.cache/git
	git config --global credential.helper netrc
fi
git config --global pull.rebase false
git config --global init.defaultBranch master
git config --global core.editor "nvim"
