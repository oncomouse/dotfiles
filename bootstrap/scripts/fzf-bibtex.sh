#!/usr/bin/env bash
## Configure FZF BibTeX (used in Vim)
if test ! "~/go/src/github.com/msprev/fzf-bibtex" ; then
  go get github.com/msprev/fzf-bibtex
  go install github.com/msprev/fzf-bibtex/cmd/bibtex-ls
  go install github.com/msprev/fzf-bibtex/cmd/bibtex-markdown
  go install github.com/msprev/fzf-bibtex/cmd/bibtex-cite
fi
