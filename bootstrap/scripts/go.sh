#!/usr/bin/env
## Configure gocode and gopls
if which gopls > /dev/null 2>&1; then
  # go get -u github.com/mdempsky/gocode
  # go install github.com/mdempsky/gocode
  # ~/go/bin/gocode
  # go get -u golang.org/x/tools/gopls
  # go install golang.org/x/tools/gopls
  env GO111MODULE="on" go get golang.org/x/tools/gopls@latest
fi
