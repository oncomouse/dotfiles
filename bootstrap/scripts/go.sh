#!/usr/bin/env
## Configure gocode and gopls
if test ! "~/go/src/github.com/mdempsky/gocode" ; then
  go get -u github.com/mdempsky/gocode
  go install github.com/mdempsky/gocode
  ~/go/bin/gocode
  go get -u golang.org/x/tools/gopls
  go install golang.org/x/tools/gopls
fi
