#!/usr/bin/env bash

# [ ! -f ./exports.sh ] && echo "Please create a exports.sh file" && exit 1
# . ./exports.sh

export PATH=$PATH:$GOPATH/bin
export GOROOT

pkill -f gocode
go get code.google.com/p/go.tools/cmd/goimports
go get -u code.google.com/p/rog-go/exp/cmd/godef
go get -u code.google.com/p/go.tools/cmd/godoc
go get -u github.com/nsf/gocode
emacs-snapshot --no-desktop &
