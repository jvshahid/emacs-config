#!/usr/bin/env bash

# [ ! -f ./exports.sh ] && echo "Please create a exports.sh file" && exit 1
# . ./exports.sh

export PATH
export GOPATH=`pwd`
export GOROOT

pkill -f gocode
go get code.google.com/p/go.tools/cmd/goimports
go get -u code.google.com/p/rog-go/exp/cmd/godef
go get -u code.google.com/p/go.tools/cmd/godoc
go get -u github.com/nsf/gocode
cp bin/godef ~/bin/
cp bin/gocode ~/bin/
emacs-snapshot --no-desktop &
