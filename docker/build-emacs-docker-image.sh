#!/usr/bin/env bash

set -e

docker pull ubuntu:latest
docker run --security-opt seccomp=unconfined --name build-emacs -v $PWD:/scripts -it ubuntu:latest /scripts/build-emacs.sh
docker commit build-emacs jvshahid/emacs:latest
docker rm -f build-emacs
