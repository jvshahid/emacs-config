#!/usr/bin/env bash

set -e

trap 'docker rm -f build-emacs' EXIT

docker pull ubuntu:latest
docker run --security-opt seccomp=unconfined --name build-emacs -v $PWD:/scripts -it ubuntu:latest /scripts/build-emacs.sh
docker commit build-emacs jvshahid/emacs:latest
docker push jvshahid/emacs
docker build -t jvshahid/development .
docker push jvshahid/development
