#!/usr/bin/env bash

set -e

trap 'docker rm -f build-emacs' EXIT

base_image=ubuntu:16.04
docker pull $base_image
docker run --security-opt seccomp=unconfined --name build-emacs -v $PWD:/scripts -it $base_image /scripts/build-emacs.sh
docker commit build-emacs jvshahid/emacs:latest
docker push jvshahid/emacs
docker build -t jvshahid/development .
docker push jvshahid/development
