#!/usr/bin/bash -e

source ./helpers.sh

packages=(
    dstat
    rsync
    ispell
)

install_package ${packages[*]}
