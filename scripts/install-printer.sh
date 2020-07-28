#!/usr/bin/bash -ex

source ./helpers.sh

install_package canon-pixma-mg5500-complete
addr=$(cnijnetprn --installer --search auto | awk '{print $2}')
$sudo lpadmin -p MG5500 -m canonmg5500.ppd -v $addr -E
