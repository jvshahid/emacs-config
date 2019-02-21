#!/usr/bin/bash -e

source ./helpers.sh

sudo=

if [ `id -u` -ne 0 ]; then
    sudo=sudo
fi

cat <<EOF | $sudo tee /etc/udev/rules.d/10-trackpoint.rules
SUBSYSTEM=="serio", ATTR{protocol}=="TPPS/2", ATTR{sensitivity}="250"
EOF

$sudo udevadm control --reload
