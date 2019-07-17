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

# override the acceleration multiplier introduced in this commit:
# https://github.com/wayland-project/libinput/commit/305387be
$sudo mkdir -p /etc/libinput/
cat <<EOF | $sudo tee /etc/libinput/local-overrides.quirks
[Lenovo X230 Trackpoint]
MatchName=*TPPS/2 IBM TrackPoint
MatchDMIModalias=dmi:*svnLENOVO:*pvrThinkPadX230:*
AttrTrackpointMultiplier=1.0
EOF
