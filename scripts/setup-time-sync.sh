#!/usr/bin/bash

cd $(dirname $0)

pacman -S --needed --noconfirm chrony

cat <<EOF >> /etc/chrony.conf
server 0.pool.ntp.org iburst
server 1.pool.ntp.org iburst
server 2.pool.ntp.org iburst
server 3.pool.ntp.org iburst
rtconutc
rtcsync
EOF

systemctl enable chronyd.service