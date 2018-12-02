#!/usr/bin/bash -e

cd $(dirname $0)

source ./helpers.sh

install_package chrony

cat <<EOF >> /etc/chrony.conf
server 0.pool.ntp.org iburst
server 1.pool.ntp.org iburst
server 2.pool.ntp.org iburst
server 3.pool.ntp.org iburst
rtconutc
rtcsync
EOF

systemctl enable chronyd.service
