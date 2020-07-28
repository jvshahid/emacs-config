#!/usr/bin/bash -e

source ./helpers.sh

install_package rsyslog

systemctl enable rsyslog
systemctl start rsyslog
