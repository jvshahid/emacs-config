#!/usr/bin/bash -e

source ./helpers.sh

install_package cups \
                system-config-printer \
                foomatic-db-engine \
                foomatic-db-nonfree-ppds

systemctl enable org.cups.cupsd.service
systemctl start org.cups.cupsd.service

usermod -a -G sys jvshahid
