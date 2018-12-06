#!/usr/bin/bash -e

ln -sf /usr/share/zoneinfo/US/Eastern /etc/localtime
hwclock --systohc

sed -i 's/^#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen

# this is what it used to look like
cat <<LOCALE > /etc/locale.conf
LANG=en_US.UTF-8
LANGUAGE=
LC_CTYPE="en_US.UTF-8"
LC_NUMERIC="en_US.UTF-8"
LC_TIME="en_US.UTF-8"
LC_COLLATE="en_US.UTF-8"
LC_MONETARY="en_US.UTF-8"
LC_MESSAGES="en_US.UTF-8"
LC_PAPER="en_US.UTF-8"
LC_NAME="en_US.UTF-8"
LC_ADDRESS="en_US.UTF-8"
LC_TELEPHONE="en_US.UTF-8"
LC_MEASUREMENT="en_US.UTF-8"
LC_IDENTIFICATION="en_US.UTF-8"
LC_ALL=
LOCALE

locale-gen

echo 'amun' > /etc/hostname

cat > /etc/hosts <<HOSTS
127.0.0.1     localhost
::1           localhost
127.0.1.1     amun.localdomain amun
HOSTS
