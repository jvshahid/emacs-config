#!/usr/bin/bash -e

ln -sf /etc/fonts/conf.avail/11-lcdfilter-default.conf /etc/fonts/conf.d
ln -sf /etc/fonts/conf.avail/70-no-bitmaps.conf /etc/fonts/conf.d

cat <<ANTIALIASING > /etc/fonts/conf.avail/10-antialias.conf
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
<!--  Use the Antialiasing -->
  <match target="font">
    <edit name="antialias" mode="assign"><bool>true</bool></edit>
  </match>
</fontconfig>
ANTIALIASING

ln -sf /etc/fonts/conf.avail/10-antialias.conf /etc/fonts/conf.d

cat <<HINTING > /etc/fonts/conf.avail/10-hinting.conf
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
<!--  Use Hinting -->
  <match target="font">
    <edit name="hinting" mode="assign"><bool>true</bool></edit>
  </match>
</fontconfig>
HINTING

ln -sf /etc/fonts/conf.avail/10-hinting.conf /etc/fonts/conf.d
