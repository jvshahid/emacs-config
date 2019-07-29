#!/usr/bin/bash -e

source ./helpers.sh

file=/usr/share/pulseaudio/alsa-mixer/paths/analog-input-internal-mic.conf

if [ ! -f ${file}.bak ]; then
    $sudo cp $file ${file}.bak
fi

$sudo sed -i '/\[Element Internal Mic Boost\]/,/^$/d' $file
$sudo sed -i '/\[Element Int Mic Boost\]/,/^$/d' $file
$sudo sed -i '/\[Element Mic Boost\]/,/^$/d' $file

cat <<EOF | $sudo tee -a $file
[Element Internal Mic Boost]
required-any = any
switch = select
volume = zero
override-map.1 = all
override-map.2 = all-left,all-right

[Element Int Mic Boost]
required-any = any
switch = select
volume = zero
override-map.1 = all
override-map.2 = all-left,all-right

[Element Mic Boost]
switch = off
volume = zero
EOF

file=/usr/share/pulseaudio/alsa-mixer/paths/analog-input-mic.conf

if [ ! -f ${file}.bak ]; then
    $sudo cp $file ${file}.bak
fi

$sudo sed -i '/\[Element Internal Mic Boost\]/,/^$/d' $file
$sudo sed -i '/\[Element Int Mic Boost\]/,/^$/d' $file
$sudo sed -i '/\[Element Mic Boost\]/,/^$/d' $file

cat <<EOF | $sudo tee -a $file
[Element Mic Boost]
required-any = any
switch = select
volume = zero
override-map.1 = all
override-map.2 = all-left,all-right

[Element Internal Mic Boost]
switch = off
volume = zero
EOF

