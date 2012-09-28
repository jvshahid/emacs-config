#!/usr/bin/env bash

if [ $# -ne 1 ]; then
    echo "Usage $0 <fan speed>"
    exit 1
fi

echo "Setting fan speed to $1"

sudo bash -c "echo 1 > /sys/devices/platform/applesmc.768/fan1_manual"
sudo bash -c "echo 1 > /sys/devices/platform/applesmc.768/fan2_manual"
sudo bash -c "echo $1 > /sys/devices/platform/applesmc.768/fan2_output"
sudo bash -c "echo $1 > /sys/devices/platform/applesmc.768/fan1_output"
sudo bash -c "echo 2 > /sys/module/hid_apple/parameters/fnmode"
