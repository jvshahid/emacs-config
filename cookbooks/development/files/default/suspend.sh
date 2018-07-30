#!/usr/bin/env bash
cinnamon-screensaver-command -l & while  cinnamon-screensaver-command -q | grep inactive >/dev/null 2>&1; do echo waiting for screensaver; done && sudo pm-suspend
