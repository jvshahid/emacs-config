#!/usr/bin/env bash
output=$(cat $1 | sed '1s/#!.*//' | ruby -c 2>&1)
exit_status=$?
file_name=$(echo $1 | sed 's/\//\\\//g')
echo $output | sed "s/^-:/$file_name:/"
exit $exit_status
