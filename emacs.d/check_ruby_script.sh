#!/usr/bin/env bash
# This script will remove the shebang line and pass the
# rest of the file to `ruby -c' to check for parse errors
output=$(cat $1 | sed '1s/#!.*//' | ruby -c 2>&1)
exit_status=$?
file_name=$(echo $1 | sed 's/\//\\\//g')
echo "$output" | sed "s/^-:/$file_name:/"
exit $exit_status
