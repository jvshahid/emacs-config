#!/usr/bin/env bash

# remove all flymake files
find -name '*flymake*' -delete

# remove emacs temporary files
find -name '#*' -delete
find -name '*~' -delete

# remove go test files
find -name '*.test' -delete

