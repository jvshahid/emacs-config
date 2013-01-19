#! /bin/bash

function show_usage () {
    echo "Usage: $0 <type>"

    echo "Where type is the type of the project and can be one of:"
    echo "  1. Java"
    echo "  2. Ruby"
    echo "  2. C++ (imply C and C++)"
    exit 1
}

if [ $# -ne 1 ]; then
    show_usage
fi

case $1 in
    Java|java) languages="Java,Ant";;
    Ruby|ruby) languages="Ruby";;
    C++|c++)   languages="C,C++";;
    *)         show_usage;;
esac

language_definitions="--langdef=scala --langmap=scala:.scala --langdef=otq --langmap=otq:.otq --langdef=xml --langmap=xml:.xml --langdef=properties --langmap=properties:.properties"

ctags-exuberant -R --extra=+fq -e --languages="$languages" "$language_definitions"
