#!/bin/bash

PROJECT_ROOT="$(dirname "$(realpath "$0")")/.."

getopts "dra" opt;
if [ -z "$opt" ]; then
    echo "Usage: $0 (-d (debug) | -r (release) | -a (all))"
    exit 1
fi

case $opt in
    d)
        cmake --build "$PROJECT_ROOT/build/debug"
        ;;
    r)
        cmake --build "$PROJECT_ROOT/build/release"
        ;;
    a)
        cmake --build "$PROJECT_ROOT/build/debug"
        cmake --build "$PROJECT_ROOT/build/release"
        ;;
    *)
        echo "Usage: $0 (-d (debug) | -r (release) | -a (all))"
        exit 1
        ;;
esac
 