#!/bin/bash

PROJECT_ROOT="$(dirname "$(realpath "$0")")/.."

getopts "dra" opt;
if [ -z "$opt" ]; then
    echo "Usage: $0 (-d (debug) | -r (release) | -a (all))"
    exit 1
fi

case $opt in
    d)
        cmake --build "$PROJECT_ROOT/build/debug" -j "$(nproc)"
        ;;
    r)
        cmake --build "$PROJECT_ROOT/build/release" -j "$(nproc)"
        ;;
    a)
        cmake --build "$PROJECT_ROOT/build/debug" -j "$(nproc)"
        cmake --build "$PROJECT_ROOT/build/release" -j "$(nproc)"
        ;;
    *)
        echo "Usage: $0 (-d (debug) | -r (release) | -a (all))"
        exit 1
        ;;
esac
 