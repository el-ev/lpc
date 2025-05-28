#!/bin/bash

PROJECT_ROOT="$(dirname "$(realpath "$0")")/.."

getopts "dra" opt;
if [ -z "$opt" ]; then
    echo "Usage: $0 (-d (debug) | -r (release) | -a (all))"
    exit 1
fi

case $opt in
    d)
        cmake --build "$PROJECT_ROOT/build/debug" --target test_cpp
        ;;
    r)
        cmake --build "$PROJECT_ROOT/build/release" --target test_cpp
        ;;
    a)
        cmake --build "$PROJECT_ROOT/build/debug" --target test_cpp
        cmake --build "$PROJECT_ROOT/build/release" --target test_cpp
        ;;
    *)
        echo "Usage: $0 (-d (debug) | -r (release) | -a (all))"
        exit 1
        ;;
esac
