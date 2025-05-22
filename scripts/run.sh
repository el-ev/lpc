#!/bin/bash

PROJECT_ROOT="$(dirname "$(realpath "$0")")/.."

if [[ "$1" != "-d" && "$1" != "-r" ]]; then
    echo "Usage: $0 (-d|-r) [args]"
    exit 1
fi

BUILD_TYPE="debug"
if [[ "$1" == "-r" ]]; then
    BUILD_TYPE="release"
fi

"$PROJECT_ROOT/scripts/build.sh" "$1"
if [[ $? -ne 0 ]]; then
    echo "Build failed"
    exit 1
fi

echo ""

"$PROJECT_ROOT/build/$BUILD_TYPE/lpc" "${@:2}"