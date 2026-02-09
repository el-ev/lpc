#!/bin/bash

PROJECT_ROOT="$(dirname "$(realpath "$0")")/.."

# Portable CPU count (macOS doesn't have nproc)
if command -v nproc &> /dev/null; then
    JOBS=$(nproc)
else
    JOBS=$(sysctl -n hw.ncpu 2>/dev/null || echo 4)
fi

getopts "dra" opt;
if [ -z "$opt" ]; then
    echo "Usage: $0 (-d (debug) | -r (release) | -a (all))"
    exit 1
fi

case $opt in
    d)
        cmake --build "$PROJECT_ROOT/build/debug" -j "$JOBS" --target lpc
        ;;
    r)
        cmake --build "$PROJECT_ROOT/build/release" -j "$JOBS" --target lpc
        ;;
    a)
        cmake --build "$PROJECT_ROOT/build/debug" -j "$JOBS" --target lpc
        cmake --build "$PROJECT_ROOT/build/release" -j "$JOBS" --target lpc
        ;;
    *)
        echo "Usage: $0 (-d (debug) | -r (release) | -a (all))"
        exit 1
        ;;
esac
