#!/usr/bin/env bash

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

usage() {
    echo "Usage: $0 (-d (debug) | -r (release) | -a (all))"
}

if ! getopts ":dra" opt || [[ $# -ne 1 ]]; then
    usage >&2
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
        usage >&2
        exit 1
        ;;
esac
