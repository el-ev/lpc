#!/bin/sh


scripts/build.sh -r > /dev/null
find test/scheme/ -type f -name "*.scm" -exec scripts/checker.py {} "$@" \;
