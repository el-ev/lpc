#!/bin/bash

PROJECT_ROOT="$(dirname "$(realpath "$0")")/.."
cd "$PROJECT_ROOT" || exit 1

scripts/build.sh -r > /dev/null 2>&1
if [ $? -ne 0 ]; then
    echo "Build failed"
    exit 1
fi

PASS=0
FAIL=0
FAILED_FILES=""

while IFS= read -r -d '' scm_file; do
    output=$(python3 scripts/checker.py "$scm_file" "$@" 2>&1)
    if [ $? -eq 0 ]; then
        PASS=$((PASS + 1))
        echo "$output"
    else
        FAIL=$((FAIL + 1))
        FAILED_FILES="$FAILED_FILES  $scm_file\n"
        echo "$output"
    fi
done < <(find test/scheme/ -type f -name "*.scm" -print0 | sort -z)

echo ""
echo "=== Scheme Test Summary ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"
echo "Total:  $((PASS + FAIL))"

if [ $FAIL -gt 0 ]; then
    echo ""
    echo "Failed tests:"
    echo -e "$FAILED_FILES"
    exit 1
fi
