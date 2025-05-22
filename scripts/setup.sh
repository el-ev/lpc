#!/bin/bash

PROJECT_ROOT="$(dirname "$(realpath "$0")")/.."

cmake -B "$PROJECT_ROOT/build/debug" -DCMAKE_BUILD_TYPE=Debug -G Ninja
cmake -B "$PROJECT_ROOT/build/release" -G Ninja