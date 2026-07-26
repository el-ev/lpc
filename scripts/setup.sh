#!/usr/bin/env bash

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CMAKE_TOOLCHAIN_ARGS=()
LLVM_PREFIX=""

if [[ -n "${CXX:-}" ]]; then
    CMAKE_TOOLCHAIN_ARGS+=("-DCMAKE_CXX_COMPILER=$CXX")
elif [[ "$(uname -s)" == "Darwin" ]] && command -v brew >/dev/null 2>&1; then
    LLVM_PREFIX="$(brew --prefix llvm 2>/dev/null || true)"
    if [[ -x "$LLVM_PREFIX/bin/clang++" ]]; then
        CMAKE_TOOLCHAIN_ARGS+=("-DCMAKE_CXX_COMPILER=$LLVM_PREFIX/bin/clang++")
    fi
fi

if [[ -n "${LIBCXX_MODULES_JSON:-}" ]]; then
    CMAKE_TOOLCHAIN_ARGS+=("-DLIBCXX_MODULES_JSON=$LIBCXX_MODULES_JSON")
elif [[ -f "$LLVM_PREFIX/lib/c++/libc++.modules.json" ]]; then
    CMAKE_TOOLCHAIN_ARGS+=("-DLIBCXX_MODULES_JSON=$LLVM_PREFIX/lib/c++/libc++.modules.json")
fi

cmake --fresh -S "$PROJECT_ROOT" -B "$PROJECT_ROOT/build/debug" \
    -DCMAKE_BUILD_TYPE=Debug -G Ninja "${CMAKE_TOOLCHAIN_ARGS[@]}"
cmake --fresh -S "$PROJECT_ROOT" -B "$PROJECT_ROOT/build/release" \
    -DCMAKE_BUILD_TYPE=Release -G Ninja "${CMAKE_TOOLCHAIN_ARGS[@]}"

cp "$PROJECT_ROOT/build/debug/compile_commands.json" "$PROJECT_ROOT"
