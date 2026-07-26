{ pkgs ? import <nixpkgs> { } }:

let
  llvm = pkgs.llvmPackages_latest;
in
(pkgs.mkShell.override { stdenv = llvm.libcxxStdenv; }) {
  packages = [
    pkgs.cmake
    pkgs.ninja
    pkgs.python3
    llvm.clang-tools
  ];

  shellHook = ''
    export CC=clang
    export CXX=clang++

    # The Nix clang driver cannot locate the libc++ std-module manifest on its
    # own (split outputs), so point CMake at it explicitly.
    export LIBCXX_MODULES_JSON="${llvm.libcxx}/lib/libc++.modules.json"
  '';
}
